#' Implement specified instructions
#'
#' @description This function creates a data frame from a list of prepared instructions.
#' @inheritParams instructions_prepare
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return rds-file (df_source_files.rds) within the cache directory.
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_check}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_implement <-
  function(ls_instructions)
  {
 #   list2env(ls_instructions, envir = environment())
    input_files <- ls_instructions$input_files
    cache_dir <- ls_instructions$cache_dir
    toplvl <- ls_instructions$toplvl
    input_dir <- ls_instructions$input_dir
    spin_index <- ls_instructions$spin_index
    cache_index <- ls_instructions$cache_index
    
    ##################### SOURCE #####################
    # prepare df_cache
    # specify rownames
    row_names <- seq_along(input_files)
    row_names_char <- paste0(strrep("0", (max(nchar(row_names)) - nchar(row_names))), row_names)

    # specify basename without extension
    basename_noxt <-
      basename(tools::file_path_sans_ext(input_files))
    # specify input_files without extension
    filename_noxt <- tools::file_path_sans_ext(input_files)
    # specify file_ext
    file_ext <- tools::file_ext(input_files)
    file_ext_low <- tolower(file_ext)
    figure_source <- paste0(filename_noxt, "_files")

    if (any(file_ext_low == "rnw")) {
      figure_source[which(file_ext_low == "rnw")] <- file.path(dirname(figure_source[which(file_ext_low == "rnw")]), "figure")
    }

    # check support of source files
    if (any(file_ext_low %in% c("r", "rmd", "rnw", "md")) == FALSE) {
      stop(
        "At least one source file is not supported. Supported file formats are R, Rmd, md and Rnw."
      )
    }

    base_rename <- paste0(row_names_char, gsub(.Platform$file.sep, "__", gsub(file.path(toplvl, input_dir), "", normalizePath(input_files))))

    base_rename_noext <- list()
    for (i in seq_along(base_rename)) {
      base_rename_noext[[i]] <- gsub(paste0(".", file_ext[i]), "", base_rename[i])
    }
    base_rename_noext <- unlist(base_rename_noext)

    ##################### CACHE #####################

    # filepath in cache
    targetdir_cache <- file.path(cache_dir, filename_noxt)

    # specify path to image of the file
    image_cache <- paste0(file.path(targetdir_cache, basename(filename_noxt)), ".RData")

    # not implemented yet:
    warnings_cache <- gsub(".RData", "_warnings.txt", image_cache)

    # arrange df_source_files
    df_source_files <-
      data.frame(
        row_names,
        row_names_char,
        input_files,
        basename_noxt,
        filename_noxt,
        file_ext,
        file_ext_low,
        figure_source,
        #        figure_out,
        #        docs_out,
        #        temp_docs_out,
        #        log,
        targetdir_cache,
        image_cache,
        warnings_cache,
        base_rename,
        base_rename_noext,
        stringsAsFactors = FALSE
      )

    # specify instruction depending on file extension
    df_source_files$instruction_ext <- NA
    df_source_files$instruction_ext[which(df_source_files$file_ext_low ==
                                            "r")] <- "source"
    df_source_files$instruction_ext[which(df_source_files$file_ext_low ==
                                            "rmd")] <- "render"
    df_source_files$instruction_ext[which(df_source_files$file_ext_low ==
                                            "md")] <- "render"
    df_source_files$instruction_ext[which(df_source_files$file_ext_low ==
                                            "rnw")] <- "knit"

    # specify which R-files to spin
    df_source_files$use_spin <- FALSE
    df_source_files$use_spin[spin_index] <- TRUE
    if (any(df_source_files$file_ext != "R")) {
      df_source_files[which(df_source_files$file_ext != "R"),
                      "use_spin"] <- NA
    }

    # specify instructions as defined in input_files
    df_source_files$instruction_no_cache <-
      df_source_files$instruction_ext
    if (length(which(df_source_files$use_spin == TRUE)) > 0) {
      df_source_files[which(df_source_files$use_spin == TRUE),
                      "instruction_no_cache"] <- "render"
    }

    # specify cache-files according to input_files
    df_source_files$use_cache_input <- FALSE
    df_source_files$use_cache_input[cache_index] <- TRUE

    # Make sure that there is no outdated df_source_files_temp.rds
    if (file.exists(file.path(cache_dir, "df_source_files.rds"))) {
      unlink(x = file.path(cache_dir, "df_source_files.rds"),
             recursive = TRUE)
    }

    assign(x = "df_source_files", value = df_source_files, pos = "framework_params")
    # write temporary df_source_files
    saveRDS(object = df_source_files,
            file = file.path(cache_dir,
                             "df_source_files.rds"))
    return(df_source_files)
  }






