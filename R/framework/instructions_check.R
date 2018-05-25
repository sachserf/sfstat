#' Check specified instructions
#'
#' @description Most checks of the given parameters of a framework project are implemented in this function.
#' @inheritParams instructions_prepare
#' @param df_source_files data frame. Predefined data frame (output of the function 'instructions_implement').
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return updated rds-file (df_source_files.rds) within the cache directory.
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_implement}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_check <-
  function(ls_instructions, df_source_files)
  {
 #   list2env(ls_instructions, envir = environment())
    # if (file.exists(file.path(cache_dir, "ls_instructions.rds"))) {
     #  ls_instructions <- readRDS(file = file.path(cache_dir, "ls_instructions.rds"))
       input_files <- ls_instructions$input_files
     #  spin_index <- ls_instructions$spin_index
    #   cache_index <- ls_instructions$cache_index
       cache_dir <- ls_instructions$cache_dir
       input_dir <- ls_instructions$input_dir
       data_dir <- ls_instructions$data_dir
       path_snapshot_source_dir <- ls_instructions$path_snapshot_source_dir
       path_snapshot_data_dir <- ls_instructions$path_snapshot_data_dir
    # } else {
    #   stop("Did not find file: '", file.path(cache_dir, "ls_instructions.rds"), "'. Run 'framework::prepare_instructions()' and 'framework::implement_instructions()' and retry.")
    # }
    #
    # if (file.exists(file.path(cache_dir, "df_source_files.rds"))) {
    #   df_source_files <- readRDS(file = file.path(cache_dir, "df_source_files.rds"))
    # } else {
    #   stop("Did not find file: '", file.path(cache_dir, "df_source_files.rds"), "'. Run 'framework::prepare_instructions()' and 'framework::implement_instructions()' and retry.")
    # }

    df_source_files$use_cache_qualified <- TRUE


    # check snapshot of source-dir

    # if snapshot is missing: do not use cache
    if (file.exists(path_snapshot_source_dir) == FALSE) {
      df_source_files$use_cache_qualified <- FALSE
    } else {
      # check file changes
      # specify changed files
      snapshot_source_dir <-
        readRDS(file = path_snapshot_source_dir)
      snapshot_source_dir$path <- input_dir

      changed_files <- utils::changedFiles(before = snapshot_source_dir, md5sum = TRUE)$changed
      added_files <- utils::changedFiles(before = snapshot_source_dir, md5sum = TRUE)$added
      files_differ <- c(changed_files, added_files)
      files_differ <- file.path(input_dir, files_differ)
      # specify changed files within input_files
      changed_files_index <- input_files %in% files_differ
      # if there are files that did not change - use cache
      if (any(changed_files_index == TRUE)) {
        df_source_files$use_cache_qualified[which(changed_files_index ==
                                                    TRUE)] <- FALSE
      }
    }

    # check snapshot of data-dir
    # if snapshot is missing: do not use cache
    if (!is.null(data_dir)) {
      if (file.exists(path_snapshot_data_dir) == FALSE) {
        if (length(list.files(data_dir, all.files = TRUE, recursive = TRUE)) > 0) {
          df_source_files$use_cache_qualified <- FALSE
        }
      } else {
        if (length(list.files(data_dir, all.files = TRUE)) > 0) {
          # check file changes
          # specify changed files
          snapshot_data_dir <- readRDS(file = path_snapshot_data_dir)
          snapshot_data_dir$path <- data_dir
          unchanged_files <- utils::changedFiles(before = snapshot_data_dir, md5sum = TRUE)$unchanged
          if (length(unchanged_files) != length(list.files(data_dir, recursive = TRUE))) {
            df_source_files$use_cache_qualified <- FALSE
            message("Cache will be ignored because files in data_dir have changed.")
          }
        }
      }
    }

    # check image

    # make sure not to use the cache if image of the file is missing
    image_exists <- file.exists(df_source_files$image_cache)
    if (any(image_exists == FALSE)) {
      df_source_files$use_cache_qualified[which(image_exists ==
                                                  FALSE)] <- FALSE
    }

    # check order of source-files

    # check if order of df_cache has changed
    if (file.exists(file.path(cache_dir, "df_source_files_last.rds"))) {
      df_source_files_last <-
        readRDS(file.path(cache_dir, "df_source_files_last.rds"))
      df_source_files_both <-
        merge(
          x = df_source_files,
          y = df_source_files_last[,
                                  c("input_files", "row_names", "instruction_no_cache")],
          by = "input_files",
          all.x = TRUE
        )
      df_source_files_both <-
        df_source_files_both[order(df_source_files_both$row_names.x), ]
      df_source_files_both$pos_match <-
        as.numeric(df_source_files_both$row_names.x) -
        as.numeric(df_source_files_both$row_names.y)
      df_source_files$use_cache_qualified[which(df_source_files_both$pos_match !=
                                                  0)] <- FALSE
      df_source_files$order_change <- FALSE
      df_source_files$order_change[which(df_source_files_both$pos_match !=
                                           0)] <- TRUE
      df_source_files$use_cache_qualified[which(df_source_files_both$pos_match !=
                                                  0)] <- FALSE
      
      # always process additional files (not in df_source_files_last):
      df_source_files$use_cache_qualified[which(!df_source_files$input_files %in% df_source_files_last$input_files)] <- FALSE
      
      # REVIEW
      # check comment out
      # make sure not to use cache if instruction changed
      # df_source_files$instruction_equal <-
      #   ifelse(
      #     df_source_files_both$instruction_no_cache.x ==
      #       df_source_files_both$instruction_no_cache.y,
      #     TRUE,
      #     FALSE
      #   )
      # if (any(which(df_source_files$instruction_equal == FALSE) >
      #         0)) {
      #   df_source_files$use_cache_qualified[which(
      #     df_source_files$instruction_equal ==
      #       FALSE & df_source_files$instruction_no_cache !=
      #       "source")] <- FALSE
      # }
    }
    else {
      df_source_files$use_cache_qualified <- FALSE
    }

    # check existence of (rendered) files
      for (i in 1:nrow(df_source_files)) {
        files_source_dir <- list.files(input_dir, full.names = TRUE, recursive = TRUE)
        filename_dot <-
          paste0(df_source_files$filename_noxt[i], ".")
        source_docs <-
          files_source_dir[grep(pattern = filename_dot,
                                x = files_source_dir,
                                fixed = TRUE)]
        render_docs <-
          source_docs[-which(source_docs == df_source_files$input_files[i])]
        if (length(render_docs) == 0 & df_source_files$instruction_no_cache[i] != "source") {
          df_source_files$use_cache_qualified[i] <- FALSE
        }
      }

    # do not use cache if subsequent files are going to be processed
    if (length(which(df_source_files$use_cache_qualified == FALSE)) >
        0) {
      df_source_files$use_cache_qualified[min(which(df_source_files$use_cache_qualified ==
                                                      FALSE)):nrow(df_source_files)] <-
        FALSE
    }

    # add final instructions
    df_source_files$instruction <-
      df_source_files$instruction_no_cache

    # edit instruction according to use-cache
    if (any(df_source_files$use_cache_input == TRUE)) {
      # do not use cache if subsequent files are not in cache_index
      if (any(df_source_files$use_cache_input) == FALSE) {
        df_source_files$use_cache_qualified[min(which(df_source_files$use_cache_input ==
                                                        FALSE)):nrow(df_source_files)] <-
          FALSE
      }
      use_cache_index <- which(
        df_source_files$use_cache_input ==
          TRUE & df_source_files$use_cache_qualified == TRUE
      )
      df_source_files$instruction[use_cache_index] <- "nothing"
    }


    # delete deprecated files in cache
    #    deprecated_cache <-
    #      dirname(df_source_files$docs_cache[df_source_files$instruction !=
    #                                           "nothing"])
    #    if (length(deprecated_cache) > 0) {
    #      lapply(X = deprecated_cache,
    #             FUN = unlink,
    #             recursive = TRUE)
    #    }

    # make sure the most recent file in cache will be loaded
    if (any(df_source_files$instruction == "nothing")) {
      df_source_files$instruction[max(which(df_source_files$instruction ==
                                              "nothing"))] <- "load"
    }

    # make sure all subsequent files of rendered files will be rendered as well
    if (any(df_source_files$instruction == "render")) {
      df_source_files$instruction[min(which(df_source_files$instruction ==
                                              "render")):nrow(df_source_files)] <-
        df_source_files$instruction_no_cache[min(which(df_source_files$instruction ==
                                                         "render")):nrow(df_source_files)]
    }

    assign(x = "df_source_files", value = df_source_files, pos = "framework_params")
    # overwrite df_source_files.rds
    saveRDS(object = df_source_files,
            file = file.path(cache_dir,
                             "df_source_files.rds"))
      return(df_source_files)
#    assign("df_source_files", df_source_files, pos = sys.frame())
  }
