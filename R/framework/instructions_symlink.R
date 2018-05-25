#' Reorganize your output!
#'
#' @description This function will create symbolic links of your input files, as well as resulting figures and rendered documents in predefined target directories.
#' @inheritParams instructions_check
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return updated rds-file (df_source_files.rds) within the cache directory.
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_implement}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_check}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_symlink <- function(ls_instructions, df_source_files) {
  ########## REQUISITES
  #   if (file.exists(file.path(cache_dir, "ls_instructions.rds"))) {
  #   ls_instructions <-
  #     readRDS(file = file.path(cache_dir, "ls_instructions.rds"))
     input_dir <- ls_instructions$input_dir
     symlink_dir_figure <- ls_instructions$symlink_dir_figure
     symlink_dir_docs <- ls_instructions$symlink_dir_docs
     symlink_dir_input <- ls_instructions$symlink_dir_input
     rename_symlink_docs <- ls_instructions$rename_symlink_docs
     rename_symlink_figure <- ls_instructions$rename_symlink_figure
     rename_symlink_input <- ls_instructions$rename_symlink_input
     toplvl <- ls_instructions$toplvl
  # } else {
  #   stop(
  #     "Did not find file: '",
  #     file.path(cache_dir, "ls_instructions.rds"),
  #     "'. Run 'framework::prepare_instructions()', 'framework::implement_instructions()', 'check_instructions()' and 'execute_instructions()' and retry."
  #   )
  # }
  #
  # if (file.exists(file.path(cache_dir, "df_source_files.rds"))) {
  #   df_source_files <-
  #     readRDS(file = file.path(cache_dir, "df_source_files.rds"))
  list2env(ls_instructions, envir = environment())

  input_files <- df_source_files$input_files
    base_rename <- df_source_files$base_rename
    base_rename_noext <- df_source_files$base_rename_noext
#    filename_noxt <- df_source_files$filename_noxt
#    row_names_char <- df_source_files$row_names_char
    figure_source <- df_source_files$figure_source
#    file_ext <- df_source_files$file_ext
  # } else {
  #   stop(
  #     "Did not find file: '",
  #     file.path(cache_dir, "df_source_files.rds"),
  #     "'. Run 'framework::prepare_instructions()', 'framework::implement_instructions()', 'check_instructions()' and 'execute_instructions()' and retry."
  #   )
  # }

    # delete deprecated directories:
    rm_symlink_dir(symlink_dir_figure)
    rm_symlink_dir(symlink_dir_docs)
    rm_symlink_dir(symlink_dir_input)
    if (exists("ls_instructions_last", where = "framework_params")) {
      rm_symlink_dir(get("ls_instructions_last", pos = "framework_params")$symlink_dir_figure)
      rm_symlink_dir(get("ls_instructions_last", pos = "framework_params")$symlink_dir_docs)
      rm_symlink_dir(get("ls_instructions_last", pos = "framework_params")$symlink_dir_input)
    }

  ######## input_files
  if (!is.null(symlink_dir_input)) {
    if (rename_symlink_input == TRUE) {
      filename_docs_out <- file.path(symlink_dir_input, base_rename)
    } else {
      filename_docs_out <-
        paste0(symlink_dir_input, gsub(
          file.path(toplvl, input_dir),
          "",
          normalizePath(input_files)
        ))
    }

  #################

    sapply(unique(dirname(filename_docs_out)),
           dir.create,
           recursive = TRUE,
           showWarnings = FALSE)

    # original
    for (i in seq_along(filename_docs_out)) {
      file.symlink(from = normalizePath(input_files)[i], to = filename_docs_out[i])
    }
  }

  ######## DOCUMENTS
  if (!is.null(symlink_dir_docs)) {
    if (rename_symlink_docs == TRUE) {
      filename_docs_out <- file.path(symlink_dir_docs, base_rename[df_source_files$instruction_no_cache != "source"])
    } else {
      filename_docs_out <-
        paste0(symlink_dir_docs, gsub(
          file.path(toplvl, input_dir),
          "",
          normalizePath(input_files[df_source_files$instruction_no_cache != "source"])
        ))

    }

  #################

        sapply(unique(dirname(filename_docs_out)),
           dir.create,
           recursive = TRUE,
           showWarnings = FALSE)

    # PDF
    pdf_out <-
      paste0(tools::file_path_sans_ext(filename_docs_out), ".pdf")
    pdf_in <- paste0(tools::file_path_sans_ext(input_files[df_source_files$instruction_no_cache != "source"]), ".pdf")
    pdf_index <- file.exists(pdf_in)
    pdf_in <- pdf_in[pdf_index]
    pdf_out <- pdf_out[pdf_index]

    for (i in seq_along(pdf_in)) {
      file.symlink(from = normalizePath(pdf_in)[i], to = pdf_out[i])
    }

    # docx
    docx_out <-
      paste0(tools::file_path_sans_ext(filename_docs_out), ".docx")
    docx_in <- paste0(tools::file_path_sans_ext(input_files[df_source_files$instruction_no_cache != "source"]), ".docx")
    docx_index <- file.exists(docx_in)
    docx_in <- docx_in[docx_index]
    docx_out <- docx_out[docx_index]

    for (i in seq_along(docx_in)) {
      file.symlink(from = normalizePath(docx_in)[i], to = docx_out[i])
    }

    # html
    html_out <-
      paste0(tools::file_path_sans_ext(filename_docs_out), ".html")
    html_in <- paste0(tools::file_path_sans_ext(input_files[df_source_files$instruction_no_cache != "source"]), ".html")
    html_index <- file.exists(html_in)
    html_in <- html_in[html_index]
    html_out <- html_out[html_index]

    for (i in seq_along(html_in)) {
      file.symlink(from = normalizePath(html_in)[i], to = html_out[i])
    }
  }

  #### FIGURES
  if (!is.null(symlink_dir_figure)) {
    if (rename_symlink_figure == TRUE) {
      figlist <-
        lapply(figure_source,
               list.files,
               recursive = TRUE,
               full.names = TRUE)

      figr_out <- list()
      for (i in seq_along(figlist)) {
        if (length(figlist[[i]]) > 0) {
        figr_out[[i]] <- file.path(symlink_dir_figure, paste0(base_rename_noext[i], "__", basename(figlist[[i]])))
        }
      }

      figr_out <- unlist(figr_out)

#      figr_out <-
#        file.path(symlink_dir_figure,
#                  paste0(base_rename_noext, "__", basename(unlist(figlist))))
    } else {
      figlist <-
        lapply(figure_source,
               list.files,
               recursive = TRUE,
               full.names = TRUE)
      figr_out <-
        paste0(symlink_dir_figure, gsub(
          file.path(toplvl, input_dir),
          "",
          normalizePath(unlist(figlist))
        ))
    }
#############################

    if (length(figr_out) > 0) {
      sapply(unique(dirname(figr_out)),
             dir.create,
             recursive = TRUE,
             showWarnings = FALSE)
      
      figr_in <-
        unlist(lapply(
          figure_source,
          list.files,
          recursive = TRUE,
          full.names = TRUE
        ))
      
      for (i in seq_along(figr_in)) {
        file.symlink(from = normalizePath(figr_in)[i], to = figr_out[i])
      }
    }
  }
}
