#' Round out your instructions!
#'
#' @description This function will write data frames, session info, log-file, etc and print summaries of these information to the console according to the parameters specified in the file 'params.R'.
#' @inheritParams instructions_prepare
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return updated rds-file (df_source_files.rds) within the cache directory.
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_implement}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_check}}
#' @author Frederik Sachser
#' @export
instructions_supplement <- function(ls_instructions) {
#    list2env(ls_instructions, envir = environment())
#   if (file.exists(file.path(cache_dir, "ls_instructions.rds"))) {
#   ls_instructions <-
#     readRDS(file = file.path(cache_dir, "ls_instructions.rds"))
   filepath_warnings <- ls_instructions$filepath_warnings
   summarize_warnings <- ls_instructions$summarize_warnings
   target_dir_data <- ls_instructions$target_dir_data
   listofdf <- ls_instructions$listofdf
   data_extension <- ls_instructions$data_extension
   rebuild_target_dir_data <- ls_instructions$rebuild_target_dir_data
   summarize_df <- ls_instructions$summarize_df
   filepath_session_info <- ls_instructions$filepath_session_info
   summarize_session_info <- ls_instructions$summarize_session_info
   filepath_log <- ls_instructions$filepath_log
   summarize_log <- ls_instructions$summarize_log
   filepath_tree <- ls_instructions$filepath_tree
   tree_directory <- ls_instructions$tree_directory
   include_hidden_tree <- ls_instructions$include_hidden_tree
   summarize_tree <- ls_instructions$summarize_tree
   summarize_memory <- ls_instructions$summarize_memory
   summarize_git <- ls_instructions$summarize_git
   toplvl <- ls_instructions$toplvl
   cache_dir <- ls_instructions$cache_dir
   filepath_image <- ls_instructions$filepath_image
   filepath_pkg_bib <- ls_instructions$filepath_pkg_bib

# } else {
#   stop(
#     "Did not find file: '",
#     file.path(cache_dir, "ls_instructions.rds"),
#     "'. Run 'framework::prepare_instructions()', 'framework::implement_instructions()', 'check_instructions()' and 'execute_instructions()' and retry."
#   )
# }


  message('\n#############################################\n################## SUMMARY ##################\n#############################################')

  # WARNINGS

  if (!is.null(filepath_warnings)) {
    write_warnings(filepath_warnings,
                   summarize_warnings)
  }

  # DATA FRAME
    if (!is.null(target_dir_data)) {
      write_dataframe(listofdf,
                      target_dir_data,
                      data_extension,
                      rebuild_target_dir_data,
                      summarize_df)
    }

  # IMAGE
  if (!is.null(filepath_image)) {
    save.image(file = filepath_image)
  }

  # PKG_BIB
  if (!is.null(filepath_pkg_bib)) {
    suppressWarnings(knitr::write_bib(file = filepath_pkg_bib))
  }

  # SESSION INFO
    if (!is.null(filepath_session_info)) {
      write_session_info(filepath_session_info, summarize_session_info)
    }

  # LOG
    if (!is.null(filepath_log)) {
      write_log(filepath_log, summarize_log)
    }

  # TREE
    if (!is.null(filepath_tree)) {
      write_tree(tree_directory,
              filepath_tree,
              include_hidden_tree,
              summarize_tree)
    }

  # MEMORY
    if (summarize_memory == TRUE) {
      summary_memory_usage()
    }

  # GIT
    if (summarize_git == TRUE) {
      summary_git(git_repo = toplvl)
    }


  # INSTRUCTIONS
  summary_instructions(cache_dir)

  message('\n#############################################\n###### FINISHED at ', Sys.time(), ' ######\n#############################################')
}

