#' Assign missing params
#'
#' @description The framework project needs some parameters specified in the file 'params.R'. For some parameters it is possible to comment out. This function will check those missing parameters and set their default (FALSE or NULL).
#' @param optional_objects_null Character vector. Specify optional parameters that should be NULL if missing.
#' @param optional_objects_false Character vector. Specify optional parameters that should be FALSE if missing.
#' @param pos Passed on to 'base::assign'.
#'
#' @note This function is not intended to be run by the user.
#' @seealso \code{\link[base]{assign}}
#' @export
assign_missing_params <- function(optional_objects_null = c(
  "symlink_dir_figure",
  "symlink_dir_docs",
  "symlink_dir_input",
  "target_dir_data",
  "listofdf",
  "data_extension",
  "tree_directory",
  "filepath_tree",
  "filepath_log",
  "filepath_session_info",
  "filepath_warnings",
  "Rplots_device",
  "filepath_image",
  "filepath_pkg_bib",
  "autobranch"
),
optional_objects_false = c(
  "rename_symlink_figure",
  "rename_symlink_docs",
  "rename_symlink_input",
  "rebuild_target_dir_data",
  "rebuild_figures",
  "include_hidden_tree",
  "quiet_processing",
  "summarize_session_info",
  "summarize_df",
  "summarize_memory",
  "summarize_log",
  "summarize_git",
  "summarize_tree",
  "summarize_warnings"
),
pos) {
  lapply(optional_objects_null[!optional_objects_null %in% ls(pos = pos)],
         assign,
         value = NULL,
         pos = pos)

  lapply(optional_objects_false[!optional_objects_false %in% ls(pos = pos)],
         assign,
         value = FALSE,
         pos = pos)
}


