#' Unlink directory containing symlinks only
#'
#' @description This function will check whether there are only symbolic links within a directory and if so delete the directory.
#' @param directory Character. Specify directory that should be deleted.
#'
#' @export
rm_symlink_dir <- function(directory) {
  if (!is.null(directory)) {
    if (any(Sys.readlink(file.path(
      directory,
      list.files(
        directory,
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = FALSE
      )
    )) == "")) {
      stop(
        "Other files than symbolic links in ",
        directory,
        ". This directory is reserved for automatic creation of symbolic links and should be treated as read-only."
      )
    } else {
      unlink(directory, recursive = TRUE)
    }
  }
}
