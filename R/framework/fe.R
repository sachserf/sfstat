#' framework version of file.edit
#'
#' @description This function will open existing files or create a new one by calling 'framework::template_rmd'.
#' @param file Character or integer. Specify full file path to open or create a file. If the value is an integer and 'framework_params' is in your search path it will look up the index of the input_file (at the point in time of the last processing) to open it.
#' @param ... Arguments passed to 'utile::file_edit'.
#'
#' @seealso \code{\link[utils]{file.edit}}, \code{\link{template_rmd}}
#' @export
fe <- function(file, ...) {
  if (is.numeric(file) && "framework_params" %in% search()) {
    input_files <-
      get(x = "ls_instructions", pos = "framework_params")$input_files
    if (length(input_files) < file) {
      stop("Index is greater than the amount of files.")
    } else {
      file.edit(input_files[file], ...)
    }
  } else {
    if (file.exists(file)) {
      if (Sys.readlink(file) != "") {
        file.edit(Sys.readlink(file), ...)
      } else {
        file.edit(file, ...)
      }
    } else {
      template_rmd(file, ...)
    }
  }
}
