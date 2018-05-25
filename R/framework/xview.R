#' External View
#'
#' @description This function will export an object to a file (using package "rio"). A system command is invoked to open the file afterwards.
#'
#' @param x Object to export (e.g. a data.frame)
#' @param file Specify filename. Directories will not be created.
#' @param ... Further params passed to rio::export (e.g. overwrite, row.names, etc.)
#' 
#' @seealso \code{\link[rio]{export}}
#' 
#' @export
xview <- function(x, file = "xview.xlsx", ...) {
  if (Sys.info()["sysname"] == "Linux") {
    cmd <- "xdg-open"
  } else if (Sys.info()["sysname"] == "Windows") {
    cmd <- "start"
  } else if (Sys.info()["sysname"] == "Darwin") {
    cmd <- "open"
  } else if (Sys.info()["sysname"] %in% c("Linux", "Darwin", "Windows") == FALSE) {
    stop("Operating system not supported.")
  }
  message("Exporting file to: ", file, " ...")
  rio::export(x = x, file = file, ...)
    system(paste(cmd, file))
}
