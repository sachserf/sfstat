#' write a log entry
#'
#' @description This function will add a log entry to the specified file.
#'
#' @param filepath_log Character. Specify file path to write the log file.
#' @param summarize_log Logical. Should a summary of the log file be printed to console? Depends on dplyr.
#'
#' @return A csv file containing current timestamp and nodename.
#' @note If the file does not exist it will be written.
#'
#' @seealso \code{\link{summary_log}}
#' @author Frederik Sachser
#' @export
write_log <- function(filepath_log = "meta/log.csv", summarize_log = TRUE) {
  if (dir.exists(dirname(file.path(filepath_log))) == FALSE) {
    dir.create(path = dirname(file.path(filepath_log)), recursive = TRUE)
  }

  POSIX <- Sys.time()
  DATE <- strftime(POSIX, format = "%F")
  WEEKDAY <- strftime(POSIX, format = "%A")
  TIME <- strftime(POSIX, format = "%T")
  NODENAME <- Sys.info()["nodename"]
  POSIX <- as.character(POSIX)

  df <- data.frame(POSIX,
                   NODENAME,
                   DATE,
                   WEEKDAY,
                   TIME,
                   row.names = NULL)
  if (file.exists(filepath_log)) {
    suppressWarnings(
      utils::write.table(
        x = df,
        file = filepath_log,
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE
      )
    )
  } else {
    utils::write.table(
      x = df,
      file = filepath_log,
      append = FALSE,
      col.names = TRUE,
      row.names = FALSE
    )
  }
  if (!is.null(summarize_log) && summarize_log == TRUE) {
    message('\nlog:\n')
    print(summary_log(filepath_log)$per_NODENAME)
  }
}
