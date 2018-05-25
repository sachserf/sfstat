#' Write a file with current session info
#'
#' @description The function writes the current timestamp and session info to a
#'   file.
#' @param filepath_session_info Character. Specify file path to write the session info file.
#' @param summarize_session_info Logical. Should session info be printed to console?
#' @note Directories will be created recursively.
#' @author Frederik Sachser
#' @export
write_session_info <-
  function(filepath_session_info = "meta/session_info.txt", summarize_session_info = FALSE)
  {
    cat("#### Sys.time ####\n", append = FALSE, file = filepath_session_info)
    sink(filepath_session_info, append = TRUE)
    print(Sys.time())
    sink()

    cat("\n\n#### Sys.info ####\n", append = TRUE, file = filepath_session_info)
    sink(filepath_session_info, append = TRUE)
    print(Sys.info()["user"])
    sink()

    cat("\n\n#### memory usage ####\n", append = TRUE, file = filepath_session_info)
    sink(filepath_session_info, append = TRUE)
    print(suppressMessages(summary_memory_usage()))
    sink()

    cat("\n\n#### sessionInfo ####\n", append = TRUE, file = filepath_session_info)
    sink(filepath_session_info, append = TRUE)
    print(utils::sessionInfo(package = NULL))
    sink()

    if (!is.null(summarize_session_info) && summarize_session_info == TRUE) {
      writeLines('\n--------------------------------------------')
      message('\nsession_info:\n')
      writeLines(text = readLines(filepath_session_info), con = stdout())
    }

  }
