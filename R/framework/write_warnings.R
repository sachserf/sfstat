#' Append _warnings.Rout of current input files
#'
#' @description This function will put together all '_warnings.Rout'-files of current input files.
#' @param filepath_warnings Character. Specify file path to write the warnings file.
#' @param summarize_warnings Logical. Should filepath_warnings be printed to console?
#'
#' @seealso \code{\link{summary_warnings}}
#' @author Frederik Sachser
#' @note The function assumes you are working in a framework project and will otherwise fail.
#' @export
write_warnings <-
  function(filepath_warnings,
           summarize_warnings = FALSE) {
    if ("framework_params" %in% search()) {
      if (exists(x = "df_source_files", where = "framework_params")) {
        unlink(filepath_warnings)
        warmes_files <-
          paste0(
            get("df_source_files", pos = "framework_params")$filename_noxt,
            "_warnings.Rout"
          )

        warmes_files <- warmes_files[file.exists(warmes_files)]

        ls_warmes_files <- lapply(warmes_files, readLines)
        names(ls_warmes_files) <- warmes_files

        for (i in seq_along(ls_warmes_files)) {
          cat(
            names(ls_warmes_files[i]),
            append = TRUE,
            sep = "\n",
            file = filepath_warnings
          )
          cat(
            ls_warmes_files[[i]],
            append = TRUE,
            sep = "\n",
            file = filepath_warnings
          )
          cat(
            "#######################",
            file = filepath_warnings,
            append = TRUE,
            sep = "\n"
          )
        }

        if (summarize_warnings == TRUE) {
          message("\nwarnings:\n")
          #writeLines(readLines(filepath_warnings), con = stdout())
          summary_warnings(filepath_warnings)
        }
      }
    } else {
      message("Can not find current input_files. source 'make.R' and retry.")
    }

  }
