#' Breakpoint for input_files
#'
#' @description This function will write a new function called framework_escape_hook. The make-like file checks whether this function exists or not and calls it next time you source the file 'make.R'. This feature becomes useful when you are reviewing or working on a specific part of your analysis and do not want to run subsequent files of your project.
#' @param up2thisfile Character or integer. Specify the file you want to work on. Next time you source the file 'make.R' all files prior to this file will be processed while subsequent files will be skipped. If input is an integer value it will be used as an index for the position of the current input_file. Otherwise file path should be relative to the top level of the project.
#' @note The hook will be removed after sourcing the file 'make.R' (breakpoint works only once). You can cancel a breakpoint by deleting the function framework_escape_hook.
#' @export
breakpoint <- function(up2thisfile) {
  
  if (class(up2thisfile) == "numeric") {
    writeLines(paste0("framework_escape_hook <-  function() {\n  if (", up2thisfile, " > 1) {\n      input_files <- framework_params$input_files[1:(", up2thisfile, " - 1)]\n    } else {\n      input_files <- framework_params$input_files[1]\n    }\n assign(x = 'input_files', value = input_files, envir = framework_params)\n}"), con = file.path("framework_escape_hook.R"))
  } else if (class(up2thisfile) == "character") {
    writeLines(paste0("framework_escape_hook <-  function() {\n if ('", up2thisfile, "' %in% framework_params$input_files) {\n    index <- grep('", up2thisfile, "', framework_params$input_files) - 1\n    if (index > 0) {\n      input_files <- framework_params$input_files[1:index]\n    } else {\n      input_files <- framework_params$input_files[1]\n    }\n  } else {\n    stop('Can not find file name in input_files. Check spelling.')\n  }\n  assign(x = 'input_files', value = input_files, envir = framework_params)\n}"), con = file.path("framework_escape_hook.R"))
  } else {
    stop("Input should be of type numeric or character.")
  }
}

