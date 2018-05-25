#' Create a template script-file
#'
#' @description This function will create an R or Rmd-file (depending on the
#'   file extension of the input) including some predefined lines. The function
#'   is designed to save some time writing the same input again and again (e.g.
#'   Author, date and other things). By using this template all warnings and
#'   messages will be written to a file ('filename_warnings.Rout') and errors
#'   will interupt rendering.
#' @param file Character. Specify the path to save the new file. Use relative
#'   file paths and specify the file extension; e.g. 'filesnew_dir/myfile.Rmd'.
#'   or 'in/src/new_dir/myfile.R'. If the extension is neither .R nor .Rmd (e.g.
#'   no extension) an .R-extension will be added by default.
#' @param Author Character. Optionally customize the name of the Author (used
#'   for the YAML header). Default is the effective user of the system info.
#' @param Date Character. Optionally customize the date (used for the YAML
#'   header). Default is the current Date (format YYYY-MM-DD).
#' @param open Logical. If TRUE the file will be opened (via `file.edit``).
#' @param doctype Character. Specify file extension for different output formats
#'   of Rmd-files (pdf, html, docx).
#' @param knitr_sidecar Logical. Write sidecar file for knitr-setup?
#' @note Missing directories will be created recursively.
#' @note It is not possible to overwrite existing files.
#' @note Other YAML header options will be choosen automatically. Edit the
#'   resulting file to customize the YAML header.
#' @note R files will be ready to be spinned via knitr. Therefore the script
#'   contains some preceding roxygen comments.
#' @author Frederik Sachser
#'
#' @export
template_rmd <-
  function(file,
           Author = Sys.info()["effective_user"],
           Date = "`r Sys.Date()`",
           open = TRUE,
           doctype = "html",
           knitr_sidecar = FALSE)
  {
    if (dir.exists(paths = dirname(file)) == FALSE) {
      dir.create(path = dirname(file), recursive = TRUE)
    }
    
    # check file extension
    if (tolower(substr(
      x = basename(file),
      start = nchar(basename(file)) - 3,
      stop = nchar(basename(file))
    )) == ".rmd") {
      fileext <- ".Rmd"
      header <-
        substr(x = basename(file),
               start = 1,
               stop = nchar(basename(file)) - 4)
      # file <- paste0(header, ".Rmd")
    } else if (toupper(substr(
      x = basename(file),
      start = nchar(basename(file)) - 1,
      stop = nchar(basename(file))
    )) == ".R") {
      fileext = ".R"
      header <-
        substr(x = basename(file),
               start = 1,
               stop = nchar(basename(file)) - 2)
      #  file <- paste0(header, ".R")
    } else if (tools::file_path_sans_ext(file) == file.path(file)) {
      fileext <- ".Rmd"
      header <- basename(file)
      file <- paste0(file, fileext)
      print("File extension is neither .Rmd nor .R: using .Rmd per default")
    } else {
      file.edit(file)
      return(message("Unknown file extension. Using file.edit()"))
    }
    
    yaml_r_docx <- paste0("#' ---\n#' title: '", header,"'\n#' author: '", Author, "'\n#' date: '", Date, "'\n#' output:\n#'  word_document: default\n#' ---")
    
    yaml_r_html <- paste0("#' ---\n#' title: '", header,"'\n#' author: '", Author, "'\n#' date: '", Date, "'\n#' output:\n#'  html_document:\n#'    theme: journal\n#'    highlight: tango\n#'    df_print: kable\n#'    fig_caption: yes\n#'    number_sections: yes\n#'    collapsed: yes\n#'    code_folding: hide\n#'    toc: yes\n#'    toc_float: yes\n#'    toc_depth: 5\n#'    keep_md: yes\n#' ---")
    
    yaml_r_pdf <- paste0("#' ---\n#' title: '", header,"'\n#' author: '", Author, "'\n#' date: '", Date, "'\n#' output:\n#'   pdf_document:\n#'     highlight: tango\n#'     df_print: kable\n#'     fig_caption: yes\n#'     number_sections: yes\n#'     toc: yes\n#'     toc_depth: 5\n#'     keep_tex: yes\n#'     latex_engine: xelatex\n#' mainfont: 'Arial'\n#' linkcolor: 'blue'\n#' urlcolor: 'blue'\n#' citecolor: 'blue'\n#' fontsize: 12pt\n#' geometry: margin=1in\n#' classoption: oneside\n#' toc: yes\n#' lot: yes\n#' lof: yes\n#' # bibliography: path2.bib\n#' link-citations: yes\n#' # csl: path2.csl # https://github.com/citation-style-language/styles\n#' ---")
    
    yaml_rmd_html <- gsub("#' ", "", yaml_r_html)
    yaml_rmd_docx <- gsub("#' ", "", yaml_r_docx)
    yaml_rmd_pdf <- gsub("#' ", "", yaml_r_pdf)
    
    knitr_chunk <- paste0("# first 'dev'-value should be suitable for output format specified in YAML metadata (html+word = raster graphics, pdf = vector graphics)\nknitr::opts_chunk$set(dev = 'png', dpi = 100, error = TRUE, warning = TRUE, message = TRUE, echo = TRUE)\nunlink('", paste0(header, "_warnings.Rout"), "')\n# knit_hook_stderr will redirect warnings and messages to stderr()\nknit_hook_stderr <- function(x, options) {\n  writeLines(text = paste0(options$label, ':\\n', x), con = stderr())\n  paste0('```\\n', x, '\\n```')\n  cat(paste0(options$label, ':\\n', x, '\\n'), file = '", paste0(header, "_warnings.Rout"), "', append = TRUE)\n}\nknitr::knit_hooks$set(message = function(x, options) knit_hook_stderr(x, options), warning = function(x, options) knit_hook_stderr(x, options), error = function(x, options) stop(x))")
    
    if (knitr_sidecar == TRUE) {
      knitr_file <- paste0(tools::file_path_sans_ext(file), "_knitr_setup.R")
      body_r <- paste0("source('", knitr_file,"')")
      writeLines(knitr_chunk, con = knitr_file)
    } else {
      body_r <- knitr_chunk
    }
    
    setup_r <- paste0("\n#+ setup_", header, ", include = FALSE")
    setup_rmd_beginn <- paste0("\n```{r setup_", header, ", include=FALSE}")
    setup_rmd_end <- paste0("```")
    
    note_r <- paste0("\n#' # Note\n#' If you are using ggplot2 you will need to explicitly call 'print()' on each ggplot2 object in order to write the files (without spinning this script).")
    note_rmd <- paste0("")
    
    # check if file exists
    if (file.exists(file) == TRUE) {
      stop("File exists. Choose different file path.")
    } else if (fileext == ".R" && doctype == "html") {
      writeLines(c(yaml_r_html, setup_r, body_r, note_r), con = file)
    } else if (fileext == ".R" && doctype == "docx") {
      writeLines(c(yaml_r_docx, setup_r, body_r, note_r), con = file)
    } else if (fileext == ".R" && doctype == "pdf") {
      writeLines(c(yaml_r_pdf, setup_r, body_r, note_r), con = file)
    } else if (fileext == ".Rmd" && doctype == "html") {
      writeLines(c(
        yaml_rmd_html,
        setup_rmd_beginn,
        body_r,
        setup_rmd_end,
        note_rmd
      ),
      con = file)
    } else if (fileext == ".Rmd" && doctype == "docx") {
      writeLines(c(
        yaml_rmd_docx,
        setup_rmd_beginn,
        body_r,
        setup_rmd_end,
        note_rmd
      ),
      con = file)
    } else if (fileext == ".Rmd" && doctype == "pdf") {
      writeLines(c(
        yaml_rmd_pdf,
        setup_rmd_beginn,
        body_r,
        setup_rmd_end,
        note_rmd
      ),
      con = file)
    }
    
    if (open == TRUE) {
      file.edit(file)
    }
  }




