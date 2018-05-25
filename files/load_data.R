#' ---
#' title: 'load_data'
#' author: 'sachserf'
#' date: '`r Sys.Date()`'
#' output:
#'  html_document:
#'    theme: journal
#'    highlight: tango
#'    df_print: kable
#'    fig_caption: yes
#'    number_sections: yes
#'    collapsed: yes
#'    code_folding: hide
#'    toc: yes
#'    toc_float: yes
#'    toc_depth: 5
#'    keep_md: yes
#' ---

#+ setup_load_data, include = FALSE
# first 'dev'-value should be suitable for output format specified in YAML metadata (html+word = raster graphics, pdf = vector graphics)
knitr::opts_chunk$set(dev = 'png', dpi = 100, error = TRUE, warning = TRUE, message = TRUE, echo = TRUE)
unlink('load_data_warnings.Rout')
# knit_hook_stderr will redirect warnings and messages to stderr()
knit_hook_stderr <- function(x, options) {
  writeLines(text = paste0(options$label, ':\n', x), con = stderr())
  paste0('```\n', x, '\n```')
  cat(paste0(options$label, ':\n', x, '\n'), file = 'load_data_warnings.Rout', append = TRUE)
}
knitr::knit_hooks$set(message = function(x, options) knit_hook_stderr(x, options), warning = function(x, options) knit_hook_stderr(x, options), error = function(x, options) stop(x))

#' # Note
#' If you are using ggplot2 you will need to explicitly call 'print()' on each ggplot2 object in order to write the files (without spinning this script).
