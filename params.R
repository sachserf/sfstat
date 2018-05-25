#' # Specify parameters for make.R
#' # INPUT (required)
toplvl = 'sfstat'
input_files = c('load_data.R')
pkg_cran_install = c('utils','tools','rmarkdown','knitr','rstudioapi')
pkg_cran_load = c('tidyverse')
pkg_gh_install = NULL
pkg_gh_load = NULL
if (system('git --version') == 0) {
  pkg_cran_load = c(pkg_cran_load, 'git2r')
}
input_dir = 'files'
data_dir = 'inst/extdata'
cache_dir = '.cache'
fun_dir = 'R'
spin_index = 0
cache_index = 999
#' -------------------
#' # OUTPUT (optional)
symlink_dir_input = NULL
symlink_dir_docs = 'out/docs'
symlink_dir_figure = 'out'
rename_symlink_input = TRUE
rename_symlink_docs = TRUE
rename_symlink_figure = TRUE
rebuild_figures = TRUE
Rplots_device = grDevices::png

target_dir_data = 'data'
listofdf = 'GlobalEnv'
data_extension = 'RData'
rebuild_target_dir_data = TRUE
filepath_session_info = 'meta/session_info.txt'
filepath_log = 'meta/log.csv'
filepath_tree = 'meta/tree.txt'
filepath_warnings = 'meta/warnings.Rout'
tree_directory = getwd()
include_hidden_tree = FALSE
filepath_pkg_bib = 'meta/pkg.bib'
filepath_image = '.RData'
autobranch = NULL

quiet_processing = TRUE
summarize_session_info = FALSE
summarize_df = FALSE
summarize_memory = FALSE
summarize_log = FALSE
summarize_git = TRUE
summarize_tree = FALSE
summarize_warnings = FALSE
