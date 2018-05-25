#' NOTE: This file is dependent on parameters specified in external files (default: params.R)
#' FRESH START
closeAllConnections()
grDevices::graphics.off()
cat('\014')
options(warn = 1)
if ('package:framework' %in% search() == TRUE) detach(package:framework)
if ('framework_params' %in% search() == TRUE) detach(framework_params)
if ('framework_fun' %in% search() == TRUE) detach(framework_fun)
rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
#' MESSAGE
message(R.version.string, ' <<', version$nickname, '>> \n', Sys.info()['effective_user'], ' @ ', version$platform, '.\n')
message('#############################################\n################ PROCESSING #################\n#############################################\n')
#' ENVIR
framework_params <- new.env(parent = .GlobalEnv)
framework_params$filepath_instructions <- c('params.R')
sapply(framework_params$filepath_instructions, source, local = framework_params)
if (framework_params$toplvl != basename(getwd())) stop('Check Working directory!')
framework_params$toplvl <- getwd()
#' FUN
framework_fun <- new.env(parent = .GlobalEnv)
sapply(
  list.files(
    path = framework_params$fun_dir,
    pattern = '*.R$',
    full.names = TRUE,
    recursive = TRUE
  ),
  source,
  local = framework_fun
)
attach(framework_fun)
rm(framework_fun)
#' ESCAPE
if (file.exists('framework_escape_hook.R')) {
  source('framework_escape_hook.R')
  framework_escape_hook()
  rm('framework_escape_hook')
  unlink('framework_escape_hook.R')
}
#' PARAMS
assign_missing_params(pos = framework_params)
framework_params$ls_instructions <- lapply(ls(envir = framework_params), get, envir = framework_params)
names(framework_params$ls_instructions) <- as.list(ls(envir = framework_params))[-which(ls(envir = framework_params) == 'ls_instructions')]
attach(framework_params)
rm(framework_params)
#' PKG
pkg_cran(pkg_names = pkg_cran_install, attach = FALSE)
pkg_gh(pkg_names = pkg_gh_install, attach = FALSE)
pkg_cran(pkg_names = pkg_cran_load)
pkg_gh(pkg_names = pkg_gh_load)
#' INSTRUCTIONS
instructions_prepare(ls_instructions = get('ls_instructions', pos = 'framework_params'))
instructions_implement(ls_instructions = get('ls_instructions', pos = 'framework_params'))
instructions_check(ls_instructions = get('ls_instructions', pos = 'framework_params'), df_source_files = get('df_source_files', pos = 'framework_params'))
instructions_execute(ls_instructions = get('ls_instructions', pos = 'framework_params'), df_source_files = get('df_source_files', pos = 'framework_params'))
instructions_symlink(ls_instructions = get('ls_instructions', pos = 'framework_params'), df_source_files = get('df_source_files', pos = 'framework_params'))
instructions_supplement(ls_instructions = get('ls_instructions', pos = 'framework_params'))
suppressWarnings(rm(knit_hook_stderr))
autosnapshot(repopath = get('ls_instructions', pos = 'framework_params')$toplvl, autobranch = get('ls_instructions', pos = 'framework_params')$autobranch)

