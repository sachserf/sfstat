#' Prepare Instructions
#'
#' @description This function processes the given ls_instructions.
#' @param ls_instructions List. A list containing all parameters of a framework project. See ?framework::template_params for a complete list of params needed.
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return rds-file (ls_instructions.rds) within the cache directory.
#' @seealso \code{\link{instructions_implement}}, \code{\link{instructions_check}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_prepare <-
  function(ls_instructions)
  {
#    list2env(ls_instructions, envir = environment())

    input_files <- ls_instructions$input_files
    pkg_cran_install <- ls_instructions$pkg_cran_install
    pkg_cran_load <- ls_instructions$pkg_cran_load
    pkg_gh_install <- ls_instructions$pkg_gh_install
    pkg_gh_load <- ls_instructions$pkg_gh_load
    input_dir <- ls_instructions$input_dir
    data_dir <- ls_instructions$data_dir
    cache_dir <- ls_instructions$cache_dir
    fun_dir <- ls_instructions$fun_dir
    spin_index <- ls_instructions$spin_index
    cache_index <- ls_instructions$cache_index
    symlink_dir_input <- ls_instructions$symlink_dir_input
    symlink_dir_docs <- ls_instructions$symlink_dir_docs
    symlink_dir_figure <- ls_instructions$symlink_dir_figure
    rename_symlink_input <- ls_instructions$rename_symlink_input
    rename_symlink_docs <- ls_instructions$rename_symlink_docs
    rename_symlink_figure <- ls_instructions$rename_symlink_figure
    rebuild_figures <- ls_instructions$rebuild_figures
    Rplots_device <- ls_instructions$Rplots_device
    target_dir_data <- ls_instructions$target_dir_data
    listofdf <- ls_instructions$listofdf
    data_extension <- ls_instructions$data_extension
    rebuild_target_dir_data <- ls_instructions$rebuild_target_dir_data
    filepath_session_info <- ls_instructions$filepath_session_info
    filepath_log <- ls_instructions$filepath_log
    filepath_tree <- ls_instructions$filepath_tree
    filepath_warnings <- ls_instructions$filepath_warnings
    tree_directory <- ls_instructions$tree_directory
    include_hidden_tree <- ls_instructions$include_hidden_tree
    filepath_image <- ls_instructions$filepath_image
    filepath_pkg_bib <- ls_instructions$filepath_pkg_bib
    quiet_processing <- ls_instructions$quiet_processing
    summarize_session_info <- ls_instructions$summarize_session_info
    summarize_df <- ls_instructions$summarize_df
    summarize_memory <- ls_instructions$summarize_memory
    summarize_log <- ls_instructions$summarize_log
    summarize_git <- ls_instructions$summarize_git
    summarize_tree <- ls_instructions$summarize_tree
    summarize_warnings <- ls_instructions$summarize_warnings

    if (!dir.exists(cache_dir))
      dir.create(cache_dir, recursive = TRUE)

    # Make sure that there is no outdated ls_instructions.rds
    unlink(x = file.path(cache_dir, "ls_instructions.rds"),
           recursive = TRUE)
    unlink(x = file.path(cache_dir, "df_source_files.rds"),
           recursive = TRUE)

    # call file_path for input_files
    input_files <- sapply(input_files, file.path, USE.NAMES = FALSE)
    cache_dir <- file.path(cache_dir)
    input_dir <- file.path(input_dir)
    toplvl <- file.path(toplvl)

    optional_paths <-
      list(data_dir,
           symlink_dir_figure,
           symlink_dir_docs,
           symlink_dir_input)
    names_opt_paths <-
      as.character(substitute(
        list(
          data_dir,
          symlink_dir_figure,
          symlink_dir_docs,
          symlink_dir_input
        )
      ))
    names_opt_paths <- names_opt_paths[2:length(names_opt_paths)]
    names(optional_paths) <- names_opt_paths
    valid_paths <-
      optional_paths[!sapply(optional_paths, is.null, USE.NAMES = FALSE)]
    valid_paths <- lapply(valid_paths, file.path)
    list2env(valid_paths, environment())

    # specify file paths (add input_dir if necessary)
    path_included <- grepl(pattern = input_dir, x = input_files)
    input_files <-
      ifelse(path_included == TRUE,
             file.path(input_files),
             file.path(input_dir, input_files))
    # check if input_files exists
    if (any(file.exists(input_files) == FALSE) == TRUE) {
      stop(
        "At least one source_file does not exist. Check file path and retry. File paths should be relative to the input_dir."
      )
    }

        # vectorize indices
    # unlink cache-dir
    if (is.null(cache_index)) {
      unlink(x = cache_dir, recursive = TRUE)
    } else if (length(cache_index) == 1 && cache_index == 999) {
      cache_index <- 1:length(input_files)
    }
    if (length(spin_index) == 1 && spin_index == 999) {
      spin_index <- 1:length(input_files)
    }

    ls_instructions <- list(
      input_files,
      pkg_cran_install,
      pkg_cran_load,
      pkg_gh_install,
      pkg_gh_load,
      spin_index,
      cache_index,
      cache_dir,
      input_dir,
      fun_dir,
      data_dir,
      symlink_dir_figure,
      symlink_dir_docs,
      rename_symlink_figure,
      rename_symlink_docs,
      Rplots_device,
      target_dir_data,
      listofdf,
      data_extension,
      rebuild_target_dir_data,
      tree_directory,
      filepath_tree,
      include_hidden_tree,
      filepath_image,
      filepath_pkg_bib,
      filepath_log,
      filepath_session_info,
      filepath_warnings,
      toplvl,
      rename_symlink_input,
      symlink_dir_input,
      rebuild_figures,
      quiet_processing,
      summarize_session_info,
      summarize_df,
      summarize_memory,
      summarize_log,
      summarize_git,
      summarize_tree,
      summarize_warnings
    )
    names_ls_instructions <- list(
      "input_files",
      "pkg_cran_install",
      "pkg_cran_load",
      "pkg_gh_install",
      "pkg_gh_load",
      "spin_index",
      "cache_index",
      "cache_dir",
      "input_dir",
      "fun_dir",
      "data_dir",
      "symlink_dir_figure",
      "symlink_dir_docs",
      "rename_symlink_figure",
      "rename_symlink_docs",
      "Rplots_device",
      "target_dir_data",
      "listofdf",
      "data_extension",
      "rebuild_target_dir_data",
      "tree_directory",
      "filepath_tree",
      "include_hidden_tree",
      "filepath_image",
      "filepath_pkg_bib",
      "filepath_log",
      "filepath_session_info",
      "filepath_warnings",
      "toplvl",
      "rename_symlink_input",
      "symlink_dir_input",
      "rebuild_figures",
      "quiet_processing",
      "summarize_session_info",
      "summarize_df",
      "summarize_memory",
      "summarize_log",
      "summarize_git",
      "summarize_tree",
      "summarize_warnings"
    )
    names(ls_instructions) <- names_ls_instructions

    ls_instructions$path_snapshot_source_dir <-
      file.path(cache_dir, "snapshot_source_dir.rds")
    ls_instructions$path_snapshot_data_dir <-
      file.path(cache_dir, "snapshot_data_dir.rds")

    # check changed directories
    if (file.exists(file.path(cache_dir, "ls_instructions_last.rds"))) {
      ls_instructions_last <-
        readRDS(file.path(cache_dir, "ls_instructions_last.rds"))
      check_paths <-
        c(
          "cache_dir",
          "input_dir",
          "fun_dir",
          "data_dir",
          "symlink_dir_figure",
          "symlink_dir_docs",
          "target_dir_data",
          "symlink_dir_input",
          "tree_target",
          "log_filepath",
          "filepath_session_info",
          "filepath_image",
          "filepath_pkg_bib",
          "filepath_log",
          "filepath_tree",
          "filepath_warnings"
        )
      changed_paths <- ls_instructions_last[check_paths]
      changed_paths_old <-
        changed_paths[!ls_instructions_last[check_paths] %in% ls_instructions[check_paths]]

      if (length(changed_paths_old) > 0) {
        changed_paths_new <-
          ls_instructions[names(ls_instructions) %in% names(changed_paths_old)]

        if (length(changed_paths) != 0) {
          warning(
            "The following paths changed since last run: \n",
            paste(
              names(changed_paths_old),
              paste0(
                ": from '",
                changed_paths_old,
                "' to '",
                changed_paths_new,
                "'"
              ),
              collape = "\n",
              sep = ""
            ),
            "\nYou probably should delete deprecated directories."
          )
        }
      }
    }

    unlink(file.path(cache_dir, "ls_instructions.rds"), recursive = TRUE)
    assign(x = "ls_instructions", value = ls_instructions, pos = "framework_params")
    saveRDS(ls_instructions, file = file.path(cache_dir, "ls_instructions.rds"))

    return(ls_instructions)
  }
