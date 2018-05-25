#' Execute your instructions!
#'
#' @description The function will loop through all input_files and call the function instructions_specify.
#' @inheritParams instructions_check
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @return rds-files within the cache directory (ls_instructions_last, df_source_files_last, snapshot_data_dir.rds, snapshot_source_dir.rds)
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_implement}},
#'   \code{\link{instructions_check}},
#'   \code{\link{instructions_specify}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_execute <-
  function(ls_instructions, df_source_files)
  {
 #   list2env(ls_instructions, envir = environment())


    # if (file.exists(file.path(cache_dir, "ls_instructions.rds"))) {
    #   ls_instructions <- readRDS(file = file.path(cache_dir, "ls_instructions.rds"))
       toplvl <- ls_instructions$toplvl
       input_dir <- ls_instructions$input_dir
       data_dir <- ls_instructions$data_dir
       path_snapshot_source_dir <- ls_instructions$path_snapshot_source_dir
       path_snapshot_data_dir <- ls_instructions$path_snapshot_data_dir
       rebuild_figures <- ls_instructions$rebuild_figures
       quiet_processing <- ls_instructions$quiet_processing
       cache_dir <- ls_instructions$cache_dir
       Rplots_device <- ls_instructions$Rplots_device
       
    # } else {
    #   stop("Did not find file: '", file.path(cache_dir, "ls_instructions.rds"), "'. Run 'framework::prepare_instructions()' and 'framework::implement_instructions()' and retry.")
    # }
    #
    # if (file.exists(file.path(cache_dir, "df_source_files.rds"))) {
    #   df_source_files <- readRDS(file = file.path(cache_dir, "df_source_files.rds"))
    # } else {
    #   stop("Did not find file: '", file.path(cache_dir, "df_source_files.rds"), "'. Run 'framework::prepare_instructions()', 'framework::implement_instructions()' and 'framework::check_instructions()' and retry.")
    # }

    # create all required directories in cache
    sapply(
      X = dirname(df_source_files$image_cache),
      FUN = dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )

# call specify_instructions()
    for (i in 1:nrow(df_source_files)) {
      instructions_specify(input_files = df_source_files$input_files[i],
        image_cache = df_source_files$image_cache[i],
        instruction = df_source_files$instruction[i],
        filename_noxt = df_source_files$filename_noxt[i],
        basename_noxt = df_source_files$basename_noxt[i],
        figure_source = df_source_files$figure_source[i],
        toplvl = toplvl,
        rebuild_figures = rebuild_figures,
        quiet_processing = quiet_processing,
        Rplots_device = Rplots_device
      )
    }

    if (file.exists(file.path(cache_dir, "df_source_files_last.rds"))) {
      df_source_files_last <- readRDS(file.path(cache_dir, "df_source_files_last.rds"))
      remove_image <- df_source_files_last$image_cache[!df_source_files_last$image_cache %in% df_source_files$image_cache]
      if (length(remove_image > 0)) {
        unlink(remove_image, recursive = TRUE)
      }
      # assign df_source_files_last to environment framework_params
      assign(x = "df_source_files_last", value = df_source_files_last, pos = "framework_params")
      # remove df_source_files_last.rds
      unlink(file.path(cache_dir, "df_source_files_last.rds"), recursive = TRUE)
      # write df_source_files.rds (stored information for subsequent function calls)
    }
    saveRDS(object = df_source_files,
            file = file.path(cache_dir,
                             "df_source_files_last.rds"))

    if (file.exists(file.path(cache_dir, "ls_instructions_last.rds"))) {
      ls_instructions_last <- readRDS(file.path(cache_dir, "ls_instructions_last.rds"))
      # assign df_source_files_last to environment framework_params
      assign(x = "ls_instructions_last", value = ls_instructions_last, pos = "framework_params")
      # remove ls_instructions_last.rds
      unlink(file.path(cache_dir, "ls_instructions_last.rds"), recursive = TRUE)
    }
    # write ls_instructions.rds (stored information for subsequent function calls)
    saveRDS(object = ls_instructions,
            file = file.path(cache_dir,
                             "ls_instructions_last.rds"))

    # remove deprecated snapshots
    if (file.exists(path_snapshot_source_dir) == TRUE) {
      file.remove(path_snapshot_source_dir)
    }
    if (file.exists(path_snapshot_data_dir) == TRUE) {
      file.remove(path_snapshot_data_dir)
    }

    # write new snapshots
    snapshot_source_dir <- utils::fileSnapshot(path = input_dir,
                                               md5sum = TRUE,
                                               recursive = TRUE)
    saveRDS(object = snapshot_source_dir, file = path_snapshot_source_dir)

    if (length(list.files(path = data_dir, recursive = TRUE, all.files = TRUE)) > 0) {
      snapshot_data_dir <- utils::fileSnapshot(path = data_dir,
                                               md5sum = TRUE,
                                               recursive = TRUE)
      saveRDS(object = snapshot_data_dir, file = path_snapshot_data_dir)
  }
}