#' Specify instructions!
#'
#' @description This function is not meant to be called directly by the user. It
#'   is integrated into the function 'instructions_execute' for batch processing of input_files.
#' @param input_files Character. File path relative to the current working directory. Possible file extensions: Rmd, R and Rnw.
#' @param image_cache Character. File path to write an image.
#' @param instruction Character. Possible values: 'nothing', 'load', 'source', 'render' and 'knit'.
#' @param filename_noxt Character. Complete file path without extension.
#' @param basename_noxt Character. Basename of the file without extension.
#' @param figure_source Character. File path to write figures.
#' @param toplvl Character. Passed to knit_root_dir.
#' @param rebuild_figures Logical. Delete figure_source before processing?
#' @param quiet_processing Logical. Passed to !echo (if instruction == source) or quiet (if instruction == render or knit).
#' @param Rplots_device Function. Specify function that will be passed to a functional to write figures of R-files (if instruction == source). E.g. grDevices::pdf or grDevices::png.
#' @note This function is part of a family of functions each of which begin with
#'   'instructions_'. The order to call these functions is:
#'   'instructions_prepare', 'instructions_implement', 'instructions_check',
#'   'instructions_execute', 'instructions_symlink' and 'instructions_supplement'.
#' @seealso \code{\link{instructions_prepare}}, \code{\link{instructions_implement}},
#'   \code{\link{instructions_check}},
#'   \code{\link{instructions_execute}},
#'   \code{\link{instructions_symlink}}, \code{\link{instructions_supplement}}
#' @author Frederik Sachser
#' @export
instructions_specify <-
  function(input_files,
           image_cache,
           instruction,
           filename_noxt,
           basename_noxt,
           figure_source,
           toplvl,
           rebuild_figures,
           quiet_processing,
           Rplots_device)
  {
    if (instruction != "nothing") {
      # specify instruction "load"
      if (instruction == "load") {
        load(image_cache, envir = .GlobalEnv)
      } else {
        if (rebuild_figures == TRUE) {
          unlink(figure_source, recursive = TRUE)
          dir.create(figure_source, recursive = TRUE)
        }
        cat(
          "\n#############################################\n",
          input_files,
          "\n#############################################\n"
        )
        # specify instruction "source"
        if (instruction == "source") {
          warmesfile <- paste0(filename_noxt, "_warnings.Rout")
          if (quiet_processing == TRUE) {
          dir.create(path = figure_source,
                     showWarnings = FALSE,
                     recursive = TRUE)
          con <-  file(warmesfile, open = "w")
         # sink(file = "all.Rout")
          sink(file = con, type = "message")

          if (!is.null(Rplots_device)) {
            if (length(grep("pdf", deparse(Rplots_device)[1])) == 1) {
              set_grDevice <- function(f) f(paste0(figure_source, .Platform$file.sep, "%03d"), onefile = FALSE)
            } else {
              set_grDevice <- function(f) f(paste0(figure_source, .Platform$file.sep, "%03d"))
            }
          set_grDevice(Rplots_device)
          source(input_files, echo = !quiet_processing)
          allfig <- list.files(figure_source, full.names = TRUE)
          if (length(allfig) > 0) {
            fig_wo_ext <- allfig[which(tools::file_ext(allfig) == "")]
            file.rename(from = fig_wo_ext, to = paste0(fig_wo_ext, ".", names(grDevices::dev.cur())))
          }
          grDevices::dev.off()
          } else {
            source(input_files, echo = !quiet_processing)
          }

        #  sink()
          sink(type = "message")
          close(con)
          writeLines(readLines(warmesfile), stderr())
          } else {
            dir.create(path = figure_source,
                       showWarnings = FALSE,
                       recursive = TRUE)
            con <-  file(warmesfile)
            sink(file = con)
            sink(file = con, type = "message")

            if (!is.null(Rplots_device)) {
              if (length(grep("pdf", deparse(Rplots_device)[1])) == 1) {
                set_grDevice <- function(f) f(paste0(figure_source, .Platform$file.sep, "%03d"), onefile = FALSE)
              } else {
                set_grDevice <- function(f) f(paste0(figure_source, .Platform$file.sep, "%03d"))
              }
              set_grDevice(Rplots_device)
            source(input_files, echo = !quiet_processing, max.deparse.length = 1000)
              allfig <- list.files(figure_source, full.names = TRUE)
              if (length(allfig) > 0) {
                fig_wo_ext <- allfig[which(tools::file_ext(allfig) == "")]
                file.rename(from = fig_wo_ext, to = paste0(fig_wo_ext, ".", names(grDevices::dev.cur())))
              }
            grDevices::dev.off()
            } else {
              source(input_files, echo = !quiet_processing, max.deparse.length = 1000)
            }

            sink(type = "message")
            sink()
            close(con)
#            writeLines(readLines(warmesfile), stdout())
            thelines <- readLines(warmesfile)
            stderrlines <- grep(pattern = "^Warning*", thelines)
            stderrlines <- c(stderrlines, stderrlines - 1)
            stdoutlines <- seq_along(thelines)[-stderrlines]

            for (i in seq_along(thelines)) {
              if (i %in% stdoutlines) {
                writeLines(thelines[i], con = stdout())
              }
              if (i %in% stderrlines) {
                writeLines(thelines[i], con = stderr())
              }
            }

            thewarnings <- thelines[sort(stderrlines)]
            writeLines(text = thewarnings, con = warmesfile, sep = "\n")

        #    thelines <- readLines(paste0(filename_noxt, "_warmes.Rout"))
        #    warninglines <- grep(pattern = "^Warning*", thelines)
         #   warninglines <- c(warninglines, warninglines - 1)
        ##    thelines <- thelines[sort(warninglines)]
        #    writeLines(text = thelines, con = (paste0(filename_noxt, "_warmes.Rout")), sep = "\n")
        #    writeLines(readLines(paste0(filename_noxt, "_warmes.Rout")), stderr())

            #         sink()

          }
 #         sink()

        }
        # specify instruction "render"
        else if (instruction == "render") {
          unlink(warmesfile <- paste0(filename_noxt, "_warnings.Rout"))
          rmarkdown::render(
            input = input_files,
            envir = globalenv(),
            output_format = "all",
            knit_root_dir = toplvl,
            clean = FALSE,
            quiet = quiet_processing
          )
        }
        # specify instruction "knit"
        else if (instruction == "knit") {
          knitr::knit(
            input = input_files,
            envir = globalenv(),
            quiet = quiet_processing,
            output = file.path(toplvl, paste0(filename_noxt, ".tex"))
          )
          if (dir.exists("figure")) {
            dir.create(figure_source,
                       recursive = TRUE,
                       showWarnings = FALSE)
            file.copy("figure", dirname(figure_source), recursive = TRUE)
            unlink("figure", recursive = TRUE)
          }
          tools::texi2pdf(
            file = paste0(filename_noxt, ".tex"),
            clean = FALSE,
            quiet = TRUE
          )
          file.rename(
            from = paste0(basename_noxt, ".pdf"),
            to = paste0(filename_noxt, ".pdf")
          )
          file.rename(
            from = paste0(basename_noxt, ".log"),
            to = paste0(filename_noxt, ".log")
          )
          file.rename(
            from = paste0(basename_noxt, ".aux"),
            to = paste0(filename_noxt, ".aux")
          )
        }
        save(list = ls(.GlobalEnv), file = image_cache)
      }
    }
  }
