#' Automatically stage all changes and commit
#'
#' @description This function will stage all changes and commit a snapshot with a predefined message (including user and timestamp) via git2r.
#'
#' @param repopath Character. Specify path to existing repository.
#' @param autobranch Character. Specify the name of an existing branch. Only if HEAD is pointing to this branch an auto-snapshot will be created. Exception: Choose NULL (default) to ignore branch-dependency.
#'
#' @seealso \code{\link[git2r]{repository}}, \code{\link[git2r]{status}}, \code{\link[git2r]{add}}, \code{\link[git2r]{commit}}
#' 
#' @export
autosnapshot <- function(repopath = getwd(), autobranch = NULL) {
  gr <- git2r::repository(path = repopath)

  if (is.null(autobranch)) {
    autobranch <- attributes(git2r::head(gr))[["name"]]
  }

  if (isTRUE(attributes(git2r::head(gr))[["name"]] == autobranch)) {
    git2r::add(gr, "*")
    if (length(git2r::status(repo = gr)[["staged"]]) == 0) {
      return(message("Nothing staged for commit."))
    } else {
      message()
    git2r::commit(
      gr,
      paste0(
        "auto-snapshot via git2r from ",
        Sys.info()["user"],
        " at ",
        Sys.time()
      )
    )
    return(message(paste0(
      "commit-message: auto-snapshot via git2r from ",
      Sys.info()["user"],
      " at ",
      Sys.time()
    )))
    }
  } else {
    return(message("HEAD != autobranch. Ignoring auto-snapshot."))
  }
}
