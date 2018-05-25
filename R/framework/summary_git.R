#' Wrapper for git2r::summary
#'
#' @param git_repo Character. Directory of your git repository.
#'
#' @seealso \code{\link[utils]{installed.packages}}, \code{\link[git2r]{repository}}
#' @export
summary_git <- function(git_repo) {
  if ('git2r' %in% utils::installed.packages()) {
    message('\ngit:\n')
    git2r::summary(git2r::repository(git_repo))
  }
}
