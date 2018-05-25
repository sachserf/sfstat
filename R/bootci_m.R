#' Compute Confidence Intervalls via Bootstrap for multiple fractions
#'
#' @description Choose multiple fractions of sample size to track conversion of CI
#'
#' @param x Numeric Vector.
#' @param iteration Integer. Specify number of iterations.
#' @param fraction Numeric vector of fractions. e.g. 0.5 means that ~50 percent of original sample size will be used to compute mean value for each iteration.
#' @param digits Integer. Specify number of digits.
#'
#' @note For faster computation of a single fraction use bootci instead.
#' @references Crawley, Michael J. The R Book. Chichester, England; Hoboken, N.J.: Wiley, 2007.
#'
#' @export
bootci_m <- function(x, iteration = 1000, fraction = c(0.05, 0.25, 0.5), digits = 2) {
  n <- length(x)
  k <- round(fraction*n)
  fraction <- k/n
  foo <- data.frame(iteration = iteration, sample_size = k, fraction = fraction)
  for (ss in k) {
    a <- numeric(iteration)
    for (i in 1:iteration) {
      a[i] <- mean(sample(x, ss, replace = T))
      foo$lower[foo$sample_size == ss] <- stats::quantile(a, .025)
      foo$upper[foo$sample_size == ss] <- stats::quantile(a, .975)
    }
  }
  if (!is.null(digits)) {
    foo <- round(foo, digits = digits)
  }
  return(foo)
}

