#' Compute Confidence Intervalls via Bootstrap
#'
#' @param x Numeric integer. The sample.
#' @param k Integer. Number of samples to compute mean (for each iteration).
#' @param n Integer. Number of iterations.
#'
#' @export
#'
#' @examples bootci(iris$Sepal.Length, 50, 10000)
bootci <- function(x, k, n) {
  a <- numeric(n)
  for (i in 1:n) {
    a[i] <- mean(sample(x, k, replace = T))
  }
  stats::quantile(a, c(.025, .975))
}
