#' Skewness
#'
#' @param value Numeric vector.
#'
#' @references https://stat.ethz.ch/pipermail/r-help/1999-July/004529.html
#' @export
#'
#' @examples skew(iris$Sepal.Length)
skew <-  function(value) {
  m3 <- mean((value - mean(value, na.rm = TRUE))^3, na.rm = TRUE)
  skew <- m3/(stats::sd(value, na.rm = TRUE)^3)
  skew
}
