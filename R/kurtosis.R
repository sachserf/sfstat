#' Kurtosis
#'
#' @param value Numeric vector
#'
#' @references Crawley, Michael J. The R Book. Chichester, England; Hoboken, N.J.: Wiley, 2007.
#' @export
#'
#' @examples kurtosis(iris$Sepal.Length)
kurtosis <- function(value) {
  m4 <- sum((value - mean(value, na.rm = TRUE))^4, na.rm = TRUE)/length(stats::na.omit(value))
  s4 <- stats::var(value, na.rm = TRUE)^2
  m4/s4 - 3
}


