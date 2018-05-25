#' Print remarks on single sample statistics
#'
#' @export
#'
#' @examples skim_remarks()
skim_remarks <- function() {
  writeLines("Remarks on variables:\n\n- p_sw: p.value of Shapiro-Wilk Normality Test.\n- skew: positive skewness = right tail longer.\n- kurtosis: platykurtic = flat-topped = negative; leptokurtic = pointy = positive.\n- cv: Coefficient of Variation (relative standard deviation); does not depend on absolute values = comparable between datasets; sd(x)/mean(x).\n- iqr: interquartile range = values between Q1 and Q3.\n- mad: mean absolute deviation (around the median) = robust alternative to iqr; average of the absolute deviation from the median.\n- sem: standard error of the mean; sd(x)/sqrt(length(x)); TRUE mean with 95cat('\u25') within +/- 2 sem around mean(x); often used in barplots of mean-values.\n- s: standard deviation = 65cat('\u25') of data within +/- 1 sd(x); valid under normality! ()\n-CI: confidence interval (lower, upper = 95cat('\u25')); computed via t.test; interpretation: if data acquisition will be repeated 100 times 95cat('\u25') of the mean values should fall within CI.\n\ngeneral remarks for interpretation:\n\n- overlapping CI or sem between samples = not signif. different BUT: non-overlapping does NOT INDICATE SIGNIFICANT DIFFERENCE.\n- correct declaration of mean value e.g. ([M]; 1,35; 95 cat('\u25') CI; 1,23 – 1,56)\n\n")
}
