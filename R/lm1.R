#' compute the linear regression estimates for a blb dataset
#'
#' @param X explanatory variables
#' @param y response variable
#' @param n number of rows
#'
#' @return list consists of coefficient and sigma
#'
lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}
