#' compute the logistic regression estimates for a blb dataset
#'
#' @param formula formula to use in blblm
#' @param data data
#' @param n number of rows
#' @param family glm family to specify
#'
#' @return list of coefficients and sigma
#' @export
#'
glm1<-function(formula, data, n, family){
  environment(formula) <- environment()
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  fit <- glm(formula, data, family = family, weights = freqs)
  list(coef = blbcoef(fit), sigma = sigma(fit))
}
