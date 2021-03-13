#' compute sigma from fit
#'
#' @param fit object
#'
#' @return sigma
#'
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}
