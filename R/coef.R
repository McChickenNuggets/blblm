#' Get coefficient of objects from fitted model
#'
#' @param object fit object
#' @param ... extra conditions
#'
#' @return coefficient of fit objects
#' @export
#' @method coef blblm
#'
#' @examples
#' coef(blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100))
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' @export
#' @method coef blbglm
coef.blbglm<-function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}
