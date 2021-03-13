#' Calculate the sigma value from fit
#'
#' @param object fit object
#' @param confidence boolean value to specify whether to have confidence interval
#' @param level significance level
#' @param ... extra conditions
#'
#' @return sigma value of object
#' @export
#' @method sigma blblm
#'
#' @examples
#' sigma(blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100))
#' sigma(blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100), confidence = TRUE)
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}
