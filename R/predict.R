#' Get the predicted value of new observation using blblm
#'
#' @param object blblm object
#' @param new_data new observation
#' @param confidence boolean value to specify whether to use confidence interval
#' @param level significance level
#' @param ... left blank
#'
#' @return predicted value of new observation using blblm
#' @export
#' @method predict blblm
#'
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}
