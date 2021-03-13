#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"
#'
#' bag of little bootstrap for linear regression model
#' @param formula formula to use in blblm
#' @param data data
#' @param m number of splits
#' @param B number of boostraps
#' @param Parallel boolean value to specify whether to use parallelization
#'
#' @return blblm object
#' @export
#'
#' @examples
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, Parallel = FALSE)
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, Parallel = TRUE )
blblm <- function(formula, data, m = 10, B = 5000, Parallel = FALSE) {
  data_list <- split_data(data, m)
  if(Parallel){
    estimates<- future_map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B=B)
    )
  }else{
    estimates <- map(
        data_list,
        ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
