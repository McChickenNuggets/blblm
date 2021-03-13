#' bag of little bootstrap for logistic regression model
#'
#' @param formula formula to use in blbglm
#' @param data data
#' @param m number of splits
#' @param B number of boostraps
#' @param family glm family to specify
#' @param Parallel boolean value to specify whether to use parallelization
#'
#' @return blbglm object
#' @export
#'
#' @examples
#' blbglm(Species ~ Sepal.Length * Sepal.Width, iris[1:100,], 3, 100, family = binomial)
#' blbglm(Species ~ Sepal.Length * Sepal.Width, iris[1:100,], 3, 100, binomial, TRUE)
blbglm <- function(formula, data, m = 10, B = 5000, family, Parallel = FALSE){
  data_list<-split_data(data,m)
  if(Parallel){
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B , family)
    )
  }else{
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B ,family)
    )
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}
