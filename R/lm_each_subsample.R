#' compute the estimates for every subsample
#'
#' @param formula formula being passed from blblm
#' @param data data
#' @param n number of rows for data
#' @param B number of bootstraps
#'
#' @return a list of lm objects
#'
#'
lm_each_subsample <- function(formula, data, n, B) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, lm1(X, y, n), simplify = FALSE)
}
