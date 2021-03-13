#' compute glm for each bootstrap
#'
#' @param formula formula being passed from blbglm
#' @param data data
#' @param n number of rows for data
#' @param B number of bootstraps
#' @param family glm family to specify
#'
#' @return a list of glm objects
#' @export
#'
glm_each_subsample <- function(formula, data, n, B, family) {
  replicate(B, glm1(formula, data, n, family), simplify = FALSE)
}
