#' Print the formula of blblm object
#' @importFrom utils capture.output
#' @param x blblm object
#' @param ... extra conditions
#'
#' @return formula of blblm object x
#' @export
#' @method print blblm
#'
#' @examples
#' print(blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100))
print.blblm <- function(x, ...) {
  cat(class(x), "model:", capture.output(x$formula))
  cat("\n")
}
