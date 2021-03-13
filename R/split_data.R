#' Split data into m parts of approximated equal sizes
#'
#' @param data data
#' @param m number of splits
#'
#' @return list of splited data
#'
#'
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}
