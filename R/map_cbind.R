map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}
