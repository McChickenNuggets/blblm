map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
