map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}
