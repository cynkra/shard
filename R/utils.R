expand_first_nested <- function(x) {
  x <-
    x %>%
    slice(1)

  if ("data" %in% names(x)) {
    x <-
      x %>%
      mutate(data = expand_first_nested(data[[1]]))
  } else {
    x <-
      x %>%
      select(1)
  }

  x
}
