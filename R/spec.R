#' @export
build_shard_spec <- function(x, name, extension, ...,
                             shard_by = NULL, container = NULL,
                             delimiter = "-") {
  ellipsis::check_dots_empty()

  shard_by <- tidyselect::eval_select(enquo(shard_by), x)
  container <- tidyselect::eval_select(enquo(container), x)

  # Prepend artificial row full of NAs to avoid corner cases
  if (nrow(x) == 0) {
    # FIXME: Alternative check, more convoluted:
    # all(purrr::map_lgl(x[1, ], is.na))
    x <- x[c(NA, seq_len(nrow(x))), ]
  }

  nested <-
    x %>%
    nest(data = -!!shard_by) %>%
    select(!!!syms(names(shard_by)), data)

  nested_spec <- nested_build_shard_spec(
    nested, name, extension,
    container = container,
    delimiter = delimiter
  )

  tibble(
    path = paste0(name, "/"),
    data = list(nested_spec)
  )
}

nested_build_shard_spec <- function(nested, name, extension, container, delimiter) {
  if (length(nested) - 1 <= length(container)) {
    tibble::tibble(
      path = paste0(name, ".", extension),
      nested
    )
  } else if (length(nested) - 2 <= length(container)) {
    first_name <- names(nested)[[1]]
    nested %>%
      transmute(
        path = paste0(name, delimiter, first_name, "=", !!sym(first_name), ".", extension),
        data
      )
  } else {
    sub_nested <-
      nested %>%
      nest(data = -1)

    first_name <- names(sub_nested)[[1]]
    sub_nested %>%
      transmute(
        path = paste0(first_name, "=", !!sym(first_name), "/"),
        data = purrr::map(data, ~nested_build_shard_spec(.x, name, extension, container, delimiter))
      )
  }
}
