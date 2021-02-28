#' @export
shard_split <- function(x, name, extension, ...,
                             shard_by = NULL, delimiter = "-") {
  ellipsis::check_dots_empty()

  shard_by <- tidyselect::eval_select(enquo(shard_by), x)

  # Prepend artificial row full of NAs to avoid corner cases
  if (nrow(x) == 0) {
    # FIXME: Alternative check, more convoluted:
    # all(purrr::map_lgl(x[1, ], is.na))
    x <- x[c(NA, seq_len(nrow(x))), ]
    extra_row <- TRUE
  } else {
    extra_row <- FALSE
  }

  new_shard_quo <- function(shard, last) {
    if (last) {
      quo(paste0(!!name, delimiter, !!shard, "=", na_as_empty(!!sym(shard))))
    } else {
      quo(paste0(!!shard, "=", na_as_empty(!!sym(shard))))
    }
  }

  chars <- map2(
    set_names(names(shard_by)),
    seq_along(shard_by) == length(shard_by),
    new_shard_quo
  )

  nested <-
    x %>%
    nest(data = -!!shard_by) %>%
    select(!!!syms(names(shard_by)), data) %>%
    arrange(!!!syms(names(shard_by))) %>%
    mutate(!!!chars)

  if (length(nested) <= 1) {
    flat <- tibble::tibble(path = !!name, nested)
  } else {
    all_chars <- quo(paste(!!!syms(names(shard_by)), sep = "/"))
    flat <-
      nested %>%
      unite(path, !!!syms(names(shard_by)), sep = "/")
  }

  if (extra_row) {
    flat$data[[1]] <- flat$data[[1]][0, ]
  }

  flat %>%
    mutate(path = paste0(name, "/", path, ".", extension))
}

na_as_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

write_split <- function(split, writer) {
  fs::dir_create(unique(dirname(split$path)))
  pwalk(split, writer)
}
