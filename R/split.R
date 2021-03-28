#' @export
shard_split <- function(x, name, extension, ...,
                        shard_by = NULL, delimiter = "-") {
  ellipsis::check_dots_empty()

  shard_split_flat(x, name, {{ shard_by }}, delimiter) %>%
    mutate(path = paste0(!!name, "/", path, ".", !!extension))
}

shard_split_flat <- function(x, name, shard_by_in, delimiter) {
  shard_by_quo <- enquo(shard_by_in)

  if (quo_is_null(shard_by_quo)) {
    flat <- tibble(path = !!name, data = list(as_tibble(x)))
    return(flat)
  }

  shard_by <- tidyselect::eval_select(shard_by_quo, x)
  shard_by_syms <- syms(names(shard_by))

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

  grouped <-
    x %>%
    group_by(!!!shard_by_syms)

  nested <-
    bind_cols(group_keys(grouped), tibble(data = group_split(grouped))) %>%
    remove_rownames() %>%
    select(!!!shard_by_syms, data) %>%
    arrange(!!!shard_by_syms) %>%
    mutate(!!!chars)

  all_chars <- quo(paste(!!!shard_by_syms, sep = "/"))
  flat <-
    nested %>%
    unite(path, !!!shard_by_syms, sep = "/")

  if (extra_row) {
    flat$data[[1]] <- flat$data[[1]][0, ]
  }

  flat
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
