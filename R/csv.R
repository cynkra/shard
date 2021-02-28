#' @export
shard_write_csv <- function(x, name, dir = ".", ..., shard_by = NULL, delimiter = "-", na = "") {
  check_dots_empty()

  split <- shard_split(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  split$path <- file.path(dir, split$path)
  write_csv_split(split, ..., na = na)
}

write_csv_split <- function(split, ...) {
  write_split(split, function(data, path) readr::write_csv(data, path, ...))
}

#' @export
shard_read_csv <- function(name, dir = ".", ..., delimiter = "-", col_names = NULL, col_types = NULL, skip = NULL) {
  stopifnot(is.null(skip))

  path <- fs::dir_ls(fs::path_abs(name, start = dir), glob = "*.csv", recurse = TRUE, type = c("file", "symlink"))

  if (length(path) == 0) {
    return(tibble())
  }

  if (is.null(col_names) || is.null(col_types)) {
    if (is.null(col_names)) {
      col_names <- TRUE
    }

    suppressMessages(col_types <- readr::spec_csv(path[[1]], col_names = col_names, col_types = col_types))
    col_names <- names(col_types$cols)
  }

  data <- map(path, readr::read_csv, ..., col_names = col_names, col_types = col_types, skip = 1L)
  split <- tibble(path = fs::path_rel(path, dir), data)
  shard_bind(split, delimiter = delimiter)
}
