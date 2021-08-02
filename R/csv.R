#' @export
shard_write_csv <- function(x, name, dir = ".", ..., shard_by = NULL, delimiter = "-", na = "") {
  check_dots_empty()

  split <- shard_split(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  split$path <- fs::path(dir, split$path)

  target_dir <- fs::path(dir, name)
  if (fs::dir_exists(target_dir)) {
    fs::dir_delete(target_dir)
  }

  write_csv_split(split, ..., na = na)
}

write_csv_split <- function(split, ...) {
  write_split(split, function(data, path) readr::write_csv(data, path, ...))
}

#' @export
shard_read_csv <- function(name, dir = ".", ..., delimiter = "-",
                           col_names = NULL, col_types = NULL, skip = NULL, version = 2L) {
  stopifnot(is.null(skip))

  info <-
    fs::dir_info(
      fs::path_abs(name, start = dir),
      glob = "*.csv",
      recurse = TRUE,
      type = c("file", "symlink")
    ) %>%
    info_for_cache()

  shard_read_csv_from_info(info, dir, delimiter, version, col_names, col_types, ...)
}

shard_read_csv_from_info <- function(info, dir, delimiter, version, col_names, col_types,
                                     ...) {
  path <- info$path

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

  data <- map(path, read_csv_cache, ..., col_names = col_names, col_types = col_types, skip = 1L)
  split <- tibble(path = fs::path_rel(path, dir), data)
  shard_bind(split, delimiter = delimiter, version = version)
}

read_csv_cache <- function(file, ..., .info = info_for_cache(fs::file_info(file))) {
  readr::read_csv(file, ...)
}
