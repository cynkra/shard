#' @export
shard_write_csv <- function(x, name, dir = ".", ..., shard_by = NULL, delimiter = "-", na = "") {
  split <- shard_split(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  split$path <- file.path(dir, split$path)
  write_csv_split(split, ..., na = na)
}

write_csv_split <- function(split, ...) {
  write_split(split, function(data, path) readr::write_csv(data, path, ...))
}

#' @export
shard_read_csv <- function(name, dir = ".", ..., delimiter = "-") {
  # FIXME: Add spec = NULL argument, call readr::spec_csv() once if unset
  path <- fs::dir_ls(fs::path(dir, name), glob = "*.csv", recurse = TRUE, type = c("file", "symlink"))
  data <- map(path, readr::read_csv, ...)
  split <- tibble(path = fs::path_rel(path, dir), data)
  shard_bind(split, delimiter = delimiter)
}
