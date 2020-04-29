#' @export
shard_write_tsv <- function(x, name, dir = ".", ..., shard_by = NULL, delimiter = "-", na = "") {
  split <- shard_split(x, name, "tsv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  split$path <- file.path(dir, split$path)
  write_tsv_split(split, ..., na = na)
}

write_tsv_split <- function(split, ...) {
  write_split(split, function(data, path) readr::write_tsv(data, path, ...))
}

#' @export
shard_read_tsv <- function(name, dir = ".", ..., delimiter = "-") {
  # FIXME: Add spec = NULL argument, call readr::spec_tsv() once if unset
  path <- fs::dir_ls(fs::path(dir, name), glob = "*.tsv", recurse = TRUE, type = c("file", "symlink"))
  data <- map(path, readr::read_tsv, ...)
  split <- tibble(path = fs::path_rel(path, dir), data)
  shard_bind(split, delimiter = delimiter)
}
