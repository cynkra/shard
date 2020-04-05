#' @export
shard_write_csv <- function(x, name, dir, ..., shard_by = NULL, delimiter = "-") {
  split <- shard_split(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  split$path <- file.path(dir, split$path)
  write_csv_split(split, ...)
}

write_csv_split <- function(split, ...) {
  fs::dir_create(unique(dirname(split$path)))
  pwalk(split, function(data, path) readr::write_csv(data, path, ...))
}
