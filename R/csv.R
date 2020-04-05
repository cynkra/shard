#' @export
write_csv_sharded <- function(x, name, dir, ..., shard_by = NULL, delimiter = "-") {
  spec <- shard_split(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  spec$path <- file.path(dir, spec$path)
  write_csv_spec(spec, ...)
}

write_csv_spec <- function(spec, ...) {
  fs::dir_create(unique(dirname(spec$path)))
  pwalk(spec, function(data, path) readr::write_csv(data, path, ...))
}
