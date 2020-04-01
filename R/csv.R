#' @export
write_csv_sharded <- function(x, name, dir, ..., shard_by = NULL, delimiter = "-") {
  spec <- build_shard_spec(x, name, "csv", shard_by = !!enexpr(shard_by), delimiter = delimiter)
  write_csv_spec(spec, dir, ...)
}

write_csv_spec <- function(spec, dir, ...) {
  dir.create(dir, showWarnings = FALSE)
  purrr::pwalk(spec, function(path, data) write_csv_spec_row(
    data, file.path(dir, path), ...
  ))
}

write_csv_spec_row <- function(data, path, ...) {
  if (grepl("/$", path)) {
    write_csv_spec(data, path, ...)
  } else {
    message("Writing ", path)
    readr::write_csv(data, path, ...)
  }
}
