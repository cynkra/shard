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
shard_read_tsv <- function(name, dir = ".", ..., delimiter = "-", col_names = NULL, col_types = NULL, skip = NULL) {
  stopifnot(is.null(skip))

  path <- fs::dir_ls(fs::path_abs(name, start = dir), glob = "*.tsv", recurse = TRUE, type = c("file", "symlink"))

  if (length(path) == 0) {
    return(tibble())
  }

  if (is.null(col_names) || is.null(col_types)) {
    if (is.null(col_names)) {
      col_names <- TRUE
    }

    suppressMessages(col_types <- readr::spec_tsv(path[[1]], col_names = col_names, col_types = col_types))
    col_names <- names(col_types$cols)
  }

  data <- map(path, readr::read_tsv, ..., col_names = col_names, col_types = col_types, skip = 1L)
  split <- tibble(path = fs::path_rel(path, dir), data)
  shard_bind(split, delimiter = delimiter)
}
