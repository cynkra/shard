#' @export
shard_bind <- function(x, ..., delimiter = "-") {
  ellipsis::check_dots_empty()

  stopifnot(is.data.frame(x))
  stopifnot(identical(names(x), c("path", "data")))
  stopifnot(nrow(x) > 0)

  slashes <- nchar(gsub("[^/]", "", x$path))
  stopifnot(all_same(slashes))
  slashes <- slashes[[1]]
  stopifnot(slashes >= 1)

  cols <- c("name", paste0("path", seq_len(slashes)))

  paths <-
    x %>%
    separate(path, into = cols, sep = "/")

  name <- paths[[1]]
  stopifnot(all_same(name))
  name <- name[[1]]

  last_path <- paths[[slashes + 1]]
  extension <- gsub("^.*[.]([^.]+)$", "\\1", last_path)
  stopifnot(all_same(extension))
  extension <- extension[[1]]

  if (slashes == 1 && all_match(last_path, paste0("^", escape(name), "[.]"))) {
    paths[["path1"]] <- NULL
    slashes <- 0
  } else {
    rx <- paste0("^", escape(name), escape(delimiter), "(.*)", "[.]", escape(extension), "$")
    stopifnot(all_match(last_path, rx))
    paths[[slashes + 1]] <- gsub(rx, "\\1", last_path)
  }

  paths[[1]] <- NULL
  path_idx <- seq_len(slashes)

  colnames <- map(paths[path_idx], ~ gsub("=.*$", "", .x))
  stopifnot(map_lgl(colnames, all_same))
  colnames <- map_chr(colnames, 1)

  new_unshard_quo <- function(old_name, new_name) {
    quo(type.convert(gsub("^[^=]*=", "", !!sym(old_name)), na.strings = "", as.is = TRUE))
  }

  cols <- map(set_names(names(colnames), colnames), new_unshard_quo)

  paths %>%
    transmute(!!!cols, data) %>%
    unnest(data)
}

all_same <- function(x) {
  length(unique(x)) == 1
}

all_match <- function(x, pattern) {
  is_empty(grep(pattern, x, invert = TRUE))
}

# From rex
escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
}

empty_as_na <- function(x) {
  x[x == ""] <- NA
  x
}
