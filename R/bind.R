#' @export
shard_bind <- function(x, ..., delimiter = "-") {
  ellipsis::check_dots_empty()

  bind_rows(x$data)
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
