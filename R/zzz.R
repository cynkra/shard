.onLoad <- function(libname, pkgname) {
  # Necessary to re-parse environment variable
  if (requireNamespace("debugme", quietly = TRUE)) {
    #activate_debugme()
    debugme::debugme()
    debug_info()
  }

  shard_read_csv_from_info <<- memoise::memoise(shard_read_csv_from_info)
  read_csv_cache <<- memoise::memoise(read_csv_cache, ~ info_for_cache(fs::file_info("DESCRIPTION")))

  shard_read_tsv_from_info <<- memoise::memoise(shard_read_tsv_from_info)
  read_tsv_cache <<- memoise::memoise(read_tsv_cache, ~ info_for_cache(fs::file_info("DESCRIPTION")))
}

activate_debugme <- function(level = 2) {
  old_debugme <- remove_from_logging(get_debugme())
  old_debugme <- gsub("(.)$", "\\1,", old_debugme)

  my_debugme <- paste0(strrep("!", level), get_pkgname())

  set_debugme(paste0(old_debugme, my_debugme))
}

deactivate_debugme <- function() {
  new_debugme <- remove_from_logging(get_debugme())
  set_debugme(new_debugme)
}

get_debugme <- function() {
  Sys.getenv("DEBUGME")
}

set_debugme <- function(debugme) {
  Sys.setenv("DEBUGME" = debugme)
  message("DEBUGME=", debugme)
}

remove_from_logging <- function(spec) {
  spec <- gsub(paste0("!*", get_pkgname(), ""), "", spec)
  spec <- gsub(",,+", ",", spec)
  spec
}

debug_info <- function(pkgname) {
  "!DEBUG Loaded"
  "!!DEBUG Level 2"
  "!!!DEBUG Level 3"
  "!!!!DEBUG Level 4"
  "!!!!!DEBUG Level 5"
  "!!!!!!DEBUG Level 6"
}

get_pkgname <- function() {
  environmentName(topenv(environment()))
}
