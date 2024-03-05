`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


package_version_string <- function(name) {
  as.character(utils::packageVersion(name))
}


vcapply <- function(...) {
  vapply(FUN.VALUE = character(1), ...)
}


vnapply <- function(...) {
  vapply(FUN.VALUE = numeric(1), ...)
}


vlapply <- function(...) {
  vapply(FUN.VALUE = logical(1), ...)
}


system3 <- function(command, args) {
  res <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  code <- attr(res, "status") %||% 0
  attr(res, "status") <- NULL
  list(success = code == 0,
       code = code,
       output = res)
}


sys_which <- function(name) {
  path <- Sys.which(name)
  if (!nzchar(path)) {
    stop(sprintf("Did not find '%s'", name), call. = FALSE)
  }
  unname(path)
}


first_dirname <- function(paths) {
  first_dir <- function(path) {
    if (basename(path) == path) {
      dir <- path
    } else {
      dir <- first_dirname(dirname(path))
    }
    dir
  }
  vcapply(paths, first_dir, USE.NAMES = FALSE)
}
