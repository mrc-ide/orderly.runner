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


vnapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, numeric(1), ...)
}
