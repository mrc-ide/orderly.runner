orderly_runner_endpoint <- function(method, path, root, validate = TRUE) {
  porcelain::porcelain_package_endpoint("orderly.runner", method, path,
                                        state = list(root = root),
                                        validate = validate)
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  orderly2::orderly_init(path, ..., logging_console = FALSE)
}
