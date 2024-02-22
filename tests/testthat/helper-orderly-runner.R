orderly_runner_endpoint <- function(method, path, root, validate = TRUE) {
  porcelain::porcelain_package_endpoint("orderly.runner", method, path,
                                        state = list(root = root),
                                        validate = validate)
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(path, ...))
}

new_queue_quietly <- function(root, ...) {
  suppressMessages(Queue$new(root, ...))
}

skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}
