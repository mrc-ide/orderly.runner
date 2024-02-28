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

start_queue_workers_quietly <- function(n_workers,
                                        controller, env = parent.frame()) {
  suppressMessages(
    rrq::rrq_worker_spawn2(n_workers, controller = controller)
  )
  withr::defer(rrq::rrq_worker_stop(controller = controller), env = env)
}

skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}
