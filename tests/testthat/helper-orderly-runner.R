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

test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(tmp, ...))
  copy_examples(examples, tmp)
  as.character(fs::path_norm(tmp))
}


copy_examples <- function(examples, path_src) {
  fs::dir_create(path_src)

  fs::dir_create(file.path(path_src, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(path_src, "src"))
  }
}


helper_add_git <- function(path) {
  gert::git_init(path)
  gert::git_add(".", repo = path)
  user <- "author <author@example.com>"
  sha <- gert::git_commit("initial", author = user, committer = user,
                          repo = path)
  branch <- gert::git_branch(repo = path)
  url <- "https://example.com/git"
  gert::git_remote_add(url, repo = path)
  list(path = path, user = user, branch = branch, sha = sha, url = url)
}

new_queue_quietly <- function(root, ...) {
  suppressMessages(Queue$new(root, ...))
}

make_worker_dirs <- function(orderly_root, ids) {
  packit_path <- file.path(orderly_root, ".packit")
  dir.create(packit_path)
  workers <- file.path(packit_path, "workers")
  dir.create(workers)
  lapply(ids, function(id) {
    worker_path <- file.path(workers, id)
    dir.create(worker_path)
    gert::git_clone(orderly_root, path = worker_path)
  })

}

start_queue_workers_quietly <- function(n_workers,
                                        controller, env = parent.frame()) {
  worker_manager <- suppressMessages(
    rrq::rrq_worker_spawn2(n_workers, controller = controller)
  )
  withr::defer(rrq::rrq_worker_stop(controller = controller), env = env)
  worker_manager
}

skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}

expect_worker_task_complete <- function(task_id, controller, n_tries) {
  is_completed <- FALSE
  for (i in seq_len(n_tries)) {
    is_completed <- rrq::rrq_task_status(
      task_id, controller = controller
    ) == "COMPLETE"
    if (is_completed == TRUE) {
      break
    }
    Sys.sleep(1)
  }
  expect_equal(is_completed, TRUE)
}
