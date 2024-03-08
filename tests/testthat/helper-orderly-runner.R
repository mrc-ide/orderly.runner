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


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(tmp, ...))
  copy_examples(examples, tmp)
  as.character(fs::path_norm(tmp))
}


test_prepare_orderly_remote_example <- function(examples, ...) {
  path_remote <- test_prepare_orderly_example(examples, ...)
  helper_add_git(path_remote)
  path_local <- tempfile()
  withr::defer_parent(unlink(path_local, recursive = TRUE))
  gert::git_clone(path_remote, path_local)
  list(
    remote = path_remote,
    local = path_local
  )
}


copy_examples <- function(examples, path_src) {
  fs::dir_create(path_src)

  fs::dir_create(file.path(path_src, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(path_src, "src"))
  }
}


helper_add_git <- function(path, add = ".") {
  gert::git_init(path)
  gert::git_add(add, repo = path)
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

start_queue_with_workers <- function(root, n_workers, env = parent.frame()) {
  q <- new_queue_quietly(root)
  worker_manager <- start_queue_workers_quietly(n_workers, q$controller,
                                                env = env)
  make_worker_dirs(root, worker_manager$id)
  q
}

skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}

expect_worker_task_complete <- function(task_id, controller, n_tries) {
  is_task_successful <- rrq::rrq_task_wait(
    task_id, controller = controller, timeout = n_tries
  )
  expect_equal(is_task_successful, TRUE)
}

initialise_git_repo <- function() {
  t <- tempfile()
  dir.create(t)
  writeLines(c("# Example", "", "example repo"), file.path(t, "README.md"))
  helper_add_git(t)
}


create_new_commit <- function(path, new_file = "new", message = "new message",
                              add = ".") {
  writeLines("new file", file.path(path, new_file))
  gert::git_add(add, repo = path)
  user <- "author <author@example.com>"
  gert::git_commit(message, author = user, committer = user, repo = path)
}


create_new_branch <- function(path, branch_name = "other") {
  initial_branch <- gert::git_branch(repo = path)
  gert::git_branch_create(branch_name, repo = path)
  commit_sha <- create_new_commit(path, branch_name)
  gert::git_branch_checkout(initial_branch, repo = path)
  list(branch = branch_name, sha = commit_sha)
}
