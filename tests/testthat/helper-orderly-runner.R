orderly_runner_endpoint <- function(
  method, path,
  root = NULL,
  repositories = NULL,
  validate = TRUE,
  skip_queue_creation = FALSE
) {
  if (skip_queue_creation) {
    queue <- NULL
  } else {
    queue <- Queue$new(root)
  }
  porcelain::porcelain_package_endpoint(
    "orderly.runner", method, path,
    state = list(root = root,
                 repositories_base_path = repositories,
                 queue = queue),
    validate = validate
  )
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(path, ...))
}


skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}


test_prepare_orderly_example <- function(examples, ..., env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  suppressMessages(orderly2::orderly_init(path, ...))
  copy_examples(examples, path)

  helper_add_git(path, orderly_gitignore = TRUE)
  path
}


test_prepare_orderly_remote_example <- function(examples, ..., env = parent.frame()) {
  path_remote <- test_prepare_orderly_example(examples, ..., env = env)
  path_local <- withr::local_tempdir(.local_envir = env)

  gert::git_clone(path_remote, path_local)
  orderly2::orderly_init(root = path_local, force = TRUE)
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


helper_add_git <- function(path, add = ".", orderly_gitignore = FALSE) {
  gert::git_init(path)
  if (orderly_gitignore) {
    orderly2::orderly_gitignore_update("(root)", root = path)
  }
  sha <- git_add_and_commit(path, add)
  branch <- gert::git_branch(repo = path)
  url <- "https://example.com/git"
  gert::git_remote_add(url, repo = path)
  list(path = path, branch = branch, sha = sha, url = url)
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
    gert::git_config_set("user.name", id, repo = worker_path)
    gert::git_config_set("user.email", id, repo = worker_path)
  })

}

start_queue_workers_quietly <- function(n_workers,
                                        controller, env = parent.frame()) {
  worker_manager <- suppressMessages(
    rrq::rrq_worker_spawn(n_workers, controller = controller)
  )
  withr::defer(rrq::rrq_worker_stop(controller = controller), env = env)
  worker_manager
}

start_queue_with_workers <- function(
  root, n_workers, env = parent.frame(), queue_id = NULL
) {
  q <- new_queue_quietly(root, queue_id = queue_id, logs_dir = tempfile())
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
  is_task_successful <- wait_for_task_complete(task_id, controller, n_tries)
  expect_true(is_task_successful)
}

wait_for_task_complete <- function(task_id, controller, n_tries) {
  rrq::rrq_task_wait(
    task_id, controller = controller, timeout = n_tries
  )
}

get_task_times <- function(task_id, controller) {
  rrq::rrq_task_times(task_id, controller = controller)
}

get_task_result <- function(task_id, controller) {
  rrq::rrq_task_result(task_id, controller = controller)
}

get_task_logs <- function(task_id, controller) {
  rrq::rrq_task_log(task_id, controller = controller)
}

initialise_git_repo <- function() {
  t <- tempfile()
  dir.create(t)
  writeLines(c("# Example", "", "example repo"), file.path(t, "README.md"))
  helper_add_git(t)
}


git_add_and_commit <- function(path, add = ".", message = "new commit") {
  gert::git_add(add, repo = path)
  user <- "author <author@example.com>"
  gert::git_commit(message, author = user, committer = user, repo = path)
}


git_add_and_commit <- function(path, add = ".", message = "new commit") {
  gert::git_add(add, repo = path)
  user <- "author <author@example.com>"
  gert::git_commit(message, author = user, committer = user, repo = path)
}


create_new_commit <- function(path, new_file = "new", add = ".", ...) {
  writeLines(ids::random_id(), file.path(path, new_file))
  git_add_and_commit(path, add, ...)
}


create_new_branch <- function(path, branch_name = "other", ...) {
  initial_branch <- gert::git_branch(repo = path)
  gert::git_branch_create(branch_name, repo = path)
  commit_sha <- create_new_commit(path, branch_name, ...)
  gert::git_branch_checkout(initial_branch, repo = path)
  list(branch = branch_name, sha = commit_sha)
}
