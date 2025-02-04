create_api <- function(repositories = NULL,
                       log_level = "off",
                       ...,
                       env = parent.frame()) {
  if (is.null(repositories)) {
    repositories <- withr::local_tempdir(.local_envir = env)
  }

  api(repositories, validate = TRUE, log_level = log_level, ...)
}


expect_success <- function(res) {
  expect_equal(res$status, 200)
  invisible(jsonlite::fromJSON(res$body)$data)
}


skip_if_no_redis <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
  invisible(available)
}


skip_if_no_test_private_repo_ssh_key <- function() {
  ssh_key <- Sys.getenv("TEST_PRIVATE_REPO_SSH_KEY", unset = NA)
  if (is.na(ssh_key)) {
    testthat::skip("Skipping test as TEST_PRIVATE_REPO_SSH_KEY is not set")
  }

  list(
    ssh_key = ssh_key,
    url = "git@github.com:mrc-ide/orderly.runner-private-test-repo.git"
  )
}


empty_json_object <- function() {
  jsonlite::toJSON(empty_object())
}


create_temporary_root <- function(..., env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  suppressMessages(orderly2::orderly_init(path, ...))
}


test_prepare_orderly_example <- function(examples, ..., env = parent.frame()) {
  # We explicitly do not initialize this as an orderly location, to mirror the
  # fact that the source repository (eg. GitHub) is generally distinct from the
  # upstream outpack repository (eg. Packit).
  #
  # We do still need to create orderly_config.yml as the bare minimum source
  # tree.

  path <- withr::local_tempdir(.local_envir = env)
  writeLines('minimum_orderly_version: "1.99.0"',
             file.path(path, "orderly_config.yml"))

  copy_examples(examples, path)

  gert::git_init(path)
  git_add_and_commit(path, add = ".")

  path
}


copy_examples <- function(examples, path_src) {
  fs::dir_create(path_src)

  fs::dir_create(file.path(path_src, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(path_src, "src"))
  }
}


start_queue_workers <- function(n, controller, env = parent.frame()) {
  storage <- withr::local_tempdir(.local_envir = env)

  ids <- vcapply(seq(n), function(i) {
    withr::with_envvar(c(ORDERLY_WORKER_STORAGE = fs::dir_create(storage, i)), {
      w <- suppressMessages(rrq::rrq_worker_spawn(1, controller = controller))
      w$id
    })
  })
  withr::defer(rrq::rrq_worker_stop(ids, controller = controller), env = env)
}


start_queue <- function(..., env = parent.frame()) {
  logs_dir <- withr::local_tempdir(.local_envir = env)
  Queue$new(logs_dir = logs_dir, ...)
}


start_queue_with_workers <- function(n, ..., env = parent.frame()) {
  q <- start_queue(..., env = env)
  start_queue_workers(n, q$controller, env = env)
  q
}


expect_worker_task_complete <- function(task_id, controller, n_tries) {
  is_task_successful <- wait_for_task_complete(task_id, controller, n_tries)
  expect_true(is_task_successful)
  invisible(get_task_result(task_id, controller))
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

initialise_git_repo <- function(env = parent.frame()) {
  path <- gert::git_init(withr::local_tempdir(.local_envir = env))
  writeLines(c("# Example", "", "example repo"),
             file.path(path, "README.md"))
  git_add_and_commit(path, add = ".")
  path
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
