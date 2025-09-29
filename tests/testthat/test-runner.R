test_that("runner runs as expected", {
  upstream_git <- test_prepare_orderly_example("data")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  storage <- withr::local_tempdir()

  expect_false(fs::dir_exists(file.path(upstream_outpack, "archive")))

  sha <- gert::git_commit_id(repo = upstream_git)
  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage), suppressMessages({
    id <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = sha,
      reportname = "data",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  # Runner has pushed the result to the upstream store
  expect_true(fs::dir_exists(
    file.path(upstream_outpack, "archive", "data", id)))

  info <- orderly::orderly_metadata(id, root = upstream_outpack)$git
  expect_equal(info$branch, "master")
  expect_equal(info$sha, sha)
})


test_that("runner runs as expected with parameters", {
  upstream_git <- test_prepare_orderly_example("parameters")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  storage <- withr::local_tempdir()

  expect_false(fs::dir_exists(file.path(upstream_outpack, "archive")))

  parameters <- list(a = -1, b = -2, c = -3)
  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage), suppressMessages({
    id <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = gert::git_commit_id(repo = upstream_git),
      reportname = "parameters",
      parameters = parameters,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  report <- file.path(upstream_outpack, "archive", "parameters", id)
  output <- readRDS(file.path(report, "data.rds"))

  expect_equal(output, parameters)
})


test_that("runner cleans up after itself", {
  upstream_git <- test_prepare_orderly_example("data")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  storage <- withr::local_tempdir()

  expect_false(fs::dir_exists(file.path(upstream_outpack, "archive")))

  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage), suppressMessages({
    id <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = gert::git_commit_id(repo = upstream_git),
      reportname = "data",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  # The only things left in the storage after running should be the local clone
  # of the repository. The worktrees directory should be cleaned up and removed.
  expect_setequal(withr::with_dir(storage, fs::dir_ls()), c("git", "worktrees"))
  expect_length(fs::dir_ls(file.path(storage, "git")), 1)
  expect_length(fs::dir_ls(file.path(storage, "worktrees")), 0)
})


test_that("runner can use an old commit", {
  upstream_git <- test_prepare_orderly_example("data")
  commit1 <- gert::git_commit_id(repo = upstream_git)
  commit2 <- create_new_commit(upstream_git)

  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  storage <- withr::local_tempdir()

  expect_false(fs::dir_exists(file.path(upstream_outpack, "archive")))

  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage), suppressMessages({
    id1 <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = commit1,
      reportname = "data",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))
  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage), suppressMessages({
    id2 <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = commit2,
      reportname = "data",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  info1 <- orderly::orderly_metadata(id1, root = upstream_outpack)$git
  expect_equal(info1$branch, "master")
  expect_equal(info1$sha, commit1)

  info2 <- orderly::orderly_metadata(id2, root = upstream_outpack)$git
  expect_equal(info2$branch, "master")
  expect_equal(info2$sha, commit2)
})


test_that("runner can pull dependencies", {
  upstream_git <- test_prepare_orderly_example(c("data", "depends"))
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  sha <- gert::git_commit_id(repo = upstream_git)

  # We use two separate storage directories to make sure we aren't just reusing
  # the local packet somehow.
  storage1 <- withr::local_tempdir()
  storage2 <- withr::local_tempdir()

  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage1), suppressMessages({
    id1 <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = sha,
      reportname = "data",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = storage2), suppressMessages({
    id2 <- runner_run(
      url = upstream_git,
      branch = "master",
      ref = sha,
      reportname = "depends",
      parameters = NULL,
      location = list(type = "path", args = list(path = upstream_outpack)),
      echo = FALSE)
  }))

  # Runner has pushed the result to the upstream store
  expect_true(fs::dir_exists(
    file.path(upstream_outpack, "archive", "data", id1)))
  expect_true(fs::dir_exists(
    file.path(upstream_outpack, "archive", "depends", id2)))

  info <- orderly::orderly_metadata(id2, root = upstream_outpack)$depends
  expect_equal(info[1,]$packet, id1)
  expect_equal(info[1,]$query, 'latest(name == "data")')
})
