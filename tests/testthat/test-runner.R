test_that("runner runs as expected", {
  orderly_root <- test_prepare_orderly_example("data")

  worker_id <- ids::adjective_animal()
  make_worker_dirs(orderly_root, worker_id)
  worker_root <- file.path(orderly_root, ".packit", "workers", worker_id)

  suppressMessages(withr::with_envvar(
    c(RRQ_WORKER_ID = worker_id),
    runner_run(orderly_root, "data", NULL,
               gert::git_branch(orderly_root),
               "HEAD", echo = FALSE)
  ))

  # report has been run with data in archive
  expect_equal(length(list.files(file.path(orderly_root, "archive"))), 1)
  # cleanup has deleted draft folder
  expect_equal(file.exists(file.path(worker_root, "draft")), FALSE)
})

test_that("runner runs as expected with parameters", {
  orderly_root <- test_prepare_orderly_example("parameters")

  worker_id <- ids::adjective_animal()
  make_worker_dirs(orderly_root, worker_id)
  worker_root <- file.path(orderly_root, ".packit", "workers", worker_id)

  parameters <- list(a = -1, b = -2, c = -3)
  suppressMessages(withr::with_envvar(
    c(RRQ_WORKER_ID = worker_id),
    runner_run(orderly_root, "parameters", parameters,
               gert::git_branch(orderly_root),
               "HEAD", echo = FALSE)
  ))

  report_archive <- file.path(orderly_root, "archive", "parameters")
  rds_path <- file.path(report_archive, list.files(report_archive), "data.rds")
  output <- readRDS(rds_path)

  expect_equal(output, parameters)
  expect_equal(file.exists(file.path(worker_root, "draft")), FALSE)
})

test_that("git clean clears unnecessary files", {
  # git-clean.R spawns a file in draft folder and one in worker root folder
  # and there will also be an empty folder draft/git-clean so we test
  # all components of git_clean
  orderly_root <- test_prepare_orderly_example("git-clean")

  worker_id <- ids::adjective_animal()
  make_worker_dirs(orderly_root, worker_id)
  worker_root <- file.path(orderly_root, ".packit", "workers", worker_id)

  suppressMessages(withr::with_envvar(
    c(RRQ_WORKER_ID = worker_id),
    runner_run(orderly_root, "git-clean", NULL,
               gert::git_branch(orderly_root),
               "HEAD", echo = FALSE)
  ))

  expect_equal(length(list.files(file.path(orderly_root, "archive"))), 1)
  expect_equal(file.exists(file.path(worker_root, "draft")), FALSE)
  expect_equal(file.exists(file.path(worker_root, "outside_draft.txt")), FALSE)
})
