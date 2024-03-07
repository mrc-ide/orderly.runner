test_that("runner runs as expected", {
  orderly_root <- test_prepare_orderly_example("data")
  gert::git_init(orderly_root)
  gert::git_add(c("src", "orderly_config.yml"), repo = orderly_root)
  gert::git_commit("first commit", repo = orderly_root)

  worker_id <- "worker1"
  make_worker_dirs(orderly_root, worker_id)
  worker_root <- file.path(orderly_root, ".packit", "workers", worker_id)

  suppressMessages(withr::with_envvar(
    c(RRQ_WORKER_ID = worker_id),
    runner_run(orderly_root, "data", NULL, "master", "HEAD", echo = FALSE)
  ))

  # report has been run with data in archive
  expect_equal(length(list.files(file.path(orderly_root, "archive"))), 1)
  # cleanup has deleted draft folder
  expect_equal(file.exists(file.path(worker_root, "draft")), FALSE)
  browser()
})

test_that("runner runs as expected with parameters", {
  orderly_root <- test_prepare_orderly_example("parameters")
  gert::git_init(orderly_root)
  gert::git_add(c("src", "orderly_config.yml"), repo = orderly_root)
  gert::git_commit("first commit", repo = orderly_root)

  worker_id <- "worker1"
  make_worker_dirs(orderly_root, worker_id)
  worker_root <- file.path(orderly_root, ".packit", "workers", worker_id)

  parameters <- list(a = -1, b = -2, c = -3)
  suppressMessages(withr::with_envvar(
    c(RRQ_WORKER_ID = worker_id),
    runner_run(orderly_root, "parameters", parameters,
               "master", "HEAD", echo = FALSE)
  ))

  report_archive <- file.path(orderly_root, "archive", "parameters")
  rds_path <- file.path(report_archive, list.files(report_archive), "data.rds")
  output <- readRDS(rds_path)

  expect_equal(output, parameters)
  expect_equal(file.exists(file.path(worker_root, "draft")), FALSE)
})
