test_that("Can bring up queue", {
  skip_if_no_redis()

  q <- Queue$new()

  expect_equal(q$number_of_workers(), 0)
  start_queue_workers(1, q$controller)
  expect_equal(q$number_of_workers(), 1)
})


test_that("creates directory for logs & adds to worker config", {
  skip_if_no_redis()

  logs_dir <- tempfile()
  q <- Queue$new(logs_dir = logs_dir)
  expect_true(fs::dir_exists(logs_dir))

  config <- rrq::rrq_worker_config_read("localhost", controller = q$controller)
  expect_equal(logs_dir, config$logdir)
})


test_that("Can connect to existing queue with queue_id", {
  skip_if_no_redis()

  queue_id <- ids::random_id()
  q1 <- Queue$new(queue_id = queue_id)
  start_queue_workers(1, q1$controller)
  q2 <- Queue$new(queue_id = queue_id)

  expect_equal(q1$number_of_workers(), 1)
  expect_equal(q2$number_of_workers(), 1)
})


test_that("Uses ORDERLY_RUNNER_QUEUE_ID if it exists", {
  skip_if_no_redis()

  id <- ids::random_id()
  q <- withr::with_envvar(c(ORDERLY_RUNNER_QUEUE_ID = id), {
    Queue$new()
  })
  expect_equal(q$controller$queue_id, id)
})


test_that("Generated namespaced id if no ids exist", {
  skip_if_no_redis()

  q <- Queue$new()
  expect_match(q$controller$queue_id, "orderly.runner")
})


test_that("Can submit task", {
  skip_if_no_redis()

  upstream_git <- test_prepare_orderly_example("data")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  q <- start_queue_with_workers(1)

  sha <- gert::git_commit_id(repo = upstream_git)
  task_id <- q$submit(
    url = upstream_git,
    branch = "master",
    ref = sha,
    reportname = "data",
    parameters = NULL,
    location = list(type = "path", args = list(path = upstream_outpack))
  )

  expect_worker_task_complete(task_id, q$controller, 10)
})


test_that("can get statuses on complete report runs with logs", {
  skip_if_no_redis()

  upstream_git <- test_prepare_orderly_example("data")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  q <- start_queue_with_workers(1)

  sha <- gert::git_commit_id(repo = upstream_git)
  location <- list(type = "path", args = list(path = upstream_outpack))
  task_id1 <- q$submit(
    url = upstream_git,
    branch = "master",
    ref = sha,
    reportname = "data",
    parameters = NULL,
    location = location
  )
  task_id2 <- q$submit(
    url = upstream_git,
    branch = "master",
    ref = sha,
    reportname = "data",
    parameters = NULL,
    location = location
  )

  task_ids <- c(task_id1, task_id2)
  wait_for_task_complete(task_ids, q$controller, 5)

  statuses <- q$get_status(task_ids)
  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("COMPLETE"))
    expect_null(status$queuePosition)
    expect_equal(status$packetId, scalar(get_task_result(task_ids[[i]], q$controller)))
    expect_equal(status$logs, get_task_logs(task_ids[[i]], q$controller))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
  }

  statuses <- q$get_status(task_ids, include_logs = FALSE)
  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("COMPLETE"))
    expect_null(status$queuePosition)
    expect_equal(status$packetId, scalar(get_task_result(task_ids[[i]], q$controller)))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
    expect_null(status$logs)
  }
})


test_that("can get status on pending report run", {
  skip_if_no_redis()

  upstream_git <- test_prepare_orderly_example("data")
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  q <- start_queue()

  sha <- gert::git_commit_id(repo = upstream_git)
  location <- list(type = "path", args = list(path = upstream_outpack))
  task_id1 <- q$submit(
    url = upstream_git,
    branch = "master",
    ref = sha,
    reportname = "data",
    parameters = NULL,
    location = location
  )
  task_id2 <- q$submit(
    url = upstream_git,
    branch = "master",
    ref = sha,
    reportname = "data",
    parameters = NULL,
    location = location
  )

  task_ids <- c(task_id1, task_id2)
  statuses <- q$get_status(task_ids)

  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("PENDING"))
    expect_equal(status$queuePosition, scalar(as.integer(i)))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
    expect_null(status$packetId)
    expect_null(status$logs)
  }
})
