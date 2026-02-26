test_that("Can parse arguments (server)", {
  expect_mapequal(parse_main(c("path")),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8001,
                       host = "0.0.0.0",
                       repositories = "path"))
  expect_mapequal(parse_main(c("--port=8080", "path")),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8080,
                       host = "0.0.0.0",
                       repositories = "path"))
  expect_mapequal(parse_main(c("--port=8080", "--validate", "path")),
                  list(log_level = "info",
                       validate = TRUE,
                       port = 8080,
                       host = "0.0.0.0",
                       repositories = "path"))
  expect_mapequal(parse_main(c("--log-level=debug", "--validate", "path")),
                  list(log_level = "debug",
                       validate = TRUE,
                       port = 8001,
                       host = "0.0.0.0",
                       repositories = "path"))
  expect_mapequal(
    parse_main(c("--host=host", "--log-level=debug", "--validate", "path")),
    list(log_level = "debug",
         validate = TRUE,
         port = 8001,
         host = "host",
         repositories = "path")
  )
})


test_that("Can construct api", {
  skip_if_not_installed("mockery")
  mock_run <- mockery::mock()
  mock_api <- mockery::mock(list(run = mock_run))
  mockery::stub(main, "api", mock_api)
  main(c("--host=my-host", "--log-level=debug", "path"))

  mockery::expect_called(mock_api, 1)
  expect_equal(mockery::mock_args(mock_api)[[1]],
               list("path", FALSE, "debug"))

  mockery::expect_called(mock_run, 1)
  expect_equal(mockery::mock_args(mock_run)[[1]],
               list(host = "my-host", port = 8001))
})

test_that("Can parse arguments (worker)", {
  expect_mapequal(parse_main_worker("path"), list(path = "path"))
})


test_that("Can parse arguments (task_status)", {
  withr::with_envvar(c(ORDERLY_RUNNER_QUEUE_ID = ""), {
    expect_mapequal(parse_main_task_status(c("some-task-id")),
                    list(task_id = "some-task-id",
                         queue_id = ""))
    expect_mapequal(parse_main_task_status(c("--queue-id=my-queue", "some-task-id")),
                    list(task_id = "some-task-id",
                         queue_id = "my-queue"))
  })
  withr::with_envvar(c(ORDERLY_RUNNER_QUEUE_ID = "env-queue"), {
    expect_mapequal(parse_main_task_status(c("some-task-id")),
                    list(task_id = "some-task-id",
                         queue_id = "env-queue"))
  })
})


test_that("main_task_status errors if no queue ID", {
  withr::with_envvar(c(ORDERLY_RUNNER_QUEUE_ID = ""), {
    expect_error(main_task_status(c("some-task-id")),
                 "Queue ID must be provided")
  })
})


test_that("main_task_status prints status for a task", {
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

  output <- capture.output(
    withr::with_envvar(
      c(ORDERLY_RUNNER_QUEUE_ID = q$controller$queue_id),
      main_task_status(c(task_id))
    )
  )

  expect_match(output, sprintf("Task ID:.*%s", task_id), all = FALSE)
  expect_match(output, "Status:.*COMPLETE", all = FALSE)
  expect_match(output, "Packet ID:", all = FALSE)
})


test_that("main_task_status errors if task does not exist", {
  skip_if_no_redis()

  q <- start_queue()

  withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = q$controller$queue_id),
    expect_error(main_task_status(c("nonexistent-task-id")),
                 "does not exist in queue")
  )
})
