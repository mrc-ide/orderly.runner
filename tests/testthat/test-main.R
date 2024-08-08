test_that("Can parse arguments (server)", {
  expect_mapequal(parse_main("path"),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8001,
                       host = "0.0.0.0",
                       path = "path"))
  expect_mapequal(parse_main(c("--port=8080", "path")),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8080,
                       host = "0.0.0.0",
                       path = "path"))
  expect_mapequal(parse_main(c("--port=8080", "--validate", "path")),
                  list(log_level = "info",
                       validate = TRUE,
                       port = 8080,
                       host = "0.0.0.0",
                       path = "path"))
  expect_mapequal(parse_main(c("--log-level=debug", "--validate", "path")),
                  list(log_level = "debug",
                       validate = TRUE,
                       port = 8001,
                       host = "0.0.0.0",
                       path = "path"))
  expect_mapequal(
    parse_main(c("--host=host", "--log-level=debug", "--validate", "path")),
    list(log_level = "debug",
         validate = TRUE,
         port = 8001,
         host = "host",
         path = "path")
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
  expect_mapequal(parse_main_worker("path"),
                  list(path = "path"))
})

test_that("Can spawn workers", {
  skip_if_not_installed("mockery")

  mock_queue_new <- mockery::mock(
    list(controller = list(
      queue_id = "test_queue_id",
      con = "test_con"
    ))
  )
  mockery::stub(main_worker, "Queue$new", mock_queue_new)

  mock_loop <- mockery::mock()
  mock_rrq_worker_new <- mockery::mock(
    list(loop = mock_loop, id = "test_worker_id")
  )
  mockery::stub(main_worker, "rrq::rrq_worker$new", mock_rrq_worker_new)

  mock_dir_create <- mockery::mock()
  mockery::stub(main_worker, "fs::dir_create", mock_dir_create)

  mock_git_clone <- mockery::mock()
  mockery::stub(main_worker, "gert::git_clone", mock_git_clone)

  main_worker(c("path"))

  mockery::expect_called(mock_queue_new, 1)
  expect_equal(mockery::mock_args(mock_queue_new)[[1]],
               list("path"))

  mockery::expect_called(mock_rrq_worker_new, 1)
  expect_equal(mockery::mock_args(mock_rrq_worker_new)[[1]],
               list("test_queue_id", con = "test_con"))

  expected_worker_path <- file.path(
    "path", ".packit", "workers", "test_worker_id"
  )
  mockery::expect_called(mock_dir_create, 1)
  expect_equal(mockery::mock_args(mock_dir_create)[[1]],
               list(expected_worker_path))

  mockery::expect_called(mock_git_clone, 1)
  expect_equal(mockery::mock_args(mock_git_clone)[[1]],
               list("path", path = expected_worker_path))

  mockery::expect_called(mock_loop, 1)
})
