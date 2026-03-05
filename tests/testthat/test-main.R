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
