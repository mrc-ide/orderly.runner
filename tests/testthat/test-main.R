test_that("Can parse arguments", {
  expect_mapequal(parse_main("path"),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8001,
                       path = "path"))
  expect_mapequal(parse_main(c("--port=8080", "path")),
                  list(log_level = "info",
                       validate = FALSE,
                       port = 8080,
                       path = "path"))
  expect_mapequal(parse_main(c("--port=8080", "--validate", "path")),
                  list(log_level = "info",
                       validate = TRUE,
                       port = 8080,
                       path = "path"))
  expect_mapequal(parse_main(c("--log-level=debug", "--validate", "path")),
                  list(log_level = "debug",
                       validate = TRUE,
                       port = 8001,
                       path = "path"))
})


test_that("Can construct api", {
  skip_if_not_installed("mockery")
  mock_run <- mockery::mock()
  mock_api <- mockery::mock(list(run = mock_run))
  mockery::stub(main, "api", mock_api)
  main(c("--log-level=debug", "path"))

  mockery::expect_called(mock_api, 1)
  expect_equal(mockery::mock_args(mock_api)[[1]],
               list("path", FALSE, "debug"))

  mockery::expect_called(mock_run, 1)
  expect_equal(mockery::mock_args(mock_run)[[1]], list("0.0.0.0", port = 8001))
})
