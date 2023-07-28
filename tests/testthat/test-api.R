test_that("root data returns sensible, validated, data", {
  ## Just hello world for the package really
  endpoint <- orderly_runner_endpoint("GET", "/", NULL)
  res <- endpoint$run()
  expect_true(res$validated)
  expect_true(all(c("orderly2", "orderly.runner") %in%
                  names(res$data)))
  expect_match(unlist(res$data), "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})


test_that("Can construct the api", {
  root <- create_temporary_root(use_file_store = TRUE)
  obj <- api(root)
  result <- evaluate_promise(value <- obj$request("GET", "/")$status)
  expect_equal(value, 200)
  logs <- lapply(strsplit(result$output, "\n")[[1]], jsonlite::parse_json)
  expect_length(logs, 2)
  expect_equal(logs[[1]]$logger, "orderly.runner")
})
