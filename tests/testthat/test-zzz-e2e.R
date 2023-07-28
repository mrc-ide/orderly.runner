test_that("can run server", {
  skip_if_not_installed("httr")
  root <- create_temporary_root(use_file_store = TRUE)
  bg <- porcelain::porcelain_background$new(api, list(root))
  bg$start()
  r <- bg$request("GET", "/")
  expect_equal(httr::status_code(r), 200)

  dat <- httr::content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_equal(dat$data$orderly2,
               package_version_string("orderly2"))
  expect_equal(dat$data$orderly.runner,
               package_version_string("orderly.runner"))
})
