skip_if_not_installed("httr")
root <- test_prepare_orderly_example(c("data", "parameters"))
repo <- helper_add_git(root)
bg <- porcelain::porcelain_background$new(api, list(root))
bg$start()
on.exit(bg$stop())

test_that("can run server", {
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

test_that("can list reports", {
  r <- bg$request("GET", "/report/list?hash=HEAD")
  expect_equal(httr::status_code(r), 200)
  
  dat <- httr::content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  reports <- vcapply(dat$data, "[[", "name")
  expect_true(all(c("data", "parameters") %in% reports))
})
