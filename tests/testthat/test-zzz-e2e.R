skip_if_not_installed("httr")
skip_if_no_redis()

queue_id <- "orderly.runner:cuteasdanimal"
root <- test_prepare_orderly_remote_example(
  c("data", "parameters")
)
queue <- start_queue_with_workers(root$local, 1, queue_id = queue_id)
bg <- porcelain::porcelain_background$new(
  api, args = list(root$local),
  env = c(ORDERLY_RUNNER_QUEUE_ID = queue_id)
)
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
  r <- bg$request("GET", "/report/list?ref=HEAD")
  expect_equal(httr::status_code(r), 200)
  
  dat <- httr::content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  reports <- vcapply(dat$data, "[[", "name")
  expect_true(all(c("data", "parameters") %in% reports))
})


test_that("can get parameters", {
  r <- bg$request("GET", "/report/data/parameters?ref=HEAD")
  expect_equal(httr::status_code(r), 200)

  dat <- httr::content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_equal(dat$data, list())

  r <- bg$request("GET", "/report/parameters/parameters?ref=HEAD")
  expect_equal(httr::status_code(r), 200)

  dat <- httr::content(r)
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_equal(dat$data, list(
    list(name = "a", value = NULL),
    list(name = "b", value = "2"),
    list(name = "c", value = NULL)
  ))
})

test_that("can run report", {
  data <- list(
    name = "data",
    branch = gert::git_branch(repo = root$local),
    hash = gert::git_commit_id(repo = root$local),
    parameters = c(NULL)
  )

  body <- jsonlite::toJSON(data, null = "null", auto_unbox = TRUE)

  r <- bg$request(
    "POST", "/report/run",
    body = body,
    encode = "raw",
    httr::content_type("application/json")
  )

  expect_equal(httr::status_code(r), 200)
  dat <- httr::content(r)

  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_worker_task_complete(dat$data$job_id, queue$controller, 10)
})

test_that("can run report with params", {
  data <- list(
    name = "parameters",
    branch = gert::git_branch(repo = root$local),
    hash = gert::git_commit_id(repo = root$local),
    parameters = list(a = 1, c = 3)
  )

  body <- jsonlite::toJSON(data, null = "null", auto_unbox = TRUE)

  r <- bg$request(
    "POST", "/report/run",
    body = body,
    encode = "raw",
    httr::content_type("application/json")
  )

  expect_equal(httr::status_code(r), 200)
  dat <- httr::content(r)

  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_worker_task_complete(dat$data$job_id, queue$controller, 10)
})

test_that("can get status of report run", {
  # run report
  data <- list(
    name = "data",
    branch = gert::git_branch(repo = root$local),
    hash = gert::git_commit_id(repo = root$local),
    parameters = c(NULL)
  )
  r <- bg$request(
    "POST", "/report/run",
    body = jsonlite::toJSON(data, null = "null", auto_unbox = TRUE),
    encode = "raw",
    httr::content_type("application/json")
  )
  job_id <- httr::content(r)$data$job_id

  r <- bg$request(
    "GET",
    sprintf("/report/status/%s", job_id)
  )
})
