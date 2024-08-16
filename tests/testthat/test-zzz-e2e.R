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
  expect_type(get_task_result(dat$data$job_id, queue$controller), "character")
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
  expect_type(get_task_result(dat$data$job_id, queue$controller), "character")
})

test_that("retruns error when getting status of run with invalid job_id", {
  res <- bg$request(
    "GET",
    sprintf("/report/status/bad_job_id")
  )
  
  errors <- httr::content(res)$errors
  expect_equal(httr::status_code(res), 400)
  expect_equal(errors[[1]]$detail, "Job ID does not exist")
})
test_that("can get status of report run", {
  # run task and wait for finish before getting status
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
  task_times <- wait_for_task_complete(job_id, queue$controller, 3)

  res <- bg$request(
    "GET",
    sprintf("/report/status/%s", job_id)
  )
  dat <- httr::content(res)$data
  task_times <- get_task_times(job_id, queue$controller)
  expect_equal(httr::status_code(res), 200)
  expect_equal(dat$status, "COMPLETE")
  expect_null(dat$queue_positionc)
  expect_equal(dat$packet_id, get_task_result(job_id, queue$controller))
  expect_equal(task_times[1], dat$time_queued)
  expect_equal(task_times[2], dat$time_started)
  expect_equal(task_times[3], dat$time_complete)
  expect_equal(get_task_logs(job_id, queue$controller), unlist(dat$logs))
})

