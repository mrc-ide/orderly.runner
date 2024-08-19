skip_if_not_installed("httr")
skip_if_no_redis()

queue_id <- "orderly.runner:cuteasdanimal"
root <- test_prepare_orderly_remote_example(
  c("data", "parameters")
)
queue <- start_queue_with_workers(root$local, 1, queue_id = queue_id)
bg <- porcelain::porcelain_background$new(
  api,
  args = list(root$local),
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
  expect_equal(
    dat$data$orderly2,
    package_version_string("orderly2")
  )
  expect_equal(
    dat$data$orderly.runner,
    package_version_string("orderly.runner")
  )
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

  expect_worker_task_complete(dat$data$taskId, queue$controller, 10)
  expect_type(get_task_result(dat$data$taskId, queue$controller), "character")
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
  expect_worker_task_complete(dat$data$taskId, queue$controller, 10)
  expect_type(get_task_result(dat$data$taskId, queue$controller), "character")
})

test_that("returns error when getting status when not passing in include_logs", {
  res <- bg$request(
    "POST",
    "/report/status",
    body = jsonlite::toJSON(c("invalid_taskId")),
    encode = "raw",
    httr::content_type("application/json")
  )

  errors <- httr::content(res)$errors
  expect_equal(httr::status_code(res), 400)
  expect_equal(errors[[1]]$detail, "query parameter 'include_logs' is missing but required")
})

test_that("can get status of report run with logs", {
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
  task_id <- httr::content(r)$data$taskId
  task_times <- wait_for_task_complete(task_id, queue$controller, 3)

  res <- bg$request(
    "POST",
    "/report/status?include_logs=TRUE",
    body = jsonlite::toJSON(c(task_id)),
    encode = "raw",
    httr::content_type("application/json")
  )
  dat <- httr::content(res)$data[[1]]
  task_times <- get_task_times(task_id, queue$controller)
  expect_equal(httr::status_code(res), 200)
  expect_equal(dat$status, "COMPLETE")
  expect_null(dat$queuePosition)
  expect_equal(dat$packetId, get_task_result(task_id, queue$controller))
  expect_equal(task_times[1], dat$timeQueued)
  expect_equal(task_times[2], dat$timeStarted)
  expect_equal(task_times[3], dat$timeComplete)
  expect_equal(get_task_logs(task_id, queue$controller), unlist(dat$logs))
})

test_that("can get status of multiple tasks without logs", {
  # run multiple tasks and wait for finish before getting status
  data <- list(
    name = "data",
    branch = gert::git_branch(repo = root$local),
    hash = gert::git_commit_id(repo = root$local),
    parameters = c(NULL)
  )
  r1 <- bg$request(
    "POST", "/report/run",
    body = jsonlite::toJSON(data, null = "null", auto_unbox = TRUE),
    encode = "raw",
    httr::content_type("application/json")
  )
  r2 <- bg$request(
    "POST", "/report/run",
    body = jsonlite::toJSON(data, null = "null", auto_unbox = TRUE),
    encode = "raw",
    httr::content_type("application/json")
  )
  task_ids <- c(httr::content(r1)$data$taskId, httr::content(r2)$data$taskId)
  task_times <- wait_for_task_complete(task_ids, queue$controller, 3)

  res <- bg$request(
    "POST",
    "/report/status?include_logs=FALSE",
    body = jsonlite::toJSON(task_ids),
    encode = "raw",
    httr::content_type("application/json")
  )
  dat <- httr::content(res)$data

  for (i in seq_along(task_ids)) {
    task_status <- dat[[i]]
    task_times <- get_task_times(task_ids[[i]], queue$controller)
    expect_equal(task_status$status, "COMPLETE")
    expect_null(task_status$queuePosition)
    expect_equal(task_status$packetId, get_task_result(task_ids[[i]], queue$controller))
    expect_equal(task_times[1], task_status$timeQueued)
    expect_equal(task_times[2], task_status$timeStarted)
    expect_equal(task_times[3], task_status$timeComplete)
    expect_null(task_status$logs)
  }
})

test_that("returns status of only job ids that exist", {
  # run report
  data <- list(
    name = "data",
    branch = gert::git_branch(repo = root$local),
    hash = gert::git_commit_id(repo = root$local),
    parameters = c(NULL)
  )
  r1 <- bg$request(
    "POST", "/report/run",
    body = jsonlite::toJSON(data, null = "null", auto_unbox = TRUE),
    encode = "raw",
    httr::content_type("application/json")
  )
  task_ids <- c(httr::content(r1)$data$taskId, "non-existant-id")
  
  res <- bg$request(
    "POST",
    "/report/status?include_logs=FALSE",
    body = jsonlite::toJSON(task_ids),
    encode = "raw",
    httr::content_type("application/json")
  )
  
  expect_equal(length(httr::content(res)$data) , 1)
})
