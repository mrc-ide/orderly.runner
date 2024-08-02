skip_if_not_installed("httr")
skip_if_not_installed("httr2")
skip_if_no_redis()

root <- test_prepare_orderly_remote_example(
  c("data", "parameters")
)
queue_id <- "orderly.runner:cute-animal"
queue <- Queue$new(root$local, queue_id = queue_id)
worker_manager <- start_queue_workers_quietly(1, queue$controller)
make_worker_dirs(root$local, worker_manager$id)

bg <- withr::with_envvar(
  c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
  porcelain::porcelain_background$new(api, list(root$local))
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
    parameters = NULL
  )

  r <- httr2::request(bg$url("/report/run")) |>
    httr2::req_body_json(list(data = data)) |>
    httr2::req_perform()

  expect_equal(r$status_code, 200)

  dat <- r |> httr2::resp_body_json()
  expect_equal(dat$status, "success")
  expect_null(dat$errors)
  expect_equal(is.character(dat$data$job_id), TRUE)
})
