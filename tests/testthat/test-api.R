test_that("root data returns sensible, validated, data", {
  ## Just hello world for the package really
  endpoint <- orderly_runner_endpoint("GET", "/", NULL,
    skip_queue_creation = TRUE
  )
  res <- endpoint$run()
  expect_true(res$validated)
  expect_true(all(c("orderly2", "orderly.runner") %in%
    names(res$data)))
  expect_match(unlist(res$data), "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})


test_that("Can construct the api", {
  root <- create_temporary_root(use_file_store = TRUE)
  obj <- api(root, skip_queue_creation = TRUE)
  result <- evaluate_promise(value <- obj$request("GET", "/")$status)
  expect_equal(value, 200)
  logs <- lapply(strsplit(result$output, "\n")[[1]], jsonlite::parse_json)
  expect_length(logs, 2)
  expect_equal(logs[[1]]$logger, "orderly.runner")
})


test_that("can list orderly reports", {
  repo <- test_prepare_orderly_remote_example(c("data", "parameters"))
  endpoint <- orderly_runner_endpoint(
    "GET", "/report/list",
    repo$local,
    skip_queue_creation = TRUE
  )

  res <- endpoint$run(gert::git_branch(repo$local))
  expect_equal(res$status_code, 200)
  expect_setequal(res$data$name, c("data", "parameters"))
  expect_true(all(res$data$updated_time > (Sys.time() - 100)))
  expect_false(all(res$data$has_modifications))

  ## Add a report on a 2nd branch
  gert::git_branch_create("other", repo = repo$local)
  fs::dir_copy(
    file.path(repo$local, "src", "parameters"),
    file.path(repo$local, "src", "parameters2")
  )
  # have to rename to parameters2.R to recognise it as orderly report
  file.rename(
    file.path(repo$local, "src", "parameters2", "parameters.R"),
    file.path(repo$local, "src", "parameters2", "parameters2.R")
  )
  sha <- git_add_and_commit(repo$local)

  ## Can list items from this sha
  other_res <- endpoint$run(sha)
  expect_equal(other_res$status_code, 200)
  params2 <- other_res$data[other_res$data$name == "parameters2", ]
  existing <- other_res$data[other_res$data$name != "parameters2", ]
  expect_equal(existing, res$data)
  expect_equal(nrow(params2), 1)
  expect_true(params2$has_modifications)

  ## We can still see all reports on main branch
  commits <- gert::git_log(repo = repo$local)$commit
  first_commit <- commits[length(commits)]
  first_commit_res <- endpoint$run(first_commit)
  expect_equal(first_commit_res$status_code, 200)
  expect_equal(first_commit_res$data, res$data)
})


test_that("can get parameters for a report", {
  repo <- test_prepare_orderly_remote_example(c("data", "parameters"))
  endpoint <- orderly_runner_endpoint(
    "GET", "/report/<name:string>/parameters",
    repo$local,
    skip_queue_creation = TRUE
  )

  res <- endpoint$run("HEAD", "data")
  expect_equal(res$status_code, 200)
  expect_equal(res$data, list())

  res <- endpoint$run("HEAD", "parameters")
  expect_equal(res$status_code, 200)
  expect_equal(res$data, list(
    list(name = scalar("a"), value = NULL),
    list(name = scalar("b"), value = scalar("2")),
    list(name = scalar("c"), value = NULL)
  ))
})

test_that("can run orderly reports", {
  skip_if_no_redis()

  queue_id <- "orderly.runner:cute-animal"
  repo <- test_prepare_orderly_example(c("data", "parameters"))
  gert::git_init(repo)
  orderly2::orderly_gitignore_update("(root)", root = repo)
  git_add_and_commit(repo)
  queue <- Queue$new(repo, queue_id = queue_id, logs_dir = tempfile())
  worker_manager <- start_queue_workers_quietly(
    1, queue$controller
  )
  make_worker_dirs(repo, worker_manager$id)

  endpoint <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    orderly_runner_endpoint("POST", "/report/run", repo)
  )

  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = repo)),
    hash = scalar(gert::git_commit_id(repo = repo)),
    parameters = scalar(NULL)
  )

  res <- endpoint$run(jsonlite::toJSON(req))
  rrq::rrq_task_wait(res$data$task_id, controller = queue$controller)
  expect_equal(
    rrq::rrq_task_status(res$data$task_id, controller = queue$controller),
    "COMPLETE"
  )

  req <- list(
    name = scalar("parameters"),
    branch = scalar(gert::git_branch(repo = repo)),
    hash = scalar(gert::git_commit_id(repo = repo)),
    parameters = list(a = scalar(1), c = scalar(3))
  )

  res <- endpoint$run(jsonlite::toJSON(req))
  rrq::rrq_task_wait(res$data$task_id, controller = queue$controller)
  expect_equal(
    rrq::rrq_task_status(res$data$task_id, controller = queue$controller),
    "COMPLETE"
  )
})

test_that("can get statuses of jobs", {
  # run 2 jobs first and wait for finish
  skip_if_no_redis()
  queue_id <- "orderly.runner:bad-animal"
  repo <- test_prepare_orderly_example(c("data", "parameters"))
  gert::git_init(repo)
  orderly2::orderly_gitignore_update("(root)", root = repo)
  git_add_and_commit(repo)
  queue <- Queue$new(repo, queue_id = queue_id)
  worker_manager <- start_queue_workers_quietly(
    1, queue$controller
  )
  make_worker_dirs(repo, worker_manager$id)
  endpoint <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    orderly_runner_endpoint("POST", "/report/run", repo)
  )
  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = repo)),
    hash = scalar(gert::git_commit_id(repo = repo)),
    parameters = scalar(NULL)
  )
  dat1 <- endpoint$run(jsonlite::toJSON(req))
  dat2 <- endpoint$run(jsonlite::toJSON(req))
  job_ids <- c(dat1$data$job_id, dat2$data$job_id)
  rrq::rrq_task_wait(job_ids, controller = queue$controller)

  # status endpoint
  endpoint <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    orderly_runner_endpoint("POST", "/report/status", repo)
  )
  dat <- endpoint$run(TRUE, jsonlite::toJSON(job_ids))$data
  
  for (i in seq_along(job_ids)) {
    task_status <- dat[[i]]
    task_times <- get_task_times(job_ids[[i]], queue$controller)
    expect_equal(task_status$status, scalar("COMPLETE"))
    expect_null(scalar(task_status$queue_position))
    expect_equal(task_status$packet_id, scalar(get_task_result(job_ids[[i]], queue$controller)))
    expect_equal(scalar(task_times[1]), task_status$time_queued)
    expect_equal(scalar(task_times[2]), task_status$time_started)
    expect_equal(scalar(task_times[3]), task_status$time_complete)
    expect_equal(get_task_logs(job_ids[[i]], queue$controller), unlist(task_status$logs))
  }
})
