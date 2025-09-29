test_that("Can construct the api", {
  obj <- create_api(log_level = "info", skip_queue_creation = TRUE)

  result <- evaluate_promise({
    res <- obj$request("GET", "/")
  })
  expect_equal(res$status, 200)
  logs <- lapply(strsplit(result$output, "\n")[[1]], jsonlite::parse_json)
  expect_length(logs, 2)
  expect_equal(logs[[1]]$logger, "orderly.runner")
})


test_that("root data returns sensible data", {
  ## Just hello world for the package really
  obj <- create_api(skip_queue_creation = TRUE)

  res <- obj$request("GET", "/")
  data <- expect_success(res)

  expect_true(all(c("orderly", "orderly.runner") %in% names(data)))
  expect_match(unlist(data), "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})


test_that("can fetch repositories", {
  repositories <- withr::local_tempdir()
  obj <- create_api(repositories = repositories, skip_queue_creation = TRUE)

  upstream_a <- test_prepare_orderly_example("data")
  upstream_b <- test_prepare_orderly_example("data")

  res <- obj$request(
    "POST",
    "/repository/fetch",
    query = list(url = upstream_a),
    body = empty_json_object())

  expect_success(res)
  expect_length(fs::dir_ls(repositories), 1)

  res <- obj$request(
    "POST",
    "/repository/fetch",
    query = list(url = upstream_b),
    body = empty_json_object())

  expect_success(res)
  expect_length(fs::dir_ls(repositories), 2)
})


test_that("can fetch private repositories", {
  private_repo <- skip_if_no_test_private_repo_ssh_key()
  repositories <- withr::local_tempdir()
  obj <- create_api(repositories = repositories, skip_queue_creation = TRUE)

  res <- obj$request(
    "POST",
    "/repository/fetch",
    query = list(url = private_repo$url),
    body = jsonlite::toJSON(list(ssh_key = scalar(private_repo$ssh_key))))

  expect_success(res)
  expect_length(fs::dir_ls(repositories), 1)
})


test_that("can list branches in repository", {
  obj <- create_api(skip_queue_creation = TRUE)

  upstream <- test_prepare_orderly_example("data")

  # Start with just the initial master branch. Fetch the repo and list its
  # branches:
  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  data <- expect_success(res)

  res <- obj$request("GET", "/repository/branches",
                     query = list(url = upstream))
  data <- expect_success(res)

  expect_equal(data$default_branch, "master")
  expect_equal(nrow(data$branches), 1)
  expect_equal(data$branches[1,]$name, "master")
  expect_equal(data$branches[1,]$message, "new commit\n")


  # Now create a "new-branch" branch and add a commit to it. Fetch the repo
  # again and now the two branches should be listed:
  info <- create_new_branch(upstream, "new-branch", message = "start new branch")

  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  data <- expect_success(res)

  res <- obj$request("GET", "/repository/branches",
                     query = list(url = upstream))
  data <- expect_success(res)
  expect_setequal(data$branches$name, c("new-branch", "master"))

  b <- data$branches[data$branches$name == "new-branch",]
  expect_equal(b$commit, info$sha)
  expect_equal(b$message, "start new branch\n")

  # Finally delete the "new-branch". Fetch the repo and check that just the
  # master branch remains in the list.
  gert::git_branch_delete("new-branch", upstream)

  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  data <- expect_success(res)

  res <- obj$request("GET", "/repository/branches",
                     query = list(url = upstream))
  data <- expect_success(res)
  expect_equal(nrow(data$branches), 1)
  expect_equal(data$branches[1,]$name, "master")
})


test_that("listing branches fails if repository was not fetched", {
  obj <- create_api(skip_queue_creation = TRUE)

  upstream <- test_prepare_orderly_example("data")
  res <- obj$request("GET", "/repository/branches",
                     query = list(url = upstream))
  expect_equal(res$status, 404)
})


test_that("can list orderly reports", {
  obj <- create_api(skip_queue_creation = TRUE)

  upstream <- test_prepare_orderly_example(c("data", "parameters"))

  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  expect_success(res)

  res <- obj$request("GET", "/report/list", query = list(url = upstream,
                                                         ref = "master"))
  data <- expect_success(res)
  expect_setequal(data$name, c("data", "parameters"))

  expect_true(all(data$updatedTime > (Sys.time() - 100)))
  expect_false(all(data$hasModifications))

  ## Add a report on a 2nd branch
  gert::git_branch_create("other", repo = upstream)
  fs::dir_copy(
    file.path(upstream, "src", "parameters"),
    file.path(upstream, "src", "parameters2")
  )
  # have to rename to parameters2.R to recognise it as orderly report
  file.rename(
    file.path(upstream, "src", "parameters2", "parameters.R"),
    file.path(upstream, "src", "parameters2", "parameters2.R")
  )
  sha <- git_add_and_commit(upstream)

  ## Synchronize the local copy of the repository
  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  expect_success(res)

  ## Can list items from this sha
  res <- obj$request("GET", "/report/list", query = list(url = upstream,
                                                         ref = sha))
  other_data <- expect_success(res)
  params2 <- other_data[other_data$name == "parameters2", ]
  existing <- res$data[other_data$name != "parameters2", ]
  expect_equal(existing, res$data)
  expect_equal(nrow(params2), 1)
  expect_true(params2$hasModifications)

  ## We can still see all reports on main branch
  res <- obj$request("GET", "/report/list", query = list(url = upstream,
                                                         ref = "master"))
  again_data <- expect_success(res)
  expect_equal(again_data, data)
})


test_that("can get parameters for a report", {
  obj <- create_api(skip_queue_creation = TRUE)

  upstream <- test_prepare_orderly_example(c("data", "parameters"))

  res <- obj$request("POST", "/repository/fetch",
                     query = list(url = upstream),
                     body = empty_json_object())
  expect_success(res)

  res <- obj$request("GET", "/report/parameters",
                     query = list(url = upstream, ref = "HEAD", name = "data"))
  data <- expect_success(res)
  expect_equal(data, list())

  res <- obj$request("GET", "/report/parameters",
                     query = list(url = upstream, ref = "HEAD", name = "parameters"))
  data <- expect_success(res)
  expect_equal(data, data.frame(name = c("a", "b", "c"),
                                value = c(NA, 2, NA)))
})


test_that("can run orderly reports", {
  skip_if_no_redis()

  queue_id <- orderly_queue_id()
  controller <- rrq::rrq_controller(queue_id)

  obj <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    create_api())

  start_queue_workers(1, controller)

  upstream_git <- test_prepare_orderly_example(c("data", "parameters"))
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = upstream_git)),
    hash = scalar(gert::git_commit_id(repo = upstream_git)),
    parameters = scalar(NULL),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res <- obj$request("POST", "/report/run",
                     query = list(url = upstream_git),
                     body = jsonlite::toJSON(req))
  data <- expect_success(res)
  rrq::rrq_task_wait(data$taskId, controller = controller)
  expect_equal(
    rrq::rrq_task_status(data$taskId, controller = controller),
    "COMPLETE"
  )

  req <- list(
    name = scalar("parameters"),
    branch = scalar(gert::git_branch(repo = upstream_git)),
    hash = scalar(gert::git_commit_id(repo = upstream_git)),
    parameters = list(a = scalar(1), c = scalar(3)),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res <- obj$request("POST", "/report/run",
                     query = list(url = upstream_git),
                     body = jsonlite::toJSON(req))
  data <- expect_success(res)
  rrq::rrq_task_wait(data$taskId, controller = controller)
  expect_equal(
    rrq::rrq_task_status(data$taskId, controller = controller),
    "COMPLETE"
  )
})

test_that("can run orderly reports from private repositories", {
  skip_if_no_redis()
  private_repo <- skip_if_no_test_private_repo_ssh_key()

  queue_id <- orderly_queue_id()
  controller <- rrq::rrq_controller(queue_id)

  obj <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    create_api())

  start_queue_workers(1, controller)

  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  req <- list(
    ssh_key = scalar(private_repo$ssh_key),
    name = scalar("data"),
    branch = scalar("main"),
    hash = scalar("ee8858c40d025fe658238c9ff9c4e3d6b683f6d9"),
    parameters = scalar(NULL),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res <- obj$request("POST", "/report/run",
                     query = list(url = private_repo$url),
                     body = jsonlite::toJSON(req))
  data <- expect_success(res)
  rrq::rrq_task_wait(data$taskId, controller = controller)
  expect_equal(
    rrq::rrq_task_status(data$taskId, controller = controller),
    "COMPLETE"
  )
})

test_that("can get statuses of jobs", {
  # run 2 jobs first and wait for finish
  skip_if_no_redis()
  queue_id <- orderly_queue_id()
  controller <- rrq::rrq_controller(queue_id)

  obj <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    create_api())

  start_queue_workers(1, controller)

  upstream_git <- test_prepare_orderly_example(c("data", "parameters"))
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = upstream_git)),
    hash = scalar(gert::git_commit_id(repo = upstream_git)),
    parameters = scalar(NULL),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res1 <- obj$request("POST", "/report/run",
                      query = list(url = upstream_git),
                      body = jsonlite::toJSON(req))
  res2 <- obj$request("POST", "/report/run",
                      query = list(url = upstream_git),
                      body = jsonlite::toJSON(req))

  dat1 <- expect_success(res1)
  dat2 <- expect_success(res2)
  task_ids <- c(dat1$taskId, dat2$taskId)
  rrq::rrq_task_wait(task_ids, controller = controller)

  # status endpoint
  res <- obj$request("POST", "/report/status",
                     body = jsonlite::toJSON(task_ids),
                     query = list(include_logs = TRUE))
  statuses <- expect_success(res)$statuses

  for (i in seq_along(task_ids)) {
    task_status <- statuses[i,]
    task_times <- get_task_times(task_ids[[i]], controller)
    expect_equal(task_status$status, "COMPLETE")
    expect_true(is.na(task_status$queuePosition))
    expect_equal(task_status$packetId, get_task_result(task_ids[[i]], controller))
    expect_equal(task_times[1], task_status$timeQueued)
    expect_equal(task_times[2], task_status$timeStarted)
    expect_equal(task_times[3], task_status$timeComplete)
    expect_equal(get_task_logs(task_ids[[i]], controller), unlist(task_status$logs))
    expect_equal(task_ids[[i]], task_status$taskId)
  }
})


test_that("errors are included in task logs", {
  skip_if_no_redis()
  queue_id <- orderly_queue_id()
  controller <- rrq::rrq_controller(queue_id)

  obj <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    create_api())

  start_queue_workers(1, controller)

  upstream_git <- test_prepare_orderly_example("data")
  writeLines("stop('Oh no!')",
             file.path(upstream_git, "src", "data", "data.R"))
  git_add_and_commit(upstream_git)

  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = upstream_git)),
    hash = scalar(gert::git_commit_id(repo = upstream_git)),
    parameters = scalar(NULL),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res <- obj$request("POST",
                     "/report/run",
                     query = list(url = upstream_git),
                     body = jsonlite::toJSON(req))
  dat <- expect_success(res)

  rrq::rrq_task_wait(dat$taskId, controller = controller)

  res <- obj$request("POST", "/report/status",
                     body = jsonlite::toJSON(dat$taskId),
                     query = list(include_logs = TRUE))

  status <- expect_success(res)$statuses[1, ]

  expect_equal(status$status, "ERROR")
  expect_contains(unlist(status$logs),
                  c("! Failed to run report",
                    "Caused by error:",
                    "! Oh no!"))
})


test_that("can cancel a run", {
  skip_if_no_redis()

  queue_id <- orderly_queue_id()
  controller <- rrq::rrq_controller(queue_id)

  obj <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = queue_id),
    create_api())

  ## start_queue_workers(1, controller)

  upstream_git <- test_prepare_orderly_example(c("data", "parameters"))
  upstream_outpack <- create_temporary_root(use_file_store = TRUE)

  req <- list(
    name = scalar("data"),
    branch = scalar(gert::git_branch(repo = upstream_git)),
    hash = scalar(gert::git_commit_id(repo = upstream_git)),
    parameters = scalar(NULL),
    location = list(
      type = scalar("path"),
      args = list(
        path = scalar(upstream_outpack)
      )
    )
  )

  res <- obj$request("POST", "/report/run",
                     query = list(url = upstream_git),
                     body = jsonlite::toJSON(req))
  data <- expect_success(res)
  task_id <- data$taskId
  expect_equal(
    rrq::rrq_task_status(task_id, controller = controller),
    "PENDING"
  )

  res <- obj$request("POST", paste0("/report/cancel/", task_id))
  data <- expect_success(res)
  expect_null(data)

  expect_equal(
    rrq::rrq_task_status(task_id, controller = controller),
    "CANCELLED"
  )

  res <- obj$request("POST", paste0("/report/cancel/", task_id))
  expect_equal(res$status, 400)
  expect_equal(
    jsonlite::fromJSON(res$body)$errors$detail,
    sprintf("Task %s is not cancelable (CANCELLED)", task_id))
})
