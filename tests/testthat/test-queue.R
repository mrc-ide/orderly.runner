test_that("Can bring up queue", {
  skip_if_no_redis()

  root <- create_temporary_root(use_file_store = TRUE)
  gert::git_init(root)
  q <- new_queue_quietly(root)
  expect_equal(q$root, root)
  expect_equal(q$config$core$use_file_store, TRUE)
  expect_equal(q$controller, rrq::rrq_controller(q$controller$queue_id))

  expect_equal(q$number_of_workers(), 0)
  start_queue_workers_quietly(1, q$controller)
  expect_equal(q$number_of_workers(), 1)
})

test_that("creates directory for logs & adds to worker config", {
  skip_if_no_redis()

  root <- create_temporary_root(use_file_store = TRUE)
  gert::git_init(root)
  logs_dir <- tempfile()
  q <- new_queue_quietly(root, logs_dir = logs_dir)
  expect_true(dir.exists(logs_dir))
  expect_equal(logs_dir, rrq::rrq_worker_config_read("localhost", controller = q$controller)$logdir)
})

test_that("Errors if not git repo", {
  root <- create_temporary_root()
  expect_error(
    Queue$new(root), # nolint
    paste(
      "Not starting server as orderly root",
      "is not version controlled."
    )
  )
})


test_that("Can connect to existing queue with queue_id", {
  skip_if_no_redis()

  root <- create_temporary_root()
  gert::git_init(root)
  queue_id <- ids::random_id()
  q1 <- new_queue_quietly(root, queue_id = queue_id)
  start_queue_workers_quietly(1, q1$controller)
  q2 <- new_queue_quietly(root, queue_id = queue_id)
  expect_equal(q1$number_of_workers(), 1)
  expect_equal(q2$number_of_workers(), 1)
})


test_that("Uses ORDERLY_RUNNER_QUEUE_ID if it exists", {
  skip_if_no_redis()

  root <- create_temporary_root()
  gert::git_init(root)
  id <- ids::random_id()
  q <- withr::with_envvar(
    c(ORDERLY_RUNNER_QUEUE_ID = id),
    new_queue_quietly(root)
  )
  expect_equal(q$controller$queue_id, id)
})


test_that("Generated namespaced id if no ids exist", {
  skip_if_no_redis()

  root <- create_temporary_root()
  gert::git_init(root)
  q <- new_queue_quietly(root)
  expect_match(q$controller$queue_id, "orderly.runner")
})


test_that("Can submit task", {
  skip_if_no_redis()

  root <- test_prepare_orderly_example("data")

  q <- start_queue_with_workers(root, 1)

  task_id <- q$submit("data", branch = gert::git_branch(root))
  expect_worker_task_complete(task_id, q$controller, 10)
})


test_that("Can submit 2 tasks on different branches", {
  skip_if_no_redis()

  root <- test_prepare_orderly_example("data")

  gert::git_branch_create("branch", repo = root)
  gert::git_branch_checkout("branch", repo = root)
  create_new_commit(root, new_file = "test.txt", add = "test.txt")

  q <- start_queue_with_workers(root, 2)

  task_id1 <- q$submit("data", branch = "master")
  task_id2 <- q$submit("data", branch = "branch")
  expect_worker_task_complete(task_id1, q$controller, 10)
  expect_worker_task_complete(task_id2, q$controller, 10)

  worker_id2 <- rrq::rrq_task_info(task_id2, controller = q$controller)$worker
  worker2_txt <- file.path(root, ".packit", "workers", worker_id2, "test.txt")
  expect_equal(file.exists(worker2_txt), TRUE)
})


test_that("Can submit 2 tasks on different commit hashes", {
  skip_if_no_redis()

  root <- test_prepare_orderly_example("data")
  commit1 <- gert::git_commit_id(repo = root)
  commit2 <- create_new_commit(root, new_file = "test.txt", add = "test.txt")

  q <- start_queue_with_workers(root, 2)

  task_id1 <- q$submit("data", ref = commit1, branch = gert::git_branch(root))
  task_id2 <- q$submit("data", ref = commit2, branch = gert::git_branch(root))
  expect_worker_task_complete(task_id1, q$controller, 10)
  expect_worker_task_complete(task_id2, q$controller, 10)

  worker_id2 <- rrq::rrq_task_info(task_id2, controller = q$controller)$worker
  worker2_txt <- file.path(root, ".packit", "workers", worker_id2, "test.txt")
  expect_equal(file.exists(worker2_txt), TRUE)
})


test_that("can get statuses on complete report runs with logs", {
  skip_if_no_redis()
  root <- test_prepare_orderly_example("data")
  q <- start_queue_with_workers(root, 1)
  task_id1 <- q$submit("data", branch = gert::git_branch(root))
  task_id2 <- q$submit("data", branch = gert::git_branch(root))
  task_ids <- c(task_id1, task_id2)
  wait_for_task_complete(task_ids, q$controller, 5)

  statuses <- q$get_status(task_ids)

  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("COMPLETE"))
    expect_null(status$queuePosition)
    expect_equal(status$packetId, scalar(get_task_result(task_ids[[i]], q$controller)))
    expect_equal(status$logs, get_task_logs(task_ids[[i]], q$controller))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
  }
})

test_that("can get statuses wihtout logs if include_logs = false", {
  # run 2 reports
  skip_if_no_redis()
  root <- test_prepare_orderly_example("data")
  q <- start_queue_with_workers(root, 1)
  task_id1 <- q$submit("data", branch = gert::git_branch(root))
  task_id2 <- q$submit("data", branch = gert::git_branch(root))
  task_ids <- c(task_id1, task_id2)
  wait_for_task_complete(task_ids, q$controller, 5)

  statuses <- q$get_status(task_ids, FALSE)

  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("COMPLETE"))
    expect_null(status$queuePosition)
    expect_equal(status$packetId, scalar(get_task_result(task_ids[[i]], q$controller)))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
    expect_null(status$logs)
  }
})

test_that("can get status on pending report run", {
  # run 2 reports
  skip_if_no_redis()
  root <- test_prepare_orderly_example("data")
  q <- new_queue_quietly(root)
  task_id1 <- q$submit("data", branch = gert::git_branch(root))
  task_id2 <- q$submit("data", branch = gert::git_branch(root))
  task_ids <- c(task_id1, task_id2)

  statuses <- q$get_status(task_ids)

  for (i in seq_along(task_ids)) {
    status <- statuses[[i]]
    expect_equal(status$status, scalar("PENDING"))
    expect_equal(status$queuePosition, scalar(as.integer(i)))
    expect_equal(scalar(task_ids[[i]]), status$taskId)
    expect_null(status$packetId)
    expect_null(status$logs)
  }
})
