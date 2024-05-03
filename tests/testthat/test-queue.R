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


test_that("Errors if not git repo", {
  root <- create_temporary_root()
  expect_error(Queue$new(root), #nolint
               paste("Not starting server as orderly root",
                     "is not version controlled."))
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
  git_info <- helper_add_git(root, c("src", "orderly_config.yml"))

  q <- start_queue_with_workers(root, 1)

  task_id <- q$submit("data", branch = git_info$branch)
  expect_worker_task_complete(task_id, q$controller, 10)
})


test_that("Can submit 2 tasks on different branches", {
  skip_if_no_redis()

  root <- test_prepare_orderly_example("data")
  git_info <- helper_add_git(root, c("src", "orderly_config.yml"))

  gert::git_branch_create("branch", repo = root)
  gert::git_branch_checkout("branch", repo = root)
  create_new_commit(root, new_file = "test.txt", add = "test.txt")

  q <- start_queue_with_workers(root, 2)

  task_id1 <- q$submit("data", branch = git_info$branch)
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
  git_info <- helper_add_git(root, c("src", "orderly_config.yml"))
  sha2 <- create_new_commit(root, new_file = "test.txt", add = "test.txt")

  q <- start_queue_with_workers(root, 2)

  task_id1 <- q$submit("data", ref = git_info$sha, branch = git_info$branch)
  task_id2 <- q$submit("data", ref = sha2, branch = git_info$branch)
  expect_worker_task_complete(task_id1, q$controller, 10)
  expect_worker_task_complete(task_id2, q$controller, 10)

  worker_id2 <- rrq::rrq_task_info(task_id2, controller = q$controller)$worker
  worker2_txt <- file.path(root, ".packit", "workers", worker_id2, "test.txt")
  expect_equal(file.exists(worker2_txt), TRUE)
})
