test_that("Can bring up queue", {
  skip_if_no_redis()

  root <- create_temporary_root(use_file_store = TRUE)
  gert::git_init(root)
  q <- new_queue_quietly(root)
  expect_equal(q$root, root)
  expect_equal(q$config$core$use_file_store, TRUE)
  expect_equal(q$controller, rrq::rrq_controller2(q$controller$queue_id))

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
  gert::git_init(root)
  gert::git_add(c("src", "orderly_config.yml"), repo = root)
  gert::git_commit("first commit", repo = root)

  q <- new_queue_quietly(root)
  worker_manager <- start_queue_workers_quietly(1, q$controller)
  make_worker_dirs(root, worker_manager$id)

  task_id <- q$submit("data")
  browser()
})
