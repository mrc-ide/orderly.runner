test_that("Can bring up queue", {
  skip_if_no_redis()

  root <- create_temporary_root(use_file_store = TRUE)
  gert::git_init(root)
  q <- new_queue_quietly(root)
  expect_equal(q$root, root)
  expect_equal(q$identity, NULL)
  expect_equal(q$number_of_workers(), 1)
  expect_equal(q$config$core$use_file_store, TRUE)
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
  q1 <- new_queue_quietly(root, queue_id = queue_id, workers = 2)
  q2 <- new_queue_quietly(root, queue_id = queue_id, workers = 1)
  expect_equal(q1$number_of_workers(), 3)
  expect_equal(q2$number_of_workers(), 3)
})


test_that("Uses ORDERLY_SERVER_QUEUE_ID if it exists", {
  skip_if_no_redis()

  root <- create_temporary_root()
  gert::git_init(root)
  id <- ids::random_id()
  q <- withr::with_envvar(
    c(ORDERLY_SERVER_QUEUE_ID = id),
    new_queue_quietly(root)
  )
  expect_equal(q$.__enclos_env__$private$queue_id, id)
})


test_that("Generated namespaced id if no ids exist", {
  skip_if_no_redis()

  root <- create_temporary_root()
  gert::git_init(root)
  q <- new_queue_quietly(root)
  expect_match(q$.__enclos_env__$private$queue_id, "orderly.runner")
})
