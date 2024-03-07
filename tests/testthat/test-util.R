test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("first_dirname gets the first dir part of the filename", {
  expect_equal(
    first_dirname(c("test/file/name.txt", "test", ".", "testing/file.txt")),
    c("test", "test", ".", "testing"))
})


test_that("sys_which", {
  prog <- "a-path-that-does-not-exist"
  expect_error(sys_which(prog),
               "Did not find 'a-path-that-does-not-exist'")
})