test_that("can do depth 1 clone", {
  repo <- initialise_git_repo()
  
  first_commit <- repo$sha
  second_commit <- create_new_commit(repo$path)
  
  first_clone <- git_clone_depth_1(repo$path, first_commit)
  log <- gert::git_log(repo = first_clone)
  expect_true(nrow(log) == 1)
  expect_equal(log$commit, first_commit)
  
  second_clone <- git_clone_depth_1(repo$path, second_commit)
  log <- gert::git_log(repo = second_clone)
  expect_true(nrow(log) == 1)
  expect_equal(log$commit, second_commit)
})


test_that("git ref to sha", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  other <- create_new_branch(repo$path, "other")
  sha1 <- git_ref_to_sha("main", repo$path)
  sha2 <- git_ref_to_sha("other", repo$path)
  
  expect_match(sha1, "^[[:xdigit:]]{40}$")
  expect_match(sha2, "^[[:xdigit:]]{40}$")
  expect_true(sha1 != sha2)
  
  expect_equal(git_ref_to_sha("HEAD", repo$path), sha1)
  expect_equal(git_ref_to_sha(substr(sha1, 1, 7), repo$path), sha1)
  
  expect_identical(git_ref_to_sha("unknown", repo$path),
                   NA_character_)
  expect_error(git_ref_to_sha("unknown", repo$path, TRUE),
               "Git reference 'unknown' not found")
})


test_that("handle failure", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  r <- git_run("unknown-command", repo = repo$path)
  expect_false(r$success)
  expect_error(
    git_run("unknown-command", repo = repo$path, check = TRUE),
    r$output, fixed = TRUE)
})
