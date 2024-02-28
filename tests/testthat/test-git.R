test_that("git ref to sha", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  other <- create_new_branch(repo$path, "other")
  sha1 <- git_ref_to_sha(repo$branch, repo$path)
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


test_that("can get default branch when remote origin is set", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  expect_null(git_get_default_branch(repo$path))
  git_run(c("symbolic-ref",
            "refs/remotes/origin/HEAD",
            "refs/remotes/origin/main"),
          repo = repo$path)
  expect_equal(git_get_default_branch(repo$path), "refs/remotes/origin/main")
})


test_that("can get files which have been modified", {
  testthat::skip_on_cran()
  repo <- test_prepare_orderly_remote_example("data")
  copy_examples("parameters", repo$local)
  gert::git_add(".", repo = repo$local)
  user <- "author <author@example.com>"
  gert::git_commit("add parameters", author = user, committer = user, 
                   repo = repo$local)
  
  log <- gert::git_log(repo = repo$local)
  expect_equal(git_get_modified(log$commit[[2]], repo = repo$local),
               character(0))
  expect_equal(git_get_modified(log$commit[[1]], repo = repo$local),
               "src/parameters/orderly.R")
  expect_equal(git_get_modified(log$commit[[1]], relative = "src/", 
                                repo = repo$local),
               "parameters/orderly.R")
  expect_equal(git_get_modified(log$commit[[1]], base = log$commit[[2]],
                                repo = repo$local),
               "src/parameters/orderly.R")
  expect_equal(git_get_modified(log$commit[[2]], base = log$commit[[1]],
                                repo = repo$local),
               character(0))
  expect_equal(git_get_modified(log$commit[[2]], base = log$commit[[2]],
                                repo = repo$local),
               character(0))
})
