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
  git_add_and_commit(repo$local)
  
  log <- gert::git_log(repo = repo$local)
  expect_equal(git_get_modified(log$commit[[2]], repo = repo$local),
               character(0))
  expect_equal(git_get_modified(log$commit[[1]], repo = repo$local),
               "src/parameters/parameters.R")
  expect_equal(git_get_modified(log$commit[[1]], relative = "src/", 
                                repo = repo$local),
               "parameters/parameters.R")
  expect_equal(git_get_modified(log$commit[[1]], base = log$commit[[2]],
                                repo = repo$local),
               "src/parameters/parameters.R")
  expect_equal(git_get_modified(log$commit[[2]], base = log$commit[[1]],
                                repo = repo$local),
               character(0))
  expect_equal(git_get_modified(log$commit[[2]], base = log$commit[[2]],
                                repo = repo$local),
               character(0))
})
