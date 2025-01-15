test_that("handle failure", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  expect_error(
    git_run("unknown-command", repo = repo$path),
    "'unknown-command' is not a git command", fixed = TRUE)
})


test_that("can get default branch when remote origin is set", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()

  expect_null(git_remote_default_branch_ref(repo$path))
  expect_null(git_remote_default_branch_name(repo$path))

  git_run(c("symbolic-ref",
            "refs/remotes/origin/HEAD",
            "refs/remotes/origin/main"),
          repo = repo$path)

  expect_equal(git_remote_default_branch_ref(repo$path),
               "refs/remotes/origin/main")

  expect_equal(git_remote_default_branch_name(repo$path),
               "main")
})

test_that("can get last commit for a path", {
  repo <- initialise_git_repo()
  c1 <- create_new_commit(repo$path, "hello.txt")
  c2 <- create_new_commit(repo$path, "world.txt")
  c3 <- create_new_commit(repo$path, "hello.txt")
  c4 <- create_new_commit(repo$path, "world.txt")

  expect_equal(git_get_latest_commit("hello.txt", "HEAD", repo$path), c3)
  expect_equal(git_get_latest_commit("world.txt", "HEAD", repo$path), c4)

  # If we start at c2, only it and its ancestors (ie. c1) are considered.
  expect_equal(git_get_latest_commit("hello.txt", c2, repo$path), c1)
  expect_equal(git_get_latest_commit("world.txt", c2, repo$path), c2)
})


test_that("can diff trees", {
   repo <- test_prepare_orderly_remote_example("data")
   copy_examples("parameters", repo$local)
   git_add_and_commit(repo$local)

   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo$local)
   expect_equal(nrow(result), 1)

   expect_equal(result$mode1, "000000")
   expect_equal(result$mode2, "040000")
   expect_match(result$hash1, "[0-9a-f]{20}")
   expect_match(result$hash2, "[0-9a-f]{20}")
   expect_equal(result$status, "A")
   expect_equal(result$src, "parameters")

   create_new_commit(repo$local, "src/parameters/hello.txt")

   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo$local)
   expect_equal(nrow(result), 1)

   expect_equal(result$mode1, "040000")
   expect_equal(result$mode2, "040000")
   expect_match(result$hash1, "[0-9a-f]{20}")
   expect_match(result$hash2, "[0-9a-f]{20}")
   expect_equal(result$status, "M")
   expect_equal(result$src, "parameters")

   fs::file_move(file.path(repo$local, "src", "parameters"),
                 file.path(repo$local, "src", "zparameters"))
   git_add_and_commit(repo$local)

   # diff-tree never detects renames or copies. They are instead represented as
   # a add and delete.
   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo$local)
   expect_equal(nrow(result), 2)
   expect_equal(result$status[[1]], "D")
   expect_equal(result$src[[1]], "parameters")
   expect_equal(result$status[[2]], "A")
   expect_equal(result$src[[2]], "zparameters")
})


test_that("repository path is unique", {
  base <- withr::local_tempdir()
  r1 <- repository_path(base, "https://github.com/mrc-ide/orderly", check = FALSE)
  r2 <- repository_path(base, "https://github.com/mrc-ide/packit", check = FALSE)
  expect_true(r1 != r2)
})


test_that("repository_path checks if path exists", {
  base <- withr::local_tempdir()
  url <- "https://github.com/mrc-ide/orderly"

  expect_error(repository_path(base, url), "Repository does not exist")

  repo <- expect_no_error(repository_path(base, url, check = FALSE))
  fs::dir_create(repo)

  expect_no_error(repository_path(base, url))
})
