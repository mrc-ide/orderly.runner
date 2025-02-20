test_that("handle failure", {
  testthat::skip_on_cran()
  repo <- initialise_git_repo()
  expect_error(
    git_run("unknown-command", repo = repo),
    "'unknown-command' is not a git command", fixed = TRUE)
})


test_that("can get default branch of clone", {
  testthat::skip_on_cran()
  upstream <- initialise_git_repo()
  repo <- gert::git_clone(upstream, withr::local_tempdir())

  expect_equal(git_remote_default_branch_ref(repo),
               "refs/remotes/origin/master")

  expect_equal(git_remote_default_branch_name(repo),
               "master")
})


test_that("git sync clone down private repo", {
  private_repo <- skip_if_no_test_private_repo_ssh_key()
  repositories <- withr::local_tempdir()

  git_sync(repositories, private_repo$url, private_repo$ssh_key)

  # has cloned successfully
  expect_length(fs::dir_ls(repositories), 1)

  fetch_head_path <- file.path(fs::dir_ls(repositories), "FETCH_HEAD")
  clone_time <- get_time_modified_of_file(fetch_head_path)

  # make sure the fetch and clone times are not the same if the test
  # runs too fast
  Sys.sleep(1)

  git_sync(repositories, private_repo$url, private_repo$ssh_key)
  fetch_time <- get_time_modified_of_file(fetch_head_path)

  # FETCH_HEAD is always updated on fetch even if there is nothing to
  # fetch so just testing that the fetch branch of git_sync also works
  expect_true(fetch_time > clone_time)
})


test_that("can get last commit for a path", {
  repo <- initialise_git_repo()
  c1 <- create_new_commit(repo, "hello.txt")
  c2 <- create_new_commit(repo, "world.txt")
  c3 <- create_new_commit(repo, "hello.txt")
  c4 <- create_new_commit(repo, "world.txt")

  expect_equal(git_get_latest_commit("hello.txt", "HEAD", repo), c3)
  expect_equal(git_get_latest_commit("world.txt", "HEAD", repo), c4)

  # If we start at c2, only it and its ancestors (ie. c1) are considered.
  expect_equal(git_get_latest_commit("hello.txt", c2, repo), c1)
  expect_equal(git_get_latest_commit("world.txt", c2, repo), c2)
})


test_that("can diff trees", {
   repo <- test_prepare_orderly_example("data")
   copy_examples("parameters", repo)
   git_add_and_commit(repo)

   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo)
   expect_equal(nrow(result), 1)

   expect_equal(result$mode1, "000000")
   expect_equal(result$mode2, "040000")
   expect_match(result$hash1, "[0-9a-f]{20}")
   expect_match(result$hash2, "[0-9a-f]{20}")
   expect_equal(result$status, "A")
   expect_equal(result$src, "parameters")

   create_new_commit(repo, "src/parameters/hello.txt")

   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo)
   expect_equal(nrow(result), 1)

   expect_equal(result$mode1, "040000")
   expect_equal(result$mode2, "040000")
   expect_match(result$hash1, "[0-9a-f]{20}")
   expect_match(result$hash2, "[0-9a-f]{20}")
   expect_equal(result$status, "M")
   expect_equal(result$src, "parameters")

   fs::file_move(file.path(repo, "src", "parameters"),
                 file.path(repo, "src", "zparameters"))
   git_add_and_commit(repo)

   # diff-tree never detects renames or copies. They are instead represented as
   # a add and delete.
   result <- git_diff_tree("HEAD^:src", "HEAD:src", repo = repo)
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
