orderly_runner_endpoint <- function(method, path, root, validate = TRUE) {
  porcelain::porcelain_package_endpoint("orderly.runner", method, path,
                                        state = list(root = root),
                                        validate = validate)
}


create_temporary_root <- function(...) {
  path <- tempfile()
  withr::defer_parent(unlink(path, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(path, ...))
}


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  suppressMessages(orderly2::orderly_init(tmp, ...))
  copy_examples(examples, tmp)
  as.character(fs::path_norm(tmp))
}


copy_examples <- function(examples, path_src) {
  fs::dir_create(path_src)

  fs::dir_create(file.path(path_src, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(path_src, "src"))
  }
}


helper_add_git <- function(path) {
  gert::git_init(path)
  gert::git_add(".", repo = path)
  user <- "author <author@example.com>"
  sha <- gert::git_commit("initial", author = user, committer = user,
                          repo = path)
  branch <- gert::git_branch(repo = path)
  url <- "https://example.com/git"
  gert::git_remote_add(url, repo = path)
  list(path = path, user = user, branch = branch, sha = sha, url = url)
}
