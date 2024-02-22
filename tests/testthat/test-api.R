test_that("root data returns sensible, validated, data", {
  ## Just hello world for the package really
  endpoint <- orderly_runner_endpoint("GET", "/", NULL)
  res <- endpoint$run()
  expect_true(res$validated)
  expect_true(all(c("orderly2", "orderly.runner") %in%
                  names(res$data)))
  expect_match(unlist(res$data), "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})


test_that("Can construct the api", {
  root <- create_temporary_root(use_file_store = TRUE)
  obj <- api(root)
  result <- evaluate_promise(value <- obj$request("GET", "/")$status)
  expect_equal(value, 200)
  logs <- lapply(strsplit(result$output, "\n")[[1]], jsonlite::parse_json)
  expect_length(logs, 2)
  expect_equal(logs[[1]]$logger, "orderly.runner")
})


test_that("can list orderly reports", {
  path <- test_prepare_orderly_example(c("data", "parameters"))
  repo <- helper_add_git(path)
  endpoint <- orderly_runner_endpoint("GET", "/report/list", path)
  get_reports_from_response <- function(res) {
    vcapply(res$data, function(item) as.character(item[["name"]]))
  }

  res <- endpoint$run(repo$branch)
  expect_equal(res$status_code, 200)
  expect_setequal(get_reports_from_response(res), c("data", "parameters"))

  ## Delete a report on a 2nd branch
  gert::git_branch_create("other", repo = path)
  unlink(file.path(path, "src", "data"), recursive = TRUE)
  gert::git_add(".", repo = path)
  sha <- gert::git_commit("Remove data report", repo = path,
                          author = "Test User <test.user@example.com>")

  ## Can list items from this sha
  other_res <- endpoint$run(sha)
  expect_equal(other_res$status_code, 200)
  expect_equal(get_reports_from_response(other_res), "parameters")

  ## We can still see all reports on main branch
  first_commit_res <- endpoint$run(repo$sha)
  expect_equal(first_commit_res$status_code, 200)
  expect_setequal(get_reports_from_response(first_commit_res), 
                  c("data", "parameters"))
})
