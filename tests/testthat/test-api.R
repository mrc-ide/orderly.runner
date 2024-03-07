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
  repo <- test_prepare_orderly_remote_example(c("data", "parameters"))
  endpoint <- orderly_runner_endpoint("GET", "/report/list", repo$local)

  res <- endpoint$run(gert::git_branch(repo$local))
  expect_equal(res$status_code, 200)
  expect_setequal(res$data$name, c("data", "parameters"))
  expect_true(all(res$data$updated_time > (Sys.time() - 100)))
  expect_false(all(res$data$has_modifications))

  ## Add a report on a 2nd branch
  gert::git_branch_create("other", repo = repo$local)
  fs::dir_copy(file.path(repo$local, "src", "parameters"),
               file.path(repo$local, "src", "parameters2"))
  # have to rename to parameters2.R to recognise it as orderly report
  file.rename(
    file.path(repo$local, "src", "parameters2", "parameters.R"),
    file.path(repo$local, "src", "parameters2", "parameters2.R")
  )
  gert::git_add(".", repo = repo$local)
  sha <- gert::git_commit("Add report data2", repo = repo$local,
                          author = "Test User <test.user@example.com>")

  ## Can list items from this sha
  other_res <- endpoint$run(sha)
  expect_equal(other_res$status_code, 200)
  params2 <- other_res$data[other_res$data$name == "parameters2", ]
  existing <- other_res$data[other_res$data$name != "parameters2", ]
  expect_equal(existing, res$data)
  expect_equal(nrow(params2), 1)
  expect_true(params2$has_modifications)

  ## We can still see all reports on main branch
  commits <- gert::git_log(repo = repo$local)$commit
  first_commit <- commits[length(commits)]
  first_commit_res <- endpoint$run(first_commit)
  expect_equal(first_commit_res$status_code, 200)
  expect_equal(first_commit_res$data, res$data)
})
