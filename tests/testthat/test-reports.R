test_that("can get list of reports", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  helper_add_git(root)

  reports <- get_reports(root = root, ref = "HEAD")
  expect_setequal(reports$name, c("data", "parameters"))
})


test_that("report list includes last modification time", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  helper_add_git(root)

  writeLines("Hello", file.path(root, "src/data/hello.txt"))
  writeLines("World", file.path(root, "src/parameters/world.txt"))

  t1 <- as.POSIXct("2000-01-01T00:00:00")
  t2 <- as.POSIXct("2010-01-01T00:00:00")

  gert::git_add("src/data/hello.txt", repo = root)
  gert::git_commit(
    "hello",
    author = gert::git_signature("author", "email", t1),
    repo = root
  )

  gert::git_add("src/parameters/world.txt", repo = root)
  gert::git_commit(
    "world",
    author = gert::git_signature("author", "email", t2),
    repo = root
  )

  reports <- get_reports(root = root, ref = "HEAD")
  expect_setequal(reports$name, c("data", "parameters"))
  expect_equal(reports[reports$name == "data",]$updated_at, t1)
  expect_equal(reports[reports$name == "parameters",]$updated_at, t2)
})


test_that("report list includes modification status", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  helper_add_git(root)

  writeLines("Hello", file.path(root, "src/data/hello.txt"))
  git_add_and_commit(root)

  reports <- get_reports(root = root, ref = "HEAD", base = "HEAD^")
  expect_setequal(reports$name, c("data", "parameters"))
  expect_true(reports[reports$name == "data",]$has_changes)
  expect_true(!reports[reports$name == "parameters",]$has_changes)

})


test_that("can get orderly script name", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  expect_equal(get_orderly_script_path("data", "HEAD", root), "src/data/data.R")
  expect_equal(get_orderly_script_path("parameters", "HEAD", root), 
               "src/parameters/parameters.R")
  
  file.copy(file.path(root, "src", "data", "data.R"),
            file.path(root, "src", "data", "orderly.R"))
  git_add_and_commit(root)
  
  expect_error(
    get_orderly_script_path("data", "HEAD", root), 
    "Found 2 valid orderly scripts. There must be one and only one.")
})


test_that("can get report parameters", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  old_sha <- gert::git_commit_id(repo = root)
  
  params <- get_report_parameters("data", "HEAD", root)
  expect_null(params)
  
  params <- get_report_parameters("parameters", "HEAD", root)
  expect_equal(params, list(a = NULL,
                            b = 2,
                            c = NULL))
  
  ## Works with a specific git hash
  params_src <- file.path(root, "src", "parameters", "parameters.R")
  contents <- readLines(params_src)
  contents <- c("orderly2::orderly_parameters(a = 'default', b = 2, c = NULL)", 
                contents[-1])
  writeLines(contents, params_src)
  sha <- git_add_and_commit(root)
  
  params_new <- get_report_parameters("parameters", sha, root)
  expect_equal(params_new, list(a = "default",
                                b = 2,
                                c = NULL))
  
  params_old <- get_report_parameters("parameters", old_sha, root)
  expect_equal(params, params_old)
})
