test_that("can get orderly script name", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  git_info <- helper_add_git(root)
  expect_equal(get_orderly_script_path("data", "HEAD", root), "src/data/data.R")
  expect_equal(get_orderly_script_path("parameters", "HEAD", root), 
               "src/parameters/orderly.R")
  
  file.copy(file.path(root, "src", "data", "data.R"),
            file.path(root, "src", "data", "orderly.R"))
  git_add_and_commit(root)
  
  expect_error(get_orderly_script_path("data", "HEAD", root), 
               "Found 2 valid orderly scripts. There must only be one.")
})


test_that("can get report parameters", {
  root <- test_prepare_orderly_example(c("data", "parameters"))
  git_info <- helper_add_git(root)
  
  params <- get_report_parameters("data", "HEAD", root)
  expect_null(params)
  
  params <- get_report_parameters("parameters", "HEAD", root)
  expect_equal(params, list(a = NULL,
                            b = 2,
                            c = NULL))
  
  ## Works with a specific git hash
  params_src <- file.path(root, "src", "parameters", "orderly.R")
  contents <- readLines(params_src)
  contents <- c("orderly2::orderly_parameters(a = 'default', b = 2, c = NULL)", 
                contents[-1])
  writeLines(contents, params_src)
  sha <- git_add_and_commit(root)
  
  params_new <- get_report_parameters("parameters", sha, root)
  expect_equal(params_new, list(a = "default",
                                b = 2,
                                c = NULL))
  
  params_old <- get_report_parameters("parameters", git_info$sha, root)
  expect_equal(params, params_old)
})
