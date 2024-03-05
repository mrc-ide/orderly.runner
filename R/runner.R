runner_run <- function(orderly_root, reportname, parameters, branch, ref, ...) {
  ## Setup
  # working dir
  worker_id <- Sys.getenv("WORKER_ID")
  worker_path <- file.path(orderly_root, ".packit", "workers", worker_id)

  # ? PASS in repo/path or repo open
  # point head to correct ref
  gert::git_fetch(repo = worker_path)
  gert::git_branch_checkout(branch, repo = worker_path)
  gert::git_reset_hard(ref, repo = worker_path)

  ## Run
  withr::with_envvar(
    c(ORDERLY_REPORT_SRC = file.path(worker_path, "src", reportname)),
    orderly2::orderly_run(reportname, parameters = parameters,
                          root = orderly_root, ...)
  )

  ## Cleanup
  # gert does not have git clean but this should achieve the same thing
  res <- tryCatch(
    gert::git_stash_save(
      include_untracked = TRUE,
      include_ignored = TRUE
    ),
    error = function(e) NULL
  )
  if (!is.null(res)) {
    gert::git_stash_drop()
  }
  # however git ignores all directories, only cares about files so we may
  # have empty directories left, TAF::rmdir removes all empty directories
  # ? use fs::dir_walk()
  # ? helper function called git_clean
  # ? investigate git_clean function
  TAF::rmdir(".", recursive = TRUE)
}

# only on worker startup
clone_orderly_repo <- function(orderly_root, worker_root) {
  suppressMessages(gert::git_clone(orderly_root, path = worker_root))
}
