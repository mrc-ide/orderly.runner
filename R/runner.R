runner_run <- function(orderly_root, reportname, parameters, branch, ref, ...) {
  # Setup
  worker_id <- Sys.getenv("RRQ_WORKER_ID")
  worker_path <- file.path(orderly_root, ".packit", "workers", worker_id)
  point_head_to_ref(worker_path, branch, ref)

  # Run
  withr::with_envvar(
    c(ORDERLY_SRC_ROOT = file.path(worker_path, "src", reportname)),
    orderly2::orderly_run(reportname, parameters = parameters,
                          root = orderly_root, ...)
  )

  # Cleanup
  git_clean(worker_path)
}

point_head_to_ref <- function(worker_path, branch, ref) {
  gert::git_fetch(repo = worker_path)
  gert::git_branch_checkout(branch, repo = worker_path)
  gert::git_reset_hard(ref, repo = worker_path)
}

add_dir_parent_if_empty <- function(files_to_delete, path) {
  contained_files <- list.files(path, full.names = TRUE)
  if (length(setdiff(contained_files, files_to_delete)) > 0) {
    return(files_to_delete)
  }
  add_dir_parent_if_empty(c(files_to_delete, path), dirname(path))
}

get_empty_dirs <- function(worker_path) {
  dirs <- fs::dir_ls(worker_path, recurse = TRUE, type = "directory")
  Reduce(add_dir_parent_if_empty, c(list(character()), dirs))
}

git_clean <- function(worker_path) {
  # gert does not have git clean but this should achieve the same thing
  tryCatch(
    {
      gert::git_stash_save(
        include_untracked = TRUE,
        include_ignored = TRUE,
        repo = worker_path
      )
      gert::git_stash_drop(repo = worker_path)
    },
    error = function(e) {
      # we don't need to rethrow the error here since it doesn't break any
      # further report runs
      if (e$message != "cannot stash changes - there is nothing to stash.") {
        # TODO add logger here
        message(e$message)
      }
      NULL
    }
  )
  # however git ignores all directories, only cares about files, so we may
  # have empty directories left
  unlink(get_empty_dirs(worker_path), recursive = TRUE)
}
