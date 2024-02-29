git_run <- function(args, repo = NULL, check = FALSE) {
  git <- sys_which("git")
  if (!is.null(repo)) {
    args <- c("-C", repo, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}


git_ref_to_sha <- function(ref, repo = NULL, check = FALSE) {
  assert_scalar_character(ref)
  res <- git_run(c("rev-parse", ref), repo = repo, check = FALSE)
  if (res$success) {
    res$output
  } else if (check) {
    stop(sprintf("Git reference '%s' not found", ref), call. = FALSE)
  } else {
    NA_character_
  }
}


git_get_default_branch <- function(repo = NULL) {
  # This is assuming remote origin exists. We'll get an error if it
  # doesn't. But this should be safe for us as we'll always have cloned
  # this from GitHub.
  origin <- gert::git_remote_info("origin", repo = repo)
  origin$head
}


git_get_modified <- function(ref, base = NULL, 
                             relative_dir = NULL, repo = NULL) {
  if (is.null(base)) {
    base <- git_get_default_branch(repo)
  }
  if (is.null(relative_dir)) {
    relative <- ""
    additional_args <- ""
  } else {
    relative <- sprintf("--relative=%s", relative_dir)
    additional_args <- sprintf("-- %s", relative_dir)
  }
  git_run(
    c("diff", "--name-only", relative,
      sprintf("%s...%s", base, git_ref_to_sha(ref, repo = repo, check = TRUE)),
      additional_args),
    repo = repo, check = TRUE)$output
}
