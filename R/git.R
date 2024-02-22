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


git_clone_depth_1 <- function(origin, ref) {
  t <- tempfile()
  dir.create(t)
  gert::git_init(t)
  gert::git_remote_add(origin, repo = t)
  sha <- git_ref_to_sha(ref, repo = origin, check = TRUE)
  git_run(c("fetch", "--depth", "1", "origin", sha), repo = t)
  git_run(c("checkout", "FETCH_HEAD"), repo = t)
  t
}
