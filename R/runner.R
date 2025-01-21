runner_run <- function(url, branch, ref, reportname, parameters, location, ssh_key = NULL, ...) {
  storage <- Sys.getenv("ORDERLY_WORKER_STORAGE")
  stopifnot(nzchar(storage) && fs::dir_exists(storage))

  repositories <- fs::dir_create(storage, "git")
  worktree_base <- fs::dir_create(storage, "worktrees")

  repo <- git_sync(repositories, url, ssh_key)

  # We could create the worktree with a detached HEAD and not bother with
  # creating the branch, but then Orderly's metadata wouldn't be as complete.
  #
  # Using a named branch does introduce some persistent state in the Git
  # repository, which the runner generally does its best to avoid. That is an
  # acceptable compromise given that repositories are private to each worker.
  #
  # The branch/ref association here does not have to match the remote: if
  # the upstream repository has just been pushed to, the commit associated with
  # this run may be an older commit of that branch. We will happily use that
  # older commit.
  gert::git_branch_create(branch, ref = ref, force = TRUE, checkout = FALSE,
                          repo = repo)
  worktree <- create_temporary_worktree(repo, branch, worktree_base)

  orderly2::orderly_init(worktree)
  orderly2::orderly_location_add("upstream", location$type, location$args,
                                 root = worktree)

  id <- orderly2::orderly_run(reportname, parameters = parameters,
                              fetch_metadata = TRUE, allow_remote = TRUE,
                              location = "upstream", root = worktree, ...)
  orderly2::orderly_location_push(id, "upstream", root = worktree)
  id
}
