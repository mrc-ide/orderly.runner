#' Get the storage path for a repository.
#'
#' Repositories are all stored as subdirectories of a common base path,
#' using a hash of their URL as the directory name.
#'
#' @param base the base directory in which all repositories are stored.
#' @param url the URL of the remote repository.
#' @param check if TRUE and the repository does not exist locally yet, an error
#'   is raised.
#' @return the path to the repository.
repository_path <- function(base, url, check = TRUE) {
  hash <- openssl::sha1(url)
  path <- file.path(base, hash)
  if (check && !fs::dir_exists(path)) {
    porcelain::porcelain_stop(
      message = "Repository does not exist",
      code = "NOT_FOUND",
      status_code = 404)
  }
  path
}


#' Clone or fetch a remote repository.
#'
#' @param base the base directory in which all repositories are stored.
#' @param url the URL of the remote repository.
#' @param ssh_key private SSH key for connecting to private repositories
#'   (Optional, default is NULL).
#' @return the path to the local clone of the repository.
git_sync <- function(base, url, ssh_key = NULL) {
  if (!is.null(ssh_key)) {
    ssh_key <- withr::local_tempfile(lines = ssh_key)
  }
  repo <- repository_path(base, url, check = FALSE)

  # gert prioritises using an ssh agent over the provided ssh key. In our
  # case we only ever want to use the provided ssh key, so we prevent any
  # attempt to use the agent by unsetting `SSH_AUTH_SOCK`.
  # https://github.com/r-lib/gert/blob/d8febbfacad1e8bb582e28f50c7e7092c0c63d21/src/clone.c#L192
  withr::with_envvar(new = c("SSH_AUTH_SOCK" = NA), {
    if (!fs::dir_exists(repo)) {
      gert::git_clone(url = url, path = repo, bare = TRUE, verbose = FALSE, ssh_key = ssh_key)
    } else {
      gert::git_fetch(repo = repo, prune = TRUE, verbose = FALSE, ssh_key = ssh_key)
    }
  })

  repo
}

git_remote_list_branches <- function(repo) {
  branches <- gert::git_branch_list(repo, local = FALSE)
  branches$name <- gsub("^refs/remotes/origin/", "", branches$ref)
  branches <- branches[branches$name != "HEAD", ]
  branches
}

git_run <- function(args, repo = NULL) {
  git <- sys_which("git")
  if (!is.null(repo)) {
    args <- c("-C", repo, args)
  }
  res <- system3(git, args)
  if (!res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}


git_remote_default_branch_ref <- function(repo) {
  # This is assuming remote origin exists. We'll get an error if it
  # doesn't. But this should be safe for us as we'll always have cloned
  # this from GitHub.
  origin <- gert::git_remote_info("origin", repo = repo)
  origin$head
}


git_remote_default_branch_name <- function(repo) {
  ref <- git_remote_default_branch_ref(repo)
  if (!is.null(ref)) {
    gsub("^refs/remotes/origin/", "", ref)
  } else {
    NULL
  }
}


#' Get the last commit to have modified the given path.
#'
#' If the path is a directory, any modification to files contained within
#' it are considered.
#'
#' @param path path to the file of directory to search for
#' @param ref the Git commit from which to start the search. Only ancestors of
#'   that commit will be considered.
#' @param repo the path to the Git repo to use.
git_get_latest_commit <- function(path, ref, repo = NULL) {
  # libgit2 (and thus gert) doesn't really have an interface for this.
  # See https://github.com/libgit2/libgit2/issues/495
  git_run(c("rev-list", "--max-count=1", ref, "--", path),
          repo = repo)$output
}


#' Get the difference between two tree-ish
#'
#' @param left the tree-ish to use as the base for the comparison.
#' @param right the tree-ish to compare to the base.
#' @param repo the path to the Git repo to use.
#' @return a dataframe for each differing entry in the trees, with columns
#'   `mode1`, `mode2`, `hash1`, `hash2`, `status`, `score`, `src` and `dst`.
git_diff_tree <- function(left, right, repo = NULL) {
  output <- git_run(c("diff-tree", left, right), repo = repo)$output

  # See https://git-scm.com/docs/git-diff-tree#_raw_output_format for a
  # description of the format.
  re <- paste0(
    "^:(?<mode1>\\d+) (?<mode2>\\d+) (?<hash1>[0-9a-f]+) (?<hash2>[0-9a-f]+)",
    " (?<status>[A-Z])(?<score>\\d+)?\\t(?<src>[^\\t]*)(?:\\t(?<dst>[^\\t]*))?$")
  as.data.frame(stringr::str_match(output, re)[, -1, drop = FALSE])
}


create_temporary_worktree <- function(repo, branch, tmpdir, env = parent.frame()) {
  path <- withr::local_tempdir(tmpdir = tmpdir, .local_envir = env)

  git_run(c("worktree", "add", "--force", path, branch), repo = repo)
  withr::defer(env = env, {
    git_run(c("worktree", "remove", "--force", path), repo = repo)
  })

  path
}
