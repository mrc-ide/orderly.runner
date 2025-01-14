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
#' @return the path to the local clone of the repository.
git_sync <- function(base, url) {
  repo <- repository_path(base, url, check = FALSE)
  if (!fs::dir_exists(repo)) {
    gert::git_clone(url = url, path = repo, bare = TRUE, verbose = FALSE)
  } else {
    gert::git_fetch(repo = repo, prune = TRUE, verbose = FALSE)
  }
  repo
}

git_remote_list_branches <- function(repo) {
  branches <- gert::git_branch_list(repo, local = FALSE)
  branches$name <- gsub("^refs/remotes/origin/", "", branches$ref)
  branches <- branches[branches$name != "HEAD", ]
  branches
}

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


git_get_modified <- function(ref, base = NULL, 
                             relative_dir = NULL, repo = NULL) {
  if (is.null(base)) {
    base <- git_remote_default_branch_ref(repo)
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
      sprintf("%s...%s", base, gert::git_commit_id(ref, repo = repo)),
      additional_args),
    repo = repo, check = TRUE)$output
}
