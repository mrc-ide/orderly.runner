extract_report_names <- function(paths) {
  re <- "^src/([^/]+)/(\\1|orderly)\\.R$"
  sub(re, "\\1", grep(re, paths, value = TRUE, perl = TRUE))
}

get_reports <- function(ref, root, base = NULL) {
  contents <- gert::git_ls(repo = root, ref = ref)
  names <- extract_report_names(contents$path)

  paths <- paste0("src/", names)
  times <- as.POSIXct(vnapply(paths, function(p) {
    commit <- git_get_latest_commit(p, ref, repo = root)
    gert::git_commit_info(commit, repo = root)$time
  }))

  result <- data.frame(
    name = names,
    updated_at = times,
    row.names = NULL
  )

  if (!is.null(base)) {
    diff <- git_diff_tree(paste0(base, ":src"),
                          paste0(ref, ":src"),
                          repo = root)
    result$has_changes <- names %in% diff$src
  }

  result
}


get_report_parameters <- function(name, ref, root) {
  path <- get_orderly_script_path(name, ref, root)
  sha <- gert::git_commit_id(ref, repo = root)
  contents <- git_run(
    c("show", sprintf("%s:%s", sha, path)), repo = root
  )$output
  exprs <- parse(text = contents)
  orderly2::orderly_parse_expr(exprs, filename = basename(path))$parameters
}


get_orderly_script_path <- function(name, ref, repo) {
  contents <- gert::git_ls(repo, ref = ref)
  re <- sprintf("^src/%s/(%s|orderly)\\.R$", name, name)
  matches <- grep(re, contents$path, value = TRUE, perl = TRUE)
  if (length(matches) != 1) {
    num <- length(matches)
    cli::cli_abort(
      c("Found {num} valid orderly script{?s}. There must be one and only one.",
        setNames(matches, rep("*", length(matches)))))
  }
  matches
}
