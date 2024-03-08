get_report_parameters <- function(name, ref, root) {
  path <- get_orderly_script_path(name, ref, root)
  sha <- gert::git_commit_id(ref, repo = root)
  contents <- git_run(
    c("show", sprintf("%s:%s", sha, path)), repo = root, check = TRUE
  )$output
  exprs <- parse(text = contents)
  orderly2::orderly_parse_expr(exprs, filename = basename(path))$parameters
}


get_orderly_script_path <- function(name, ref, root) {
  contents <- gert::git_ls(root, ref = ref)
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
