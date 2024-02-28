##' Create an orderly runner, a porcelain object
##'
##' @title Create orderly runner
##'
##' @param root Orderly root
##'
##' @param validate Logical, indicating if validation should be done
##'   on responses.  This should be `FALSE` in production
##'   environments.  See [porcelain::porcelain] for details
##'
##' @param log_level Logging level to use. Sensible options are "off",
##'   "info" and "all".
##'
##' @return A [porcelain::porcelain] object. Notably this does *not*
##'   start the server
##'
##' @export
api <- function(root, validate = NULL, log_level = "info") {
  logger <- porcelain::porcelain_logger(log_level)
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$include_package_endpoints(state = list(root = root))
  api
}


##' @porcelain GET / => json(root)
root <- function() {
  versions <- list(orderly2 = package_version_string("orderly2"),
                   orderly.runner = package_version_string("orderly.runner"))
  lapply(versions, scalar)
}


##' @porcelain 
##'   GET /report/list => json(report_list)
##'   query hash :: string
##'   state root :: root
report_list <- function(root, hash) {
  contents <- gert::git_ls(root, ref = hash)
  re <- "^src/([^/]+)/(\\1|orderly)\\.(yml|R)$"
  nms <- sub(re, "\\1", 
             grep(re, contents$path, value = TRUE, perl = TRUE),
             perl = TRUE)
  last_changed <- function(nm) {
    max(contents$modified[startsWith(contents$path, sprintf("src/%s", nm))])
  }
  updated_time <- vnapply(nms, last_changed, USE.NAMES = FALSE)
  modified_sources <- git_get_modified(hash, relative_dir = "src/", repo = root)
  modified_reports <- unique(first_dirname(modified_sources))
  has_modifications <- vlapply(nms, function(report_name) {
    report_name %in% modified_reports
  }, USE.NAMES = FALSE)
  data.frame(name = nms, 
             updated_time = updated_time,
             has_modifications = has_modifications)
}
