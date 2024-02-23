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
  ## Note there is a bug in current gert, and git_ls returns 0 for modified
  ## and created time whenever called with a "ref". Get them via stat_files
  ## for now
  contents <- gert::git_stat_files(contents$path, ref = hash, repo = root)
  re <- "^src/([^/]+)/(\\1|orderly)\\.(yml|R)$"
  nms <- sub(re, "\\1", 
             grep(re, contents$file, value = TRUE, perl = TRUE), 
             perl = TRUE)
  last_changed <- function(nm) {
    max(contents$modified[startsWith(contents$file, sprintf("src/%s", nm))])
  }
  data.frame(name = nms, 
             updated_time = vnapply(nms, last_changed, USE.NAMES = FALSE))
}
