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
  orderly2::orderly_list_src(root, locate = FALSE)
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
