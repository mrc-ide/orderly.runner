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
##' @param skip_queue_creation Skip queue creation, this is primarily
##'   used for tests where we can't establish a Redis connection.
##'
##' @return A [porcelain::porcelain] object. Notably this does *not*
##'   start the server
##'
##' @export
api <- function(
  root, validate = NULL, log_level = "info",
  skip_queue_creation = FALSE
) {
  logger <- porcelain::porcelain_logger(log_level)

  # Set ORDERLY_RUNNER_QUEUE_ID to specify existing queue id
  if (skip_queue_creation) {
    queue <- NULL
  } else {
    queue <- Queue$new(root)
  }

  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$include_package_endpoints(state = list(root = root, queue = queue))
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
##'   query ref :: string
##'   state root :: root
report_list <- function(root, ref) {
  contents <- gert::git_ls(root, ref = ref)
  re <- "^src/([^/]+)/(\\1|orderly)\\.R$"
  nms <- sub(re, "\\1", 
             grep(re, contents$path, value = TRUE, perl = TRUE),
             perl = TRUE)
  last_changed <- function(nm) {
    max(contents$modified[startsWith(contents$path, sprintf("src/%s", nm))])
  }
  updated_time <- vnapply(nms, last_changed, USE.NAMES = FALSE)
  modified_sources <- git_get_modified(ref, relative_dir = "src/", repo = root)
  modified_reports <- unique(first_dirname(modified_sources))
  has_modifications <- vlapply(nms, function(report_name) {
    report_name %in% modified_reports
  }, USE.NAMES = FALSE)
  data.frame(name = nms, 
             updated_time = updated_time,
             has_modifications = has_modifications)
}


##' @porcelain
##'   GET /report/<name:string>/parameters => json(report_parameters)
##'   query ref :: string
##'   state root :: root
report_parameters <- function(root, ref, name) {
  params <- get_report_parameters(name, ref, root)
  lapply(names(params), function(param_name) {
    value <- params[[param_name]]
    list(
      name = scalar(param_name),
      value = if (is.null(value)) value else scalar(as.character(value))
    )
  })
}

##' @porcelain
##'   POST /report/run => json(report_run_response)
##'   state root :: root
##'   state queue :: queue
##'   body data :: json(report_run_request)
submit_report_run <- function(root, queue, data) {
  data <- jsonlite::parse_json(data)
  job_id <- queue$submit(
    data$name,
    branch = data$branch,
    ref = data$hash,
    parameters = data$parameters
  )
  list(job_id = scalar(job_id))
}

##' @porcelain
##'   GET /report/status/<job_id:string> => json(report_run_status_response)
##'   state root :: root
##'   state queue :: queue
report_run_status <- function(root, queue, job_id) { 
  queue$get_status(job_id)
}
