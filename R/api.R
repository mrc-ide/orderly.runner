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
    skip_queue_creation = FALSE) {
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
  versions <- list(
    orderly2 = package_version_string("orderly2"),
    orderly.runner = package_version_string("orderly.runner")
  )
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
    perl = TRUE
  )
  last_changed <- function(nm) {
    max(contents$modified[startsWith(contents$path, sprintf("src/%s", nm))])
  }
  updatedTime <- vnapply(nms, last_changed, USE.NAMES = FALSE)
  modified_sources <- git_get_modified(ref, relative_dir = "src/", repo = root)
  modified_reports <- unique(first_dirname(modified_sources))
  hasModifications <- vlapply(nms, function(report_name) {
    report_name %in% modified_reports
  }, USE.NAMES = FALSE)
  data.frame(
    name = nms,
    updatedTime = updatedTime,
    hasModifications = hasModifications
  )
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
      value = if (is.null(value)) value else scalar(value)
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
  task_id <- queue$submit(
    data$name,
    branch = data$branch,
    ref = data$hash,
    parameters = data$parameters
  )
  list(taskId = scalar(task_id))
}

##' @porcelain
##'   POST /report/status => json(report_run_status_response)
##'   state queue :: queue
##'   query include_logs :: logical
##'   body data :: json(report_run_status_request)
report_run_status <- function(queue, include_logs, data) {
  task_ids <- jsonlite::fromJSON(data)
  queue$get_status(task_ids, include_logs)
}
