##' Create an orderly runner, a porcelain object
##'
##' @title Create orderly runner
##'
##' @param root Orderly root
##'
##' @param repositories_base_path Path in which Git repositories are
##'   cloned.
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
    root, repositories_base_path,
    validate = NULL, log_level = "info",
    skip_queue_creation = FALSE) {
  logger <- porcelain::porcelain_logger(log_level)

  # Set ORDERLY_RUNNER_QUEUE_ID to specify existing queue id
  if (skip_queue_creation) {
    queue <- NULL
  } else {
    queue <- Queue$new(root)
  }

  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$include_package_endpoints(state = list(
    root = root,
    repositories_base_path = repositories_base_path,
    queue = queue))
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


##' @porcelain POST /repository/fetch => json(repository_fetch_response)
##'   state repositories_base_path :: repositories_base_path
##'   body data :: json(repository_fetch_request)
repository_fetch <- function(repositories_base_path, data) {
  data <- jsonlite::parse_json(data)
  r <- git_sync(repositories_base_path, data$url)

  empty_object()
}


##' @porcelain GET /repository/branches => json(repository_branches)
##'   state repositories_base_path :: repositories_base_path
##'   query url :: string
repository_branches <- function(repositories_base_path, url) {
  repo <- repository_path(repositories_base_path, url)
  branches <- git_remote_list_branches(repo)
  message <- vcapply(branches$commit, function(commit) {
    gert::git_commit_info(repo = repo, ref = commit)$message
  })

  branches$message <- message
  list(
    default_branch = scalar(git_remote_default_branch_name(repo)),
    branches = data.frame(
      name = branches$name,
      commit_hash = branches$commit,
      time = as.numeric(branches$updated),
      message = message,
      row.names = NULL
    )
  )
}


##' @porcelain
##'   GET /report/list => json(report_list)
##'   query ref :: string
##'   state root :: root
report_list <- function(root, ref) {
  base <- git_get_default_branch(root)
  reports <- get_reports(root = root, ref = ref, base = base)

  data.frame(
    name = reports$name,
    updatedTime = as.numeric(reports$updated_at),
    hasModifications = reports$has_changes
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
