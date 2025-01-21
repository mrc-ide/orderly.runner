##' Create an orderly runner, a porcelain object
##'
##' @title Create orderly runner
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
    repositories_base_path,
    validate = NULL, log_level = "info",
    skip_queue_creation = FALSE) {
  logger <- porcelain::porcelain_logger(log_level)

  # Set ORDERLY_RUNNER_QUEUE_ID to specify existing queue id
  if (skip_queue_creation) {
    queue <- NULL
  } else {
    queue <- Queue$new()
  }

  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$include_package_endpoints(state = list(
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
##'   query url :: string
##'   body data :: json(repository_fetch_request)
##'   state repositories_base_path :: repositories_base_path
repository_fetch <- function(repositories_base_path, url, data) {
  data <- jsonlite::parse_json(data)
  r <- git_sync(repositories_base_path, url, data$ssh_key)

  empty_object()
}


##' @porcelain GET /repository/branches => json(repository_branches)
##'   query url :: string
##'   state repositories_base_path :: repositories_base_path
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
##'   query url :: string
##'   query ref :: string
##'   state repositories_base_path :: repositories_base_path
report_list <- function(repositories_base_path, url, ref) {
  repo <- repository_path(repositories_base_path, url)
  base <- git_remote_default_branch_ref(repo)
  reports <- get_reports(root = repo, ref = ref, base = base)

  data.frame(
    name = reports$name,
    updatedTime = as.numeric(reports$updated_at),
    hasModifications = reports$has_changes
  )
}


##' @porcelain
##'   GET /report/parameters => json(report_parameters)
##'   query url :: string
##'   query ref :: string
##'   query name :: string
##'   state repositories_base_path :: repositories_base_path
report_parameters <- function(repositories_base_path, url, ref, name) {
  repo <- repository_path(repositories_base_path, url)
  params <- get_report_parameters(name, ref, repo)
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
##'   query url :: string
##'   body data :: json(report_run_request)
##'   state queue :: queue
submit_report_run <- function(queue, url, data) {
  data <- jsonlite::parse_json(data)
  task_id <- queue$submit(
    reportname = data$name,
    url = url,
    branch = data$branch,
    ref = data$hash,
    parameters = data$parameters,
    location = data$location,
    ssh_key = data$ssh_key
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
