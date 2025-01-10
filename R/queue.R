#' Object for managing running jobs on the redis queue
#'
#' @keywords internal
Queue <- R6::R6Class("Queue", # nolint
  cloneable = FALSE,
  public = list(
    #' @field controller RRQ controller
    controller = NULL,

    #' @description
    #' Create object, read configuration and setup Redis connection.
    #'
    #' @param queue_id ID of an existing queue to connect to, creates a new one
    #'   if NULL (default NULL)
    #' @param logs_dir directory to store worker logs
    initialize = function(queue_id = NULL, logs_dir = "logs/worker") {
      # Connect to Redis
      con <- redux::hiredis()

      # Create queue
      self$controller <- rrq::rrq_controller(queue_id %||% orderly_queue_id())
      dir.create(logs_dir, showWarnings = FALSE)
      worker_config <- rrq::rrq_worker_config(heartbeat_period = 10, logdir = logs_dir)
      rrq::rrq_worker_config_save("localhost", worker_config,
        controller = self$controller
      )
    },

    #' @description
    #' Submit a job the Redis queue for runner to run.
    #'
    #' @param url The URL of the Git repository containing the reports.
    #' @param branch Name of git branch to checkout the repository.
    #' @param ref Git commit-ish value (e.g HEAD or commit hash or branch name).
    #' @param reportname Name of orderly report.
    #' @param parameters Parameters to run the report with.
    #' @param location Location of the outpack repository from which to pull
    #'   dependencies and push the produced packet.
    submit = function(url, branch, ref, reportname, parameters, location) {
      run_args <- list(
        url,
        branch,
        ref,
        reportname,
        parameters,
        location
      )
      rrq::rrq_task_create_call(runner_run, run_args,
        separate_process = TRUE,
        controller = self$controller
      )
    },

    # Just until we add queue status for testing
    number_of_workers = function() {
      rrq::rrq_worker_len(self$controller)
    },

    #' @description
    #' Gets status of packet run
    #'
    #' @param task_ids Task ids  to get status of.
    #' @param include_logs Whether to include logs in response or not
    #' @return status of redis queue job
    get_status = function(task_ids, include_logs = TRUE) {
      invalid_task_ids <- task_ids[!rrq::rrq_task_exists(task_ids, controller = self$controller)]
      if (length(invalid_task_ids) > 0) {
        porcelain::porcelain_stop(sprintf("Job ids [%s] do not exist in the queue", paste(invalid_task_ids, collapse = ", ")))
      }
      statuses <- rrq::rrq_task_status(task_ids, controller = self$controller)
      tasks_times <- rrq::rrq_task_times(task_ids, controller = self$controller)
      queuePositions <- rrq::rrq_task_position(task_ids, controller = self$controller)

      lapply(seq_along(task_ids), function(index) {
        list(
          status = scalar(statuses[index]),
          queuePosition = if (statuses[index] == "PENDING") scalar(queuePositions[index]) else NULL,
          timeQueued = scalar(tasks_times[task_ids[index], 1]),
          timeStarted = scalar(tasks_times[task_ids[index], 2]),
          timeComplete = scalar(tasks_times[task_ids[index], 3]),
          packetId = if (statuses[index] == "COMPLETE") scalar(rrq::rrq_task_result(task_ids[index], controller = self$controller)) else NULL,
          logs = if (include_logs) rrq::rrq_task_log(task_ids[index], controller = self$controller) else NULL,
          taskId = scalar(task_ids[index])
        )
      })
    }
  ),
)

runner_has_git <- function(path) {
  nzchar(Sys.which("git")) && file.exists(file.path(path, ".git"))
}

orderly_queue_id <- function() {
  id <- Sys.getenv("ORDERLY_RUNNER_QUEUE_ID", "")
  if (nzchar(id)) id else sprintf("orderly.runner:%s", ids::random_id())
}
