#' Object for managing running jobs on the redis queue
#'
#' @keywords internal
Queue <- R6::R6Class("Queue", #nolint
  cloneable = FALSE,
  public = list(
    #' @field root Orderly root
    root = NULL,
    #' @field config Orderly config
    config = NULL,
    #' @field controller RRQ controller
    controller = NULL,

    #' @description
    #' Create object, read configuration and setup Redis connection.
    #'
    #' @param root Orderly root.
    #' @param queue_id ID of an existing queue to connect to, creates a new one
    #'   if NULL (default NULL)
    initialize = function(root, queue_id = NULL) {
      self$root <- root
      self$config <- orderly2::orderly_config(self$root)
      if (!runner_has_git(self$root)) {
        cli::cli_abort(paste("Not starting server as orderly",
                             "root is not version controlled."))
      }

      # Create queue
      self$controller <- rrq::rrq_controller(
        queue_id %||% orderly_queue_id()
      )
      log_dir_name <- "runner-logs"
      dir.create(log_dir_name, showWarnings = FALSE)
      worker_config <- rrq::rrq_worker_config(heartbeat_period = 10, logdir = log_dir_name)
      rrq::rrq_worker_config_save("localhost", worker_config,
                                  controller = self$controller)
    },

    #' @description
    #' Submit a job the Redis queue for runner to run.
    #'
    #' @param reportname Name of orderly report.
    #' @param parameters Parameters to run the report with (default NULL)
    #' @param branch Name of git branch to checkout the repository
    #'   (default master)
    #' @param ref Git commit-ish value (e.g HEAD or commit hash or branch name).
    #'   We reset hard to this ref and run the report. (default HEAD)
    submit = function(reportname, parameters = NULL,
                      branch = "master", ref = "HEAD") {
      run_args <- list(
        self$root,
        reportname,
        parameters,
        branch,
        ref
      )
      rrq::rrq_task_create_call(runner_run, run_args,
                                separate_process = TRUE,
                                controller = self$controller)
    },

    # Just until we add queue status for testing
    number_of_workers = function() {
      rrq::rrq_worker_len(self$controller)
    },

    #' @description
    #' Gets status of packet run
    #'
    #' @param job_ids Id of redis queue job to get status of.
    #' @param include_logs Whether to include logs in response or not
    #' @return status of redis queue job
    get_status = function(job_ids, include_logs = TRUE) {
      valid_job_ids <- job_ids[rrq::rrq_task_exists(job_ids, controller = self$controller)]
      if (length(valid_job_ids) != length(job_ids)) {
        cli::cli_warn("Some job ids do not exist in the queue")
      }
      statuses <- rrq::rrq_task_status(valid_job_ids, controller = self$controller)
      tasks_times <- rrq::rrq_task_times(valid_job_ids, controller = self$controller)
      queue_positions <- rrq::rrq_task_position(valid_job_ids, controller = self$controller)

      lapply(seq_along(valid_job_ids), function(index) {
        list(
          status = scalar(statuses[index]),
          queue_position = if (statuses[index] == "PENDING") scalar(queue_positions[index]) else NULL,
          time_queued = scalar(tasks_times[valid_job_ids[index], 1]),
          time_started = scalar(tasks_times[valid_job_ids[index], 2]),
          time_complete = scalar(tasks_times[valid_job_ids[index], 3]),
          packet_id = if (statuses[index] == "COMPLETE") scalar(rrq::rrq_task_result(valid_job_ids[index], controller = self$controller)) else NULL,
          logs = if (include_logs) rrq::rrq_task_log(valid_job_ids[index], controller = self$controller) else NULL
        )
      })
    },

    #' @description Destroy queue
    finalize = function() {
      rrq::rrq_destroy(controller = self$controller)
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
