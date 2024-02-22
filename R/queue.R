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

    #' @description
    #' Create object, read configuration and setup Redis connection.
    #'
    #' @param root Orderly root.
    #' @param queue_id ID of an existing queue to connect to, creates a new one
    #'   if NULL (default NULL)
    #' @param workers Number of workers to spawn (default 1)
    #' @param cleanup_on_exit If TRUE workers are killed on exit (defaults
    #'   to TRUE if workers is > 0)
    initialize = function(root, queue_id = NULL, workers = 1,
                          cleanup_on_exit = workers > 0) {
      private$cleanup_on_exit <- cleanup_on_exit
      self$root <- root
      self$config <- orderly2::orderly_config(self$root)
      if (!runner_has_git(self$root)) {
        cli::cli_abort(paste("Not starting server as orderly",
                             "root is not version controlled."))
      }

      # Connecting to Redis
      private$con <- redux::hiredis()

      # Create queue
      private$queue_id <- queue_id %||% orderly_queue_id()
      private$queue <- rrq::rrq_controller$new(private$queue_id,
                                               private$con)
      worker_config <- rrq::rrq_worker_config(heartbeat_period = 10)
      private$queue$worker_config_save("localhost", worker_config)
      private$start_workers(workers)
    },

    # Just until we add queue status for testing
    number_of_workers = function() {
      private$queue$worker_len()
    },

    #' @description stop workers and destroy queue if
    #' cleanup_on_exit is TRUE and Redis connection available
    finalize = function() {
      if (private$cleanup_on_exit && !is.null(private$con)) {
        private$queue$worker_stop(type = "kill")
        private$queue$destroy(delete = TRUE)
      }
    }
  ),
  private = list(
    cleanup_on_exit = NULL,
    con = NULL,
    queue = NULL,
    queue_id = NULL,
    start_workers = function(workers) {
      if (workers > 0L) {
        rrq::rrq_worker_spawn(private$queue, workers)
      }
      invisible()
    }
  ),
)

runner_has_git <- function(path) {
  nzchar(Sys.which("git")) && file.exists(file.path(path, ".git"))
}

orderly_queue_id <- function() {
  id <- Sys.getenv("ORDERLY_SERVER_QUEUE_ID", "")
  if (nzchar(id)) id else sprintf("orderly.runner:%s", ids::random_id())
}
