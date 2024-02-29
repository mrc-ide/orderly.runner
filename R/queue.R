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
      self$controller <- rrq::rrq_controller2(
        queue_id %||% orderly_queue_id()
      )
      worker_config <- rrq::rrq_worker_config(heartbeat_period = 10)
      rrq::rrq_worker_config_save2("localhost", worker_config,
                                   controller = self$controller)
    },

    # Just until we add queue status for testing
    number_of_workers = function() {
      rrq::rrq_worker_len(self$controller)
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
