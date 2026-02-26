parse_main <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
orderly.runner.server [options] <path>

Options:
  --log-level=LEVEL  Log-level (off, info, all) [default: info]
  --validate         Enable json schema validation
  --host=HOST        Host to run api on [default: 0.0.0.0]
  --port=PORT        Port to run api on [default: 8001]"
  dat <- docopt::docopt(usage, args)
  list(log_level = dat$log_level,
       validate = dat$validate,
       port = as.integer(dat$port),
       repositories = dat$path,
       host = dat$host)
}

main <- function(args = commandArgs(TRUE)) {
  dat <- parse_main(args)
  api_obj <- api(dat$repositories, dat$validate, dat$log_level)
  api_obj$run(host = dat$host, port = dat$port)
}


parse_main_worker <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
orderly.runner.worker <path>"
  dat <- docopt::docopt(usage, args)
  list(path = dat$path)
}

main_worker <- function(args = commandArgs(TRUE)) {
  dat <- parse_main_worker(args)

  queue_id <- Sys.getenv("ORDERLY_RUNNER_QUEUE_ID")
  worker <- rrq::rrq_worker$new(queue_id, timeout_config = 30)

  fs::dir_create(dat$path)

  # This environment variable is used by the code submitted by the API.
  withr::with_envvar(c(ORDERLY_WORKER_STORAGE = dat$path), worker$loop())
}


parse_main_task_status <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
orderly.runner.task_status [options] <task_id>

Options:
  --queue-id=ID  Queue ID to connect to (overrides ORDERLY_RUNNER_QUEUE_ID)"
  dat <- docopt::docopt(usage, args)
  list(
    task_id = dat$task_id,
    queue_id = dat$queue_id %||% Sys.getenv("ORDERLY_RUNNER_QUEUE_ID", "")
  )
}

main_task_status <- function(args = commandArgs(TRUE)) {
  dat <- parse_main_task_status(args)

  if (!nzchar(dat$queue_id)) {
    stop("Queue ID must be provided via --queue-id or ORDERLY_RUNNER_QUEUE_ID")
  }

  controller <- rrq::rrq_controller(dat$queue_id)

  if (!rrq::rrq_task_exists(dat$task_id, controller = controller)) {
    stop(sprintf("Task '%s' does not exist in queue '%s'",
                 dat$task_id, dat$queue_id))
  }

  status <- rrq::rrq_task_status(dat$task_id, controller = controller)
  times <- rrq::rrq_task_times(dat$task_id, controller = controller)

  cat(sprintf("Task ID:       %s\n", dat$task_id))
  cat(sprintf("Status:        %s\n", status))
  cat(sprintf("Time queued:   %s\n", format(times[dat$task_id, 1])))
  cat(sprintf("Time started:  %s\n", format(times[dat$task_id, 2])))
  cat(sprintf("Time complete: %s\n", format(times[dat$task_id, 3])))

  if (status == "COMPLETE") {
    result <- rrq::rrq_task_result(dat$task_id, controller = controller)
    cat(sprintf("Packet ID:     %s\n", result))
  } else if (status == "ERROR") {
    result <- rrq::rrq_task_result(dat$task_id, controller = controller)
    cat(sprintf("Error:         %s\n", conditionMessage(result)))
  }

  logs <- rrq::rrq_task_log(dat$task_id, controller = controller)
  if (length(logs) > 0) {
    cat("\nLogs:\n")
    cat(paste(logs, collapse = "\n"), "\n")
  }

  invisible(NULL)
}
