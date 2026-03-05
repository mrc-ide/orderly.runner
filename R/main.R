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
