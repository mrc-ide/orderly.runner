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
       path = dat$path,
       host = dat$host)
}

main <- function(args = commandArgs(TRUE)) {
  dat <- parse_main(args)
  api_obj <- api(dat$path, dat$validate, dat$log_level)
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
  # assumes ORDERLY_RUNNER_QUEUE_ID is set
  queue <- Queue$new(dat$path)
  worker <- rrq::rrq_worker$new(
    queue$controller$queue_id,
    con = queue$controller$con
  )
  worker$loop()
}
