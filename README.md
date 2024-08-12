# orderly.runner

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/orderly.runner/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/orderly.runner/actions)
[![Build status]()](https://buildkite.com/mrc-ide/mrcide/orderly-dot-runner?branch=main)
[![codecov.io](https://codecov.io/github/mrc-ide/orderly.runner/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/orderly.runner?branch=main)
<!-- badges: end -->

## Installation

To install `orderly.runner`:

```r
remotes::install_github("mrc-ide/orderly.runner", upgrade = FALSE)
```

## Testing and development

To run the full docker setup:

1. Optionally modify `docker/test/examples` orderly reports. These will be used as reports in the container
1. Run `docker/test/run-test` - this will produce a `test-repo` directory to show you what was copied into the docker containers (you can create just this directory without running the docker containers by running `docker/test/setup-test-repo` if you want)
1. The server will be available at `localhost:8001`
1. To view the orderly root directory in the docker container (you may want to do this after workers have run orderly reports for example), run `docker/test/copy-orderly-root` and this will copy the contents to `docker/test/orderly-root-volume`
1. Finally to clear docker and remove `test-repo` and `orderly-root-volume` directories run `docker/test/clear-test`


## Notes for deploying

When running the server or worker containers, you should have `REDIS_CONTAINER_NAME` env var set to connect to the redis container from the server and worker containers. You should also set the `ORDERLY_RUNNER_QUEUE_ID` to the same thing between server and worker containers so they connect to the same queue.

## License

MIT © Imperial College of Science, Technology and Medicine
