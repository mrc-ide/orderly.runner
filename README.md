# orderly.runner

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/orderly.runner/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/orderly.runner/actions)
[![Build status]()](https://buildkite.com/mrc-ide/mrcide/orderly-dot-runner?branch=main)
[![codecov.io](https://codecov.io/github/mrc-ide/orderly.runner/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/orderly.runner?branch=main)
<!-- badges: end -->

A small HTTP server for running orderly reports.

The worker and API server are intended to be run as separate containers created from the same image, using the two different entrypoints in `docker/bin`.

## Installation

To install `orderly.runner` from GitHub:

```r
remotes::install_github("mrc-ide/orderly.runner", upgrade = FALSE)
```

## Local development

### Installation

To install for local development, you can use:

```r
devtools::install(dependencies = TRUE)
```

### Set the SSH key environment variable

To develop (or test) functions that use private git repositories, you will need to set the environment variable `TEST_PRIVATE_REPO_SSH_KEY`.

1. [Create an ssh key](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent) - crucially, without setting a passphrase.
1. Add it as a deploy key on [the private test repo](https://github.com/mrc-ide/orderly.runner-private-test-repo/) at [this link](https://github.com/mrc-ide/orderly.runner-private-test-repo/settings/keys).
1. Then set the private key as an environment variable:

```r
Sys.setenv(TEST_PRIVATE_REPO_SSH_KEY = paste(readLines("path/to/ssh/key"), collapse = "\n"))
```

### Testing (outside docker)

```r
devtools::test()
# or to run a specific test file:
devtools::test(filter='e2e')
```

If redis is not running, then tests relying on redis will be skipped.

### Running redis

To run a redis container, use `scripts/redis start`. Bring it down with `scripts/redis stop`.

## To run the full docker setup

1. Optionally modify `docker/test/examples` orderly reports. These will be used as reports in the container
1. Clear the decks with `docker/test/clear-test`.
1. Run `docker/test/run-test` to use the docker image from GitHub Container Registry, or `docker/test/run-test --local` to build from your local version for development. This will produce a `test-repo` directory to show you what was copied into the docker containers (you can create just this directory without running the docker containers by running `docker/test/setup-test-repo` if you want).
1. The server will be available at `localhost:8001`
1. To view the orderly root directory in the docker container (you may want to do this after workers have run orderly reports for example), run `docker/test/copy-orderly-root` and this will copy the contents to `docker/test/orderly-root-volume`
1. Finally to clear docker and remove `test-repo` and `orderly-root-volume` directories run `docker/test/clear-test`

## Notes for deploying

When running the server or worker containers, you should set `REDIS_URL` to point to your Redis instance.
You should also set the `ORDERLY_RUNNER_QUEUE_ID` to the same thing between server and worker containers so they connect to the same queue.

## License

MIT © Imperial College of Science, Technology and Medicine
