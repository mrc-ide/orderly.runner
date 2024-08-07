FROM rocker/r-ver:4.0.4

RUN apt-get update &&  apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Without this, we are unable to pick up more recent packages
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

COPY DESCRIPTION /tmp/DESCRIPTION

RUN install2.r --error remotes &&  \
        Rscript -e 'remotes::install_deps("/tmp")' && \
        Rscript -e 'remotes::install_github("mrc-ide/orderly2", upgrade = FALSE)' && \
        Rscript -e 'remotes::install_github("mrc-ide/rrq", upgrade = FALSE)' && \
        Rscript -e 'remotes::install_github("reside-ic/porcelain", upgrade = FALSE)'

COPY . /src
RUN R CMD INSTALL --install-tests /src && rm -rf /src

ENV ORDERLY_RUNNER_QUEUE_ID=orderly.runner

COPY docker/bin /usr/local/bin/

# ENTRYPOINT for server is /usr/local/bin/orderly.runner.server
# ENTRYPOINT for worker is /usr/local/bin/orderly.runner.worker