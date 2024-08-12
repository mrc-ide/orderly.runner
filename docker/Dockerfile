FROM rocker/r-ver:4.1

RUN apt-get update &&  apt-get install -y --no-install-recommends \
        git \
        libcurl4-openssl-dev \
        libhiredis-dev \
        libssl-dev \
        zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Without this, we are unable to pick up more recent packages
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

COPY DESCRIPTION /tmp/DESCRIPTION

RUN install2.r --error remotes &&  \
        Rscript -e 'remotes::install_deps("/tmp")'

COPY . /src
RUN R CMD INSTALL --install-tests /src && rm -rf /src

COPY docker/bin /usr/local/bin/

RUN git config --global --add safe.directory "*"
RUN echo ".packit" > /.gitignore
RUN git config --global core.excludesFile "/.gitignore"

# ENTRYPOINT for server is /usr/local/bin/orderly.runner.server
# ENTRYPOINT for worker is /usr/local/bin/orderly.runner.worker
