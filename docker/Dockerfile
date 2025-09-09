FROM rocker/r-ver:4

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libpq5 \
    libproj-dev \
    libproj25 \
    libudunits2-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r pak

COPY DESCRIPTION /src/DESCRIPTION
RUN Rscript -e "pak::local_install_deps('/src')"

COPY . /src
RUN Rscript -e "pak::local_install('/src')"

VOLUME /library

ENV R_LIBS=/library:/usr/local/lib/R/site-library:/usr/local/lib/R/library

COPY docker/bin /usr/local/bin/

# ENTRYPOINT for server is /usr/local/bin/orderly.runner.server
# ENTRYPOINT for worker is /usr/local/bin/orderly.runner.worker
