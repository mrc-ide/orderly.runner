FROM rocker/r-ver:4.4

RUN install2.r pak

COPY DESCRIPTION /src/DESCRIPTION
RUN Rscript -e "pak::local_install_deps('/src')"

COPY . /src
RUN Rscript -e "pak::local_install('/src')"

COPY docker/bin /usr/local/bin/

# ENTRYPOINT for server is /usr/local/bin/orderly.runner.server
# ENTRYPOINT for worker is /usr/local/bin/orderly.runner.worker
