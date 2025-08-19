FROM tercen/runtime-r44:4.4.3-7

COPY . /operator
WORKDIR /operator



# Use Posit Package Manager for pre-compiled binaries (Ubuntu 20.04/focal)
ENV R_REPOS="https://packagemanager.rstudio.com/cran/__linux__/focal/latest"

# Install from binaries to avoid compilation issues
RUN R -e "options(repos = c(CRAN = Sys.getenv('R_REPOS'))); install.packages(c('dplyr', 'tidyr', 'tibble', 'curl', 'jsonlite', 'httr'), type = 'binary')"

RUN R -e "renv::restore();"

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]