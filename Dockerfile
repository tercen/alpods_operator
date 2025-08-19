#FROM tercen/runtime-r44:4.4.3-7
FROM tercen/runtime-r40:4.0.4-6

COPY . /operator
WORKDIR /operator

# Install packages one by one to isolate issues
RUN R -e "install.packages(c('dplyr', 'tidyr', 'tibble'), repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore();"

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]