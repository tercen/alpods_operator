FROM tercen/runtime-r44:4.4.3-7

COPY . /operator
WORKDIR /operator



# Install required packages
RUN R -e "install.packages(c('dplyr', 'tidyr', 'tibble'), repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore(exclude = 'curl'); install.packages('curl', repos = 'https://cloud.r-project.org')"

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]