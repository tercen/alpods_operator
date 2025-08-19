FROM tercen/runtime-r44:4.4.3-7

COPY . /operator
WORKDIR /operator

# Set compiler flags to not treat format-security warnings as errors, otherwise curl fails to install
ENV CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2"
ENV CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2"

# Install required packages
RUN R -e "install.packages(c('dplyr', 'tidyr', 'tibble'), repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore()"

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]