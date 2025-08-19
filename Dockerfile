FROM tercen/runtime-r44:4.4.3-7

COPY . /operator
WORKDIR /operator

# Create system-wide Makevars to override strict compiler flags
RUN mkdir -p /usr/local/lib/R/etc && \
    echo "# Custom Makevars to fix compilation issues" > /usr/local/lib/R/etc/Makevars.site && \
    echo "CFLAGS = -g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2" >> /usr/local/lib/R/etc/Makevars.site && \
    echo "CXXFLAGS = -g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2" >> /usr/local/lib/R/etc/Makevars.site && \
    echo "CPPFLAGS = -I/usr/local/include" >> /usr/local/lib/R/etc/Makevars.site


# Set environment variables to relax compiler strictness
ENV CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2"
ENV CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Wdate-time -D_FORTIFY_SOURCE=2"

# Install packages one by one to isolate issues
RUN R -e "install.packages(c('dplyr', 'tidyr', 'tibble'), repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore();"

ENV TERCEN_SERVICE_URI https://tercen.com

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]