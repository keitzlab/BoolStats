# Use the official Rocker Shiny image (Ubuntu-based)
FROM rocker/shiny:latest

# Install required R packages for your app
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('lme4')"
RUN R -e "install.packages('multcomp')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('grDevices')"
RUN R -e "install.packages('dplyr')"

# Ensure shiny user is used to run Shiny Server (set user to shiny)
USER shiny

# Set the working directory inside the container
WORKDIR /srv/shiny-server

# Copy your Shiny app from your local machine into the container
COPY . /srv/shiny-server/

# Ensure the permissions of copied files are correct

# Expose port 3838 to the outside world (Shiny default port)
EXPOSE 3838

# Start the Shiny server when the container runs
CMD ["/usr/bin/shiny-server"]

