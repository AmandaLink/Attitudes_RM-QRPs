FROM rocker/shiny:latest

# Update, install system dependencies, and clean up
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        git \
        libxml2-dev \
        libmagick++-dev \
        libssl-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libudunits2-dev \
        && apt-get clean && \
        rm -rf /var/lib/apt/lists/*

# Command to install standard R packages from CRAN; enter the list of required packages for your app here
RUN Rscript -e "install.packages(c('shiny', 'shinyjs', 'shinycssloaders', 'dplyr', 'ggplot2', 'readr', 'gtsummary', 'gt', 'glue', 'plotly', 'sf', 'rnaturalearth'), repos = 'http://cran.rstudio.com/', dependencies = TRUE)" 


RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/

# Use the shiny user and expose port 3838
USER shiny
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
