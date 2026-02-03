# Attitudes_RM-QRPs

## How the dashboard is built
The dashboard is built using R (4.5.1) with Shiny. The code of the Shiny app can be found at /app/app.R; the app can be run locally e.g. using RStudio. The R packages used are listed in the code and the renv.lock file.

The underlying aggregated data used for the app is located in the app folder, as .csv and .rds objects.

In addition, this repository contains commands to build an image with the app code and data (see Dockerfile for settings) and to push to DockerHub through GitHub actions (see .github/workflows/docker-image.yml for settings).

## Contact
