#  Load packages and input data -------------------------------------------
# Load required packages
packages <- c("dplyr", "tidyr", "ggplot2", "TMB", "reshape2", "here", "remotes", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))

# Install the FIMS package from specific repositories
# install.packages("FIMS", repos = c("https://noaa-fims.r-universe.dev", "https://cloud.r-project.org"))

# detach("package:FIMS", unload = TRUE)
library(FIMS)

# Define the years and ages for the assessment
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages

# Source the script to prepare input data
source(file.path("content", "R", "pk_prepare_dat.R"))

# Run FIMS without helper functions ---------------------------------------
clear()
clear_logs()

# Source the script to prepare FIMS inputs without using helper functions
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs.R"))

# Create the TMB model and generate the report
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
parameters$p[1] = 3.5
input <- list(data=age_frame, parameters=parameters, version='GOA pollock')

## devtools::load_all('C:/Users/cole.monnahan/FIMS')

fit <- fit_fims(input, verbose=FALSE)
fit <- fit_fims(input)
