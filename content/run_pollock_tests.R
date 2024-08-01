#  Load packages and input data -------------------------------------------
# Load required packages
packages <- c("dplyr", "tidyr", "ggplot2", "TMB", "reshape2", "here", "remotes", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))

# Install the FIMS package from specific repositories
# install.packages("FIMS", repos = c("https://noaa-fims.r-universe.dev", "https://cloud.r-project.org"))
library(FIMS)

# Define the years and ages for the assessment
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages

# Source the script to prepare input data
source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))

# Run FIMS without helper functions ---------------------------------------
clear()
clear_logs()

# Source the script to prepare FIMS inputs without using helper functions
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs.R"))

# Create the TMB model and generate the report
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj1 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
rep1 <- obj1$report()

# Run FIMS with helper functions (by process) -------------------------------
clear()
clear_logs()

source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_process.R"))

success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj2 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)

rep2 <- obj2$report()

# Run FIMS with helper functions (by fleet) ---------------------------------
clear()
clear_logs()
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_fleet.R"))

success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj3 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)

rep3 <- obj3$report()


# Compare the reports from the three different runs -----------------------
# Check if reports from the first two runs are identical (original v.s. by process)
all.equal(rep1, rep2)
# Check if reports from the first and third runs are identical (original v.s. by fleet)
all.equal(rep1, rep3)

# Print the sorted parameter values from the three runs
print(cbind(sort(obj1$par), sort(obj2$par), sort(obj3$par)))

clear()
clear_logs()

# Optimize the models and compare the optimized parameters
opt1 <- with(obj1, nlminb(par, fn, gr))
opt2 <- with(obj2, nlminb(par, fn, gr))
opt3 <- with(obj3, nlminb(par, fn, gr))

# Compare optimized reports
all.equal(obj1$report(opt1$par), obj2$report(opt2$par))
all.equal(obj1$report(opt1$par), obj3$report(opt2$par))

# Check the sum of parameters difference
sum(obj1$par) - sum(obj2$par)
sum(obj1$par) - sum(obj3$par)

# Print the sorted optimized parameter values from the three runs
print(cbind(sort(obj1$par), sort(obj2$par), sort(obj3$par)))

clear()
clear_logs()

rm(list = ls())
