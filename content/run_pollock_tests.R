

packages <- c("dplyr", "tidyr", "ggplot2", "TMB", "reshape2", "here", "remotes", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))
## install.packages("FIMS", repos = c("https://noaa-fims.r-universe.dev", "https://cloud.r-project.org"))
library(FIMS)
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages
## nfleets <- 1
## This will fit the models bridging to FIMS (simplifying)
## source("fit_bridge_models.R")
## compare changes to model
# source("R/pk_prepare_dat.R")
source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))
## Some global settings which I Think we can ignore for now
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q2 <- TRUE
estimate_q3 <- TRUE
estimate_q6 <- TRUE
estimate_F <- TRUE
estimate_recdevs <- TRUE



clear()
clear_logs()
# source("R/pk_prepare_FIMS_inputs.R")
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs.R"))
## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj1 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
rep1 <- obj1$report()


## Organize input lists by process
clear()
clear_logs()
# source("R/pk_prepare_FIMS_inputs_by_process.R")
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_process.R"))
## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj2 <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
## report values for the two models
rep2 <- obj2$report()

## Test that the two models are identical
all.equal(rep1,rep2)

cbind(sort(obj1$par), sort(obj2$par))

clear()
clear_logs()

## Parameters are in a different order but seem to match
opt1 <- with(obj1, nlminb(par,fn,gr))
opt2 <- with(obj2, nlminb(par,fn,gr))
all.equal(obj1$report(opt1$par), obj2$report(opt2$par))
sum(obj1$par)-sum(obj2$par)

cbind(sort(opt1$par)- sort(opt2$par))
