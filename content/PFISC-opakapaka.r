## Script to run Opakapaka case study in FIMS. 
## Created by Meg Oshima

library(dplyr)
library(tidyr)
library(ggplot2)
require(FIMS)
library(TMB)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library(TMBhelper)
# remotes::install_github("r4ss/r4ss")
require(r4ss)

## Version documentation
R_version <- version$version.string
TMB_version <- packageDescription("TMB")$Version
FIMS_commit <- substr(packageDescription("FIMS")$GithubSHA1, 1, 7)

## Read in SS input files
load(file.path(getwd(), "content", "data_files", "opaka_model2.RDS"))
#ss3dat <- ss3output$dat
#ss3ctl <- ss3output$ctl
#ss3dat <- SS_readdat_3.30(file = file.path(getwd(), "..", "Opaka-FIMS-Case-Study", "Model", "07_subset_yrs", "data.ss"))
## Function written by Ian Taylor to get SS3 data into FIMSframeAge format
source("./content/R/get_ss3_data.r")

## Define the dimensions
### years from 1949 to 2023
years <- seq(ss3dat$styr, ss3dat$endyr)
nyears <- length(years)
nseasons <- 1
ages <- ss3dat$agebin_vector
nages <- length(ages)

#head(data_mile1)
## Use R/get_ss3_data function to get from data.ss to FIMSframeAge
opaka_dat <- get_ss3_data(ss3dat, fleets = c(1,2), ages = ages) #trying with just one fishery and one survey first
head(opaka_dat)  #landings get aggregated into one fleet if you have multiple (commercial + non-commercial)
head(ss3dat$catch)
opaka_dat |> filter(type == "index") |> tail()
ss3dat$CPUE |> filter(index == 2)
opaka_dat |> filter(type == "age") |> filter(datestart >= "2017-01-01") |> filter(name == "fleet2")  |> head()
head(ss3dat$agecomp)

str(opaka_dat)

##use subset of data for years where BFISH index exists
#opaka_dat_sub <- opaka_dat |> filter(as.Date(datestart) > as.Date("2016-01-01"))
#nyears <- length(unique(opaka_dat_sub$datestart))
## Set up FIMS model

#age data
age_frame <- FIMS::FIMSFrameAge(opaka_dat)
age_frame@ages
age_frame@nages
head(age_frame@data)
tail(age_frame@data)
age_frame@fleets
age_frame@start_year
age_frame@end_year #not sure why this is saying 9 when other components are correct

#plot data components in age_frame
age_frame@data |> 
ggplot() +
geom_line(aes(x = lubridate::year(datestart), y = value, color = name)) + 
facet_wrap(~type, scales = "free") + 
theme_bw() +
labs(x = "Year", y = "")

#fishery data
fishery_catch <- FIMS::m_landings(age_frame)
head(fishery_catch)
fishery_agecommp <- FIMS::m_agecomp(age_frame, "fleet1")
#fishery_index <- FIMS::m_index(age_frame, "fleet1")

#survey data
survey_index <- FIMS::m_index(age_frame, "fleet2")
survey_agecomp <- FIMS::m_agecomp(age_frame, "fleet2")

## Creating modules 

##Fleet module
#show(Index)
#show(AgeComp)
fishing_fleet_index <- methods::new(Index, nyears)
fishing_fleet_age_comp <- methods::new(AgeComp, nyears, nages)
#Q: Don't understand why we put the fishery catch in the index data? 
fishing_fleet_index$index_data <- fishery_catch
fishing_fleet_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "fleet1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n) |>
  round(1)
##Fleet selectivity
# switches to turn on or off estimation
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q <- TRUE
estimate_F <- TRUE
estimate_recdevs <- TRUE
estimate_init_naa <- FALSE
estimate_log_rzero <- TRUE
estimate_random_effect <- FALSE

#methods::show(LogisticSelectivity)
#estimating logistic selectivity for fishery
fishing_fleet_selectivity <- methods::new(LogisticSelectivity)
fishing_fleet_selectivity$inflection_point$value <- 2.0 #starting value
fishing_fleet_selectivity$inflection_point$is_random_effect <- estimate_random_effect
fishing_fleet_selectivity$inflection_point$estimated <- estimate_fish_selex
fishing_fleet_selectivity$slope$value <- 1.0 #starting value
fishing_fleet_selectivity$slope$is_random_effect <- estimate_random_effect
fishing_fleet_selectivity$slope$estimated <- estimate_fish_selex

# Create fleet module
fishing_fleet <- methods::new(Fleet)
# Set nyears and nages
fishing_fleet$nages <- nages
fishing_fleet$nyears <- nyears
# Set values for log_Fmort
fishing_fleet$log_Fmort <- log(rep(0.00001, nyears))
# Turn on estimation for F
fishing_fleet$estimate_F <- estimate_F
fishing_fleet$random_F <- estimate_random_effect
# Set value for log_q
fishing_fleet$log_q <- 0
fishing_fleet$estimate_q <- estimate_q
fishing_fleet$random_q <- estimate_random_effect
fishing_fleet$log_obs_error <- rep(log(sqrt(log(0.01^2 + 1))), nyears)
#fishing_fleet$estimate_obs_error <- FALSE
# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fishing_fleet$SetObservedIndexData(fishing_fleet_index$get_id())
fishing_fleet$SetObservedAgeCompData(fishing_fleet_age_comp$get_id())
fishing_fleet$SetSelectivity(fishing_fleet_selectivity$get_id())

## Survey Module
#Q: "We will now repeat the steps from Fleet to set up the Survey. A survey object is essentially the same as a fleet object with a catchability (q) variable." -FIMS Demo
# Does that mean fleet doesn't have a q? What if you have CPUE for fleet? 

#Survey data
# fleet index data
survey_fleet_index <- methods::new(Index, nyears)
# survey age composition data
survey_fleet_age_comp <- methods::new(AgeComp, nyears, nages)
survey_fleet_index$index_data <- survey_index
# Effective sampling size is 200
survey_fleet_age_comp$age_comp_data <- opaka_dat |>
  dplyr::filter(type == "age") |>
  dplyr::filter(name == "fleet2") |> 
  dplyr::mutate(n = value * uncertainty) |> 
  dplyr::pull(n)
# survey selectivity
survey_fleet_selectivity <- new(LogisticSelectivity)
survey_fleet_selectivity$inflection_point$value <- 1.5
survey_fleet_selectivity$inflection_point$is_random_effect <- estimate_random_effect
survey_fleet_selectivity$inflection_point$estimated <- estimate_survey_selex
survey_fleet_selectivity$slope$value <- 2.0
survey_fleet_selectivity$slope$is_random_effect <- estimate_random_effect
survey_fleet_selectivity$slope$estimated <- estimate_survey_selex

#creating survey object
survey_fleet <- methods::new(Fleet)
survey_fleet$is_survey <- TRUE
survey_fleet$nages <- nages
survey_fleet$nyears <- nyears
survey_fleet$estimate_F <- FALSE
survey_fleet$random_F <- FALSE
survey_fleet$log_q <- log(2.94455e-07)
#Q: can I have 2 surveys? If so, where am I specifying there are 2? Do I create a second survey fleet module? For now, start with just fleet1 and fleet2, then add in fleet3 to try a third survey module
survey_fleet$estimate_q <- TRUE
survey_fleet$random_q <- FALSE
# sd = sqrt(log(cv^2 + 1)), sd is log transformed
survey_fleet$log_obs_error <- age_frame@data |>
  dplyr::filter(type == "index" & name == "fleet2") |>
  dplyr::pull(uncertainty) |>
  log()
#survey_fleet$estimate_obs_error <- FALSE
survey_fleet$SetAgeCompLikelihood(1)
survey_fleet$SetIndexLikelihood(1)
survey_fleet$SetSelectivity(survey_fleet_selectivity$get_id())
survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
survey_fleet$SetObservedAgeCompData(survey_fleet_age_comp$get_id())

## Population Module

#Recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
#There are three parameters we need to set-up: *log_sigma_recruit*, *log_rzero*, and *logit_steep*.
recruitment$log_sigma_recruit$value <- log(0.52)
recruitment$log_sigma_recruit$estimated
recruitment$log_rzero$value <- 5.58284
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- TRUE
recruitment$logit_steep$value <- -log(1.0 - 0.76) + log(0.76 - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE

recruitment$estimate_log_devs <- estimate_recdevs
recruitment$log_devs <- rep(0, nyears) #set to no deviations to start, then try adding in from SS

#Growth
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages

#weights <- ss3rep$wtatage |> 
#dplyr::filter(Sex == 1 & Fleet == 1 & Yr == 1949) |> 
#dplyr::select(paste(1:21)) |> 
#round(4)
#ewaa_growth$weights <- unlist(weights)

ewaa_growth$weights <- c(0.041, 0.2942, 0.7766, 1.235, 1.7445, 2.2816, 2.7782, 3.2152, 3.5887, 3.9014, 4.1592, 4.3694, 4.5392, 4.6756, 4.7846, 4.8713, 4.9401, 4.9945, 5.0376, 5.0715, 5.0983)
# maturity
maturity <- new(LogisticMaturity)
# approximate age-based equivalent to length-based maturity in petrale model
# based on looking at ss3rep$endgrowth |> dplyr::filter(Sex == 1) |> dplyr::select(Age_Beg, Len_Mat)
maturity$inflection_point$value <- 3.5
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- 2 # arbitrary guess, not sure how to relate this to SS value of -2.26
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)

M_value <- ss3ctl$MG_parms["NatM_p_1_Fem_GP_1", "INIT"]
population$log_M <- rep(log(M_value), nages * nyears)
population$estimate_M <- FALSE
# initial numbers at age based on R0 + mortality
init_naa <- exp(recruitment$log_rzero$value) * exp(-(ages - 1) * M_value)
init_naa[nages] <- init_naa[nages] / M_value # sum of infinite series
population$log_init_naa <- log(init_naa)
population$estimate_init_naa <- estimate_init_naa
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # fleet plus surveys
population$nseasons <- 1
population$nyears <- nyears
# population$proportion_female <- rep(0.5, nages)

population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())

## Create FIMS Model and Make TMB Function
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(eval.max = 10000, iter.max = 10000))
#print(opt)
report <- obj$report()
head(report$ssb)

report$log_recruit_dev
report$biomass
report$M
report$F_mort

plot(x = years, y = ss3dat$catch$catch[2:76], pch = 16)
lines(x = years, y = report$exp_catch[[1]], col = "blue")

plot(x = c(years, max(years) + 1), y = report$ssb[[1]], type = "l")
report$exp_index
ss3dat$CPUE

plot(x = years, y = report$exp_index[[2]], type = "l")
