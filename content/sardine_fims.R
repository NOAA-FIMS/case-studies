#------------------------------------------------------------------------
#FIMS example for sardine

# options(max.print = 1000, device = 'windows', dplyr.summarise.inform = FALSE)
library(plyr)
library(reshape2)
library(dplyr)
library(tidyverse)
library(devtools)
# library(doParallel)
# devtools::install_github("https://github.com/r4ss/r4ss")
library(r4ss)
# library(patchwork)

# devtools::install_github("NOAA-FIMS/FIMS")
# 
# library(FIMS)

setwd("C:/Users/peter.kuriyama/SynologyDrive/Research/noaa/FIMS/")
devtools::load_all("../fims_v2/FIMS")

#Ageing error

#------------------------------------------------------------------------
#Call Chris stuff


#------------------------------------------------------------------------
#Changes made to simplified assessment

#Original base model
basemod <- SS_output("sardine_example/original_model", printstats = FALSE)
basedat <- SS_readdat("sardine_example/original_model/data_echo.ss_new")

#Simplified model
simpmod <- SS_output("sardine_example/simplified_model", printstats = FALSE)
ss3dat <- SS_readdat("sardine_example/simplified_model/data_echo.ss_new")

#-----------Format ss3dat to have FIMS data format
#type, name, age, datestart, dateend, value, unit, uncertainty
#landings, (index, ag

#Format this so that there's no missing year
ss3dat <- SS_readdat("sardine_example/expected_values/data_expval.ss")

#Fleet1 is Fishery; SUrvey1 is survey
catch <- ss3dat$catch[which(ss3dat$catch$year >= 2005),]


catch <- ss3dat$catch %>% dplyr::filter(year >= 2005)
fimscatch <- tibble(type = "landings", name = "fleet1",
                    age = NA, datestart = paste0(catch$year, "-01-01"),
                    dateend = paste0(catch$year, "-12-31"), value = catch$catch,
                    unit = "mt", uncertainty = 0.05)

cpue <- ss3dat$CPUE

fimsindex <- tibble(type = "index", name = "survey1",
                    age = NA, datestart = paste0(cpue$year, "-01-01"),
                    dateend = paste0(cpue$year, "-12-31"),
                    value = cpue$obs, unit = 'mt', uncertainty = .3)

acomps <- ss3dat$agecomp %>% select(Yr, FltSvy, Nsamp, paste0("a", 0:8)) %>%
  melt(id.var = c("Yr", "FltSvy", "Nsamp"))
acomps$age <- as.numeric(gsub("a", "", acomps$variable))
acomps <- acomps %>% distinct(FltSvy) %>% mutate(name = c("fleet1", "survey1")) %>%
  right_join(acomps)

acomps <- acomps %>% mutate(nn = name)

# write.csv(acomps, "sardine_acomps.csv")



fimsage <- tibble(type = "age", name = acomps$name,
                  age = acomps$age, datestart = paste0(acomps$Yr, "-01-01"),
                  dateend = paste0(acomps$Yr, "-12-31"),
                  value = acomps$value, unit = "", uncertainty = acomps$Nsamp)


#Add 1 to all age data? Ages can start at 0 but are indexed from 1?
fimsage$age <- fimsage$age + 1
fimsage$uncertainty <- 50

#Check missing years for age comps

fimscatch$value <- round(fimscatch$value) #Doesn't accept decimals??
fimsindex$unit <- ""

#Combine everything
fimsdat <- rbind(fimscatch, fimsindex, fimsage)

fimsdat$age <- as.integer(fimsdat$age) 
fimsdat$value <- as.numeric(fimsdat$value)

# write.csv(fimsdat, "sardinefims.csv")

#---------------------------------------
#Copied from Ian's Petrale example

# define the dimensions
# years <- seq(ss3dat$styr, ss3dat$endyr)
years <- 2005:2023

ages <- unique(fimsage$age) ##AGes 1:9, added 1 because no age 0?

# ages <- ss3dat$agebin_vector
nages <- length(ages)
nyears <- length(years)
nseasons <- 1

# ages <- 0:ss3dat$Nages # population ages in SS3, starts at age 0

#Are the characteristics in the fimsframe inherited by the data?

nfleets <- 1 #survey and one fishery

#------------------------
#FIMS data input
fimsdat <- as.data.frame(fimsdat)

age_frame <- FIMS::FIMSFrameAge(fimsdat) #Cannot be FIMSFrame

fishery_catch <- FIMS::m_landings(age_frame)
fishery_agecomp <- FIMS::m_agecomp(age_frame, "fleet1")
survey_index <- FIMS::m_index(age_frame, "survey1")
survey_agecomp <- FIMS::m_agecomp(age_frame, "survey1")

#---------------------------------------
#Fishing fleet index
fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
fish_index$index_data <- fishery_catch



# Q: I'm confused about FIMSFrame being set up with age comps in proportions
#   vs here needing age comps in numbers
# A: It's just not sorted out in FIMS yet, in the future this could be made simpler
fish_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "fleet1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n) |>
  round(1)


# switches to turn on or off estimation
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q <- FALSE #Fix at 1
estimate_F <- TRUE
estimate_recdevs <- TRUE
estimate_init_naa <- FALSE #I think I want to do this
estimate_log_rzero <- TRUE

#---------------------------------------
#Fishery module
#---------------------------------------
#Just one combined MexCal fleet

### set up fishery
## methods::show(DoubleLogisticSelectivity)
fish_selex <- methods::new(LogisticSelectivity)

#Use parameters close to those estimated in SS model  
fish_selex$inflection_point$value <- 1 #Fishery selectivity
fish_selex$inflection_point$is_random_effect <- FALSE
fish_selex$inflection_point$estimated <- estimate_fish_selex #Estimation on

fish_selex$slope$value <- 1
fish_selex$slope$is_random_effect <- FALSE
fish_selex$slope$estimated <- estimate_fish_selex #Estimation on

## create fleet object for fishing 
fish_fleet <- methods::new(Fleet)
fish_fleet$nages <- nages
fish_fleet$nyears <- nyears
fish_fleet$log_Fmort <- log(rep(0.00001, nyears))
fish_fleet$estimate_F <- estimate_F
fish_fleet$random_F <- FALSE
fish_fleet$log_q <- 0 #Not sure if this will be right
fish_fleet$estimate_q <- estimate_q
fish_fleet$random_q <- FALSE



fish_fleet$log_obs_error <- log(sqrt(log(catch$catch_se ^ 2 + 1))) #invalid 'pos' argument
fish_fleet$log_obs_error <- 1 #Has to be an integer?
fish_fleet$log_obs_error <- .5

# fish_fleet$log_obs_error <- rep(log(sqrt(log(0.05^2 + 1)))) #invalid pos argument

# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fish_fleet$SetObservedIndexData(fish_index$get_id()) #Don't understand this
fish_fleet$SetObservedAgeCompData(fish_age_comp$get_id())
fish_fleet$SetSelectivity(fish_selex$get_id())



## Setup survey
survey_fleet_index <- methods::new(Index, nyears)
survey_age_comp <- methods::new(AgeComp, nyears, nages)
survey_fleet_index$index_data <- survey_index

survey_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "survey1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey_selex <- new(LogisticSelectivity)
survey_selex$inflection_point$value <- 1
survey_selex$inflection_point$is_random_effect <- FALSE
survey_selex$inflection_point$estimated <- estimate_survey_selex
survey_selex$slope$value <- 1
survey_selex$slope$is_random_effect <- FALSE
survey_selex$slope$estimated <- estimate_survey_selex

## create fleet object for survey
survey_fleet <- methods::new(Fleet)
survey_fleet$is_survey <- TRUE
survey_fleet$nages <- nages
survey_fleet$nyears <- nyears
survey_fleet$estimate_F <- FALSE
survey_fleet$random_F <- FALSE
survey_fleet$log_q <- 0 #  catchability fixed ~1 = exp(0)
survey_fleet$estimate_q <- estimate_q
survey_fleet$random_q <- FALSE
# Q: why can't the index uncertainty come from FIMSFrame?
survey_fleet$log_obs_error <- 1

survey_fleet$SetAgeCompLikelihood(1)
survey_fleet$SetIndexLikelihood(1)
survey_fleet$SetSelectivity(survey_selex$get_id())
survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
survey_fleet$SetObservedAgeCompData(survey_age_comp$get_id())

# Population module

# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
# methods::show(BevertonHoltRecruitment)

# petrale sigmaR is 0.5
recruitment$log_sigma_recruit$value <- log(1.2)
# petrale log(R0) is around 9.6 (where R0 is in thousands)
# Q: do we need to account for SS3 R0 in thousands?
# recruitment$log_rzero$value <- log(1000) + ss3ctl$SR_parms["SR_LN(R0)", "INIT"]

#999



recruitment$log_rzero$value <- #------------------------------------------------------------------------
#FIMS example for sardine

# options(max.print = 1000, device = 'windows', dplyr.summarise.inform = FALSE)
library(plyr)
library(reshape2)
library(dplyr)
library(tidyverse)
library(devtools)
# library(doParallel)
# devtools::install_github("https://github.com/r4ss/r4ss")
library(r4ss)
# library(patchwork)

# devtools::install_github("NOAA-FIMS/FIMS")
# 
# library(FIMS)

setwd("C:/Users/peter.kuriyama/SynologyDrive/Research/noaa/FIMS/")
devtools::load_all("../fims_v2/FIMS")

#Ageing error

#------------------------------------------------------------------------
#Call Chris stuff


#------------------------------------------------------------------------
#Changes made to simplified assessment

#Original base model
basemod <- SS_output("sardine_example/original_model", printstats = FALSE)
basedat <- SS_readdat("sardine_example/original_model/data_echo.ss_new")

#Simplified model
simpmod <- SS_output("sardine_example/simplified_model", printstats = FALSE)
ss3dat <- SS_readdat("sardine_example/simplified_model/data_echo.ss_new")

#-----------Format ss3dat to have FIMS data format
#type, name, age, datestart, dateend, value, unit, uncertainty
#landings, (index, ag

#Format this so that there's no missing year
ss3dat <- SS_readdat("sardine_example/expected_values/data_expval.ss")

#Fleet1 is Fishery; SUrvey1 is survey
catch <- ss3dat$catch[which(ss3dat$catch$year >= 2005),]


catch <- ss3dat$catch %>% dplyr::filter(year >= 2005)
fimscatch <- tibble(type = "landings", name = "fleet1",
                    age = NA, datestart = paste0(catch$year, "-01-01"),
                    dateend = paste0(catch$year, "-12-31"), value = catch$catch,
                    unit = "mt", uncertainty = 0.05)

cpue <- ss3dat$CPUE

fimsindex <- tibble(type = "index", name = "survey1",
                    age = NA, datestart = paste0(cpue$year, "-01-01"),
                    dateend = paste0(cpue$year, "-12-31"),
                    value = cpue$obs, unit = 'mt', uncertainty = .3)

acomps <- ss3dat$agecomp %>% select(Yr, FltSvy, Nsamp, paste0("a", 0:8)) %>%
  melt(id.var = c("Yr", "FltSvy", "Nsamp"))
acomps$age <- as.numeric(gsub("a", "", acomps$variable))
acomps <- acomps %>% distinct(FltSvy) %>% mutate(name = c("fleet1", "survey1")) %>%
  right_join(acomps)

acomps <- acomps %>% mutate(nn = name)

# write.csv(acomps, "sardine_acomps.csv")



fimsage <- tibble(type = "age", name = acomps$name,
                  age = acomps$age, datestart = paste0(acomps$Yr, "-01-01"),
                  dateend = paste0(acomps$Yr, "-12-31"),
                  value = acomps$value, unit = "", uncertainty = acomps$Nsamp)


#Add 1 to all age data? Ages can start at 0 but are indexed from 1?
fimsage$age <- fimsage$age + 1
fimsage$uncertainty <- 50

#Check missing years for age comps

fimscatch$value <- round(fimscatch$value) #Doesn't accept decimals??
fimsindex$unit <- ""

#Combine everything
fimsdat <- rbind(fimscatch, fimsindex, fimsage)

fimsdat$age <- as.integer(fimsdat$age) 
fimsdat$value <- as.numeric(fimsdat$value)

# write.csv(fimsdat, "sardinefims.csv")

#---------------------------------------
#Copied from Ian's Petrale example

# define the dimensions
# years <- seq(ss3dat$styr, ss3dat$endyr)
years <- 2005:2023

ages <- unique(fimsage$age) ##AGes 1:9, added 1 because no age 0?

# ages <- ss3dat$agebin_vector
nages <- length(ages)
nyears <- length(years)
nseasons <- 1

# ages <- 0:ss3dat$Nages # population ages in SS3, starts at age 0

#Are the characteristics in the fimsframe inherited by the data?

nfleets <- 1 #survey and one fishery

#------------------------
#FIMS data input
fimsdat <- as.data.frame(fimsdat)

age_frame <- FIMS::FIMSFrameAge(fimsdat) #Cannot be FIMSFrame

fishery_catch <- FIMS::m_landings(age_frame)
fishery_agecomp <- FIMS::m_agecomp(age_frame, "fleet1")
survey_index <- FIMS::m_index(age_frame, "survey1")
survey_agecomp <- FIMS::m_agecomp(age_frame, "survey1")

#---------------------------------------
#Fishing fleet index
fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
fish_index$index_data <- fishery_catch



# Q: I'm confused about FIMSFrame being set up with age comps in proportions
#   vs here needing age comps in numbers
# A: It's just not sorted out in FIMS yet, in the future this could be made simpler
fish_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "fleet1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n) |>
  round(1)


# switches to turn on or off estimation
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q <- FALSE #Fix at 1
estimate_F <- TRUE
estimate_recdevs <- TRUE
estimate_init_naa <- FALSE #I think I want to do this
estimate_log_rzero <- TRUE

#---------------------------------------
#Fishery module
#---------------------------------------
#Just one combined MexCal fleet

### set up fishery
## methods::show(DoubleLogisticSelectivity)
fish_selex <- methods::new(LogisticSelectivity)

#Use parameters close to those estimated in SS model  
fish_selex$inflection_point$value <- 1 #Fishery selectivity
fish_selex$inflection_point$is_random_effect <- FALSE
fish_selex$inflection_point$estimated <- estimate_fish_selex #Estimation on

fish_selex$slope$value <- 1
fish_selex$slope$is_random_effect <- FALSE
fish_selex$slope$estimated <- estimate_fish_selex #Estimation on

## create fleet object for fishing 
fish_fleet <- methods::new(Fleet)
fish_fleet$nages <- nages
fish_fleet$nyears <- nyears
fish_fleet$log_Fmort <- log(rep(0.00001, nyears))
fish_fleet$estimate_F <- estimate_F
fish_fleet$random_F <- FALSE
fish_fleet$log_q <- 0 #Not sure if this will be right
fish_fleet$estimate_q <- estimate_q
fish_fleet$random_q <- FALSE



fish_fleet$log_obs_error <- log(sqrt(log(catch$catch_se ^ 2 + 1))) #invalid 'pos' argument
fish_fleet$log_obs_error <- 1 #Has to be an integer?
fish_fleet$log_obs_error <- .5

# fish_fleet$log_obs_error <- rep(log(sqrt(log(0.05^2 + 1)))) #invalid pos argument

# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fish_fleet$SetObservedIndexData(fish_index$get_id()) #Don't understand this
fish_fleet$SetObservedAgeCompData(fish_age_comp$get_id())
fish_fleet$SetSelectivity(fish_selex$get_id())



## Setup survey
survey_fleet_index <- methods::new(Index, nyears)
survey_age_comp <- methods::new(AgeComp, nyears, nages)
survey_fleet_index$index_data <- survey_index

survey_age_comp$age_comp_data <- age_frame@data |>
  dplyr::filter(type == "age" & name == "survey1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey_selex <- new(LogisticSelectivity)
survey_selex$inflection_point$value <- 1
survey_selex$inflection_point$is_random_effect <- FALSE
survey_selex$inflection_point$estimated <- estimate_survey_selex
survey_selex$slope$value <- 1
survey_selex$slope$is_random_effect <- FALSE
survey_selex$slope$estimated <- estimate_survey_selex

## create fleet object for survey
survey_fleet <- methods::new(Fleet)
survey_fleet$is_survey <- TRUE
survey_fleet$nages <- nages
survey_fleet$nyears <- nyears
survey_fleet$estimate_F <- FALSE
survey_fleet$random_F <- FALSE
survey_fleet$log_q <- 0 #  catchability fixed ~1 = exp(0)
survey_fleet$estimate_q <- estimate_q
survey_fleet$random_q <- FALSE
# Q: why can't the index uncertainty come from FIMSFrame?
survey_fleet$log_obs_error <- 1

survey_fleet$SetAgeCompLikelihood(1)
survey_fleet$SetIndexLikelihood(1)
survey_fleet$SetSelectivity(survey_selex$get_id())
survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
survey_fleet$SetObservedAgeCompData(survey_age_comp$get_id())

# Population module

# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
# methods::show(BevertonHoltRecruitment)

# petrale sigmaR is 0.5
recruitment$log_sigma_recruit$value <- log(1.2)
# petrale log(R0) is around 9.6 (where R0 is in thousands)
# Q: do we need to account for SS3 R0 in thousands?
# recruitment$log_rzero$value <- log(1000) + ss3ctl$SR_parms["SR_LN(R0)", "INIT"]

#999



recruitment$log_rzero$value <- simpmod$parameters %>% slice(grep("R0", Label)) %>% pull(Init)
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- estimate_log_rzero

#Sardine steepness fixed at 0.6
steep <- 0.6
recruitment$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE

recruitment$estimate_log_devs <- estimate_recdevs

# Q: why are parameters "log_devs" when output is "report$log_recruit_dev"?
# and are they multipliers, not deviations from zero?
# needed to change from 1 to 0 to get stable population
recruitment$log_devs <- rep(0, nyears) # set to no deviations (multiplier) to start

# growth
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages
# NOTE: getting weight-at-age vector from
# petrale_output$wtatage |>
#   dplyr::filter(Sex == 1 & Fleet == -1 & Yr == 1876) |>
#   dplyr::select(paste(0:40)) |>
#   round(4)


# simpmod$wtatage %>% dplyr::filter(Fleet == 2) %>% slice(1) %>%
#   select(as.character(0:10)) %>% unlist %>% as.vector

ewaa_growth$weights <- simpmod$wtatage %>% dplyr::filter(Fleet == 2) %>% slice(1) %>%
  select(as.character(0:10)) %>% unlist %>% as.vector

# maturity
maturity <- new(LogisticMaturity)
# approximate age-based equivalent to length-based maturity in petrale model
# based on looking at model$endgrowth |> dplyr::filter(Sex == 1) |> dplyr::select(Age_Beg, Len_Mat)
maturity$inflection_point$value <- -2
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- -1.2
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)
# petrale natural mortality is estimated around 0.14
M_value <- 1.3
population$log_M <- rep(log(M_value), nages * nyears)
population$estimate_M <- FALSE
# initial numbers at age based on R0 + mortality
init_naa <- exp(recruitment$log_rzero$value) * exp(-(ages - 1) * M_value)
init_naa[nages] <- init_naa[nages] / M_value # sum of infinite series
population$log_init_naa <- log(init_naa)
population$estimate_init_naa <- estimate_init_naa
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # fleets plus surveys
population$nseasons <- nseasons
population$nyears <- nyears
# population$proportion_female <- rep(0.5, nages)

population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())

#Rbomb out here


## Run FIMS model

#| warning: true
#| label: run-FIMS
#| output: false
#| eval: true

# make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr,
              control = list(eval.max = 10000, iter.max = 10000)
)
print(opt)






# sdr <- TMB::sdreport(obj)
# sdr_fixed <- summary(sdr, "fixed")
# print(sdr_fixed)
```

#Fleet module for both index and agecomp modules.
#-pass data objects into the fleet models, dimension indicated raw data
#-years with missing data give -999\

#Show agecomp or Index
#

























# 
# 
# #---------------------------------------
# #Sum catch values
# catch <- basedat$catch
# catch[which(catch$fleet == 2), "fleet"] <- 1
# 
# catch1 <- catch %>% group_by(year, fleet) %>% summarize(catch = sum(catch)) %>%
#   as.data.frame
# 
# catch1[which(catch1$fleet == 3), "fleet"] <- 2
# catch1$seas <- 1
# catch1$catch_se <- 0.05
# catch1 <- catch1 %>% select(names(catch))
# 
# newdat$catch <- catch1
# 
# 
# #---------------------------------------
# #Sum age comps
# acomps <- basedat$agecomp
# acomps[which(acomps$FltSvy == 2), "FltSvy"] <- 1
# acomps[which(acomps$FltSvy == 3), "FltSvy"] <- 2
# acomps[which(acomps$FltSvy == 4), "FltSvy"] <- 3
# 
# acomps1 <- acomps %>% group_by(Yr, FltSvy, Ageerr) %>% 
#   summarize(Nsamp = sum(Nsamp), a0 = sum(a0), a1 = sum(a1),
#             a2 = sum(a2), a3 = sum(a3), a4 = sum(a4),
#             a5 = sum(a5), a6 = sum(a6), a7 = sum(a7),
#             a8 = sum(a8)) %>% as.data.frame
# acomps1$Seas <- 6
# acomps1$Gender <- 0
# acomps1$Part <- 0
# acomps1$Lbin_lo <- -1
# acomps1$Lbin_hi <- -1
# acomps1 <- acomps1 %>% select(names(acomps))
# newdat$agecomp <- acomps1
# 
# #---------------------------------------
# #CPUE - Just use summer values
# cpue1 <- basedat$CPUE
# cpue1 <- cpue1 %>% filter(seas != 10)
# 
# newdat$CPUE <- cpue1
# SS_writedat(datlist = newdat, outfile = "sardine_example/simplified_model/simpdat.ss")
# 
# 
# 
# catch1[which(catch1$fleet == 2), "fleet"] <- 1
# catch1 <- catch1 %>% group_by(year, fleet) %>% summarize(catch = sum(catch))
# 
# #------------------------------------------------------------------------
# 
# 
# load("data_files/sardine2024.Rdata")
# 
# #------key to convert model years to calendar years
# times <- sardine2024$timeseries %>% select(Yr, Seas)
# times$calyear <- times$Yr
# times[which(times$Seas == 2), 'calyear'] <- times[which(times$Seas == 2), 'calyear'] + 1
# times$datestart <- paste0(times$calyear, "-01-01")
# times$dateend <- paste0(times$calyear, "-06-30")
# 
# #Change for seas 1
# times[which(times$Seas == 1), 'datestart'] <- paste0(times[which(times$Seas == 1), 'calyear'],
#                                                      "-07-01")
# times[which(times$Seas == 1), 'dateend'] <- paste0(times[which(times$Seas == 1), 'calyear'],
#                                                    "-12-31")
# 
# #------catch data
# #FIMS formatting
# #type, name, age, datestart, dateend, value, unit, uncertainty
# 
# #Catch 
# landings <- sardine2024$catch %>% select(Fleet, Yr, Seas, Obs, se) %>%
#   filter(Yr > 2004)
# landings$type <- "landings"
# landings$name <- paste0("fleet", landings$Fleet)
# landings <- landings %>% left_join(times, by = c("Yr", "Seas"))
# landings$unit <- "mt"
# landings$uncertainty <- landings$se
# 
# #Create key for changing model year to calendar year
# expand(Yr = 20)
# 
# times  <- tibble(Yr = 2005:2023)
# times <- times %>% expand(Yr, Seas = 1:2)
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------
# landings$cal_year
# 
# 
# landings %>% distinct(Seas) %>% mutate(start = c("-07-01", "01-01"))
# 
# 
# landings$datestart <- paste0(landings$Yr, "-07-01")
# landings$dateend <- paste0(landings$Yr, "-12-31")
# 
# landings[which(landings$Seas == 2), 'datestart']
# 
# 
# #Might have to keep the model summer only
# unit = "mt"
# uncertainty <- 0.05
# 
# #CPUE
# sardine2024$cpue
# 
# #Age comp
# 
# 
# #Weight-at-age
# 
# 
# remotes::install_github("NOAA-FIMS/FIMS")
# 
# 
# load_all()
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- estimate_log_rzero

#Sardine steepness fixed at 0.6
steep <- 0.6
recruitment$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE

recruitment$estimate_log_devs <- estimate_recdevs

# Q: why are parameters "log_devs" when output is "report$log_recruit_dev"?
# and are they multipliers, not deviations from zero?
# needed to change from 1 to 0 to get stable population
recruitment$log_devs <- rep(0, nyears) # set to no deviations (multiplier) to start

# growth
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages
# NOTE: getting weight-at-age vector from
# petrale_output$wtatage |>
#   dplyr::filter(Sex == 1 & Fleet == -1 & Yr == 1876) |>
#   dplyr::select(paste(0:40)) |>
#   round(4)


# simpmod$wtatage %>% dplyr::filter(Fleet == 2) %>% slice(1) %>%
#   select(as.character(0:10)) %>% unlist %>% as.vector

ewaa_growth$weights <- simpmod$wtatage %>% dplyr::filter(Fleet == 2) %>% slice(1) %>%
  select(as.character(0:10)) %>% unlist %>% as.vector

# maturity
maturity <- new(LogisticMaturity)
# approximate age-based equivalent to length-based maturity in petrale model
# based on looking at model$endgrowth |> dplyr::filter(Sex == 1) |> dplyr::select(Age_Beg, Len_Mat)
maturity$inflection_point$value <- -2
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- -1.2
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)
# petrale natural mortality is estimated around 0.14
M_value <- 1.3
population$log_M <- rep(log(M_value), nages * nyears)
population$estimate_M <- FALSE
# initial numbers at age based on R0 + mortality
init_naa <- exp(recruitment$log_rzero$value) * exp(-(ages - 1) * M_value)
init_naa[nages] <- init_naa[nages] / M_value # sum of infinite series
population$log_init_naa <- log(init_naa)
population$estimate_init_naa <- estimate_init_naa
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # fleets plus surveys
population$nseasons <- nseasons
population$nyears <- nyears
# population$proportion_female <- rep(0.5, nages)

population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())

#Rbomb out here


## Run FIMS model

#| warning: true
#| label: run-FIMS
#| output: false
#| eval: true

# make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr,
              control = list(eval.max = 10000, iter.max = 10000)
)
print(opt)






# sdr <- TMB::sdreport(obj)
# sdr_fixed <- summary(sdr, "fixed")
# print(sdr_fixed)
```

#Fleet module for both index and agecomp modules.
#-pass data objects into the fleet models, dimension indicated raw data
#-years with missing data give -999\

#Show agecomp or Index
#

























# 
# 
# #---------------------------------------
# #Sum catch values
# catch <- basedat$catch
# catch[which(catch$fleet == 2), "fleet"] <- 1
# 
# catch1 <- catch %>% group_by(year, fleet) %>% summarize(catch = sum(catch)) %>%
#   as.data.frame
# 
# catch1[which(catch1$fleet == 3), "fleet"] <- 2
# catch1$seas <- 1
# catch1$catch_se <- 0.05
# catch1 <- catch1 %>% select(names(catch))
# 
# newdat$catch <- catch1
# 
# 
# #---------------------------------------
# #Sum age comps
# acomps <- basedat$agecomp
# acomps[which(acomps$FltSvy == 2), "FltSvy"] <- 1
# acomps[which(acomps$FltSvy == 3), "FltSvy"] <- 2
# acomps[which(acomps$FltSvy == 4), "FltSvy"] <- 3
# 
# acomps1 <- acomps %>% group_by(Yr, FltSvy, Ageerr) %>% 
#   summarize(Nsamp = sum(Nsamp), a0 = sum(a0), a1 = sum(a1),
#             a2 = sum(a2), a3 = sum(a3), a4 = sum(a4),
#             a5 = sum(a5), a6 = sum(a6), a7 = sum(a7),
#             a8 = sum(a8)) %>% as.data.frame
# acomps1$Seas <- 6
# acomps1$Gender <- 0
# acomps1$Part <- 0
# acomps1$Lbin_lo <- -1
# acomps1$Lbin_hi <- -1
# acomps1 <- acomps1 %>% select(names(acomps))
# newdat$agecomp <- acomps1
# 
# #---------------------------------------
# #CPUE - Just use summer values
# cpue1 <- basedat$CPUE
# cpue1 <- cpue1 %>% filter(seas != 10)
# 
# newdat$CPUE <- cpue1
# SS_writedat(datlist = newdat, outfile = "sardine_example/simplified_model/simpdat.ss")
# 
# 
# 
# catch1[which(catch1$fleet == 2), "fleet"] <- 1
# catch1 <- catch1 %>% group_by(year, fleet) %>% summarize(catch = sum(catch))
# 
# #------------------------------------------------------------------------
# 
# 
# load("data_files/sardine2024.Rdata")
# 
# #------key to convert model years to calendar years
# times <- sardine2024$timeseries %>% select(Yr, Seas)
# times$calyear <- times$Yr
# times[which(times$Seas == 2), 'calyear'] <- times[which(times$Seas == 2), 'calyear'] + 1
# times$datestart <- paste0(times$calyear, "-01-01")
# times$dateend <- paste0(times$calyear, "-06-30")
# 
# #Change for seas 1
# times[which(times$Seas == 1), 'datestart'] <- paste0(times[which(times$Seas == 1), 'calyear'],
#                                                      "-07-01")
# times[which(times$Seas == 1), 'dateend'] <- paste0(times[which(times$Seas == 1), 'calyear'],
#                                                    "-12-31")
# 
# #------catch data
# #FIMS formatting
# #type, name, age, datestart, dateend, value, unit, uncertainty
# 
# #Catch 
# landings <- sardine2024$catch %>% select(Fleet, Yr, Seas, Obs, se) %>%
#   filter(Yr > 2004)
# landings$type <- "landings"
# landings$name <- paste0("fleet", landings$Fleet)
# landings <- landings %>% left_join(times, by = c("Yr", "Seas"))
# landings$unit <- "mt"
# landings$uncertainty <- landings$se
# 
# #Create key for changing model year to calendar year
# expand(Yr = 20)
# 
# times  <- tibble(Yr = 2005:2023)
# times <- times %>% expand(Yr, Seas = 1:2)
# 
# 
# 
# 
# 
# #------------------------------------------------------------------------
# landings$cal_year
# 
# 
# landings %>% distinct(Seas) %>% mutate(start = c("-07-01", "01-01"))
# 
# 
# landings$datestart <- paste0(landings$Yr, "-07-01")
# landings$dateend <- paste0(landings$Yr, "-12-31")
# 
# landings[which(landings$Seas == 2), 'datestart']
# 
# 
# #Might have to keep the model summer only
# unit = "mt"
# uncertainty <- 0.05
# 
# #CPUE
# sardine2024$cpue
# 
# #Age comp
# 
# 
# #Weight-at-age
# 
# 
# remotes::install_github("NOAA-FIMS/FIMS")
# 
# 
# load_all()