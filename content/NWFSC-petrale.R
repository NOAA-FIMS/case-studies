library(dplyr)
library(tidyr)
library(ggplot2)
require(FIMS)
library(TMB)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library(TMBhelper)
# remotes::install_github("r4ss/r4ss")
require(r4ss) 

R_version <- version$version.string
TMB_version <- packageDescription("TMB")$Version
FIMS_commit <- substr(packageDescription("FIMS")$GithubSHA1, 1, 7)


get_ss3_data <- function(dat, fleets){
  # create empty data frame
  res <- data.frame(type = character(),
                name = character(),
                age = integer(),
                datestart = character(),
                dateend = character(),
                value = double(),
                unit = character(),
                uncertainty = double())

#Q: is it true that we can only have a single landings fleet? m_landings() doesn't accept a fleet name.  

  # aggregate landings across fleets
  catch_by_year <- dat$catch |> 
    dplyr::group_by(year) |> 
    dplyr::summarize(catch = sum(catch), uncertainty = mean(catch_se))

  # convert landings to FIMSFrame format
  landings <- data.frame(type = "landings",
                     name = paste0("fleet1"), # landings aggregated to fleet 1
                     age = NA,
                     datestart = paste0(catch_by_year$year, "-01-01"),
                     dateend = paste0(catch_by_year$year, "-12-31"),
                     value = catch_by_year$catch,
                     unit = "mt",
                     uncertainty = catch_by_year$uncertainty)

  # check for any gaps in landings time series
  years <- min(catch_by_year$year):max(catch_by_year$year)
  if (!all(years %in% catch_by_year$year)) {
    stop("missing years in landings")
  }

  # convert indices to FIMSFrame format
  index_info <- dat$CPUE |> 
    dplyr::filter(index %in% fleets) |>
    dplyr::select(year, index, obs, se_log)

  # add -999 for missing years
  # create empty data frame for all combinations of year and fleet
  index_info_empty <- tidyr::expand_grid(
    year = years,
    index = fleets 
  ) |> dplyr::mutate(obs = -999, se_log = 1)
  # combine the two data frames and remove redundant rows
  index_info <- rbind(index_info, index_info_empty) |>
    dplyr::distinct(year, index, .keep_all = TRUE) |> 
    dplyr::arrange(index, year)

  indices <- data.frame(type = "index",
                      name = paste0("fleet", index_info$index),
                      age = NA,
                      datestart = paste0(index_info$year, "-01-01"),
                      dateend = paste0(index_info$year, "-12-31"),
                      value = index_info$obs,
                      unit = "",
                      uncertainty = index_info$se_log)

  # partially convert age comps (filter, make into long table)
  age_info <-
    dat$agecomp |>
    # specific to petrale:
    # fleet 4 used conditional age-at-length data with marginal observations
    # entered as fleet == -4 (to exclude from likelihood due to redundancy)
    # using only marginals in this case and exclude CAAL data
    dplyr::filter(FltSvy %in% c(1,2,3,-4)) |> 
    dplyr::mutate(FltSvy = abs(FltSvy)) |>
    dplyr::filter(FltSvy %in% fleets) |> 
    dplyr::select(!dplyr::starts_with("m", ignore.case = FALSE)) |> # exclude male comps
    tidyr::pivot_longer( # convert columns f1...f17 to values in a new "age" colum of a longer table
        cols = dplyr::starts_with("f", ignore.case = FALSE), 
        names_to = "age", 
        values_to = "value") |>
    dplyr::mutate(age = as.numeric(substring(age, first = 2))) |> # convert "f17" to 17
    dplyr::select(Yr, FltSvy, Nsamp, age, value)

  # add -999 for missing years
  # create empty data frame for all combinations of year, fleet, and age
  age_info_empty <- tidyr::expand_grid(
    Yr = years,
    FltSvy = fleets,
    age = ages
  ) |> dplyr::mutate(Nsamp = 1, value = -999)
  # combine the two data frames and remove redundant rows
  age_info <- rbind(age_info, age_info_empty) |>
    dplyr::distinct(Yr, FltSvy, age, .keep_all = TRUE) |> 
    dplyr::arrange(FltSvy, Yr, age)

  # finish converting age comps to FIMSFrame format
  agecomps <- data.frame(
    type = "age",
    name = paste0("fleet", abs(age_info$FltSvy)), # abs to include fleet == -4
    age = age_info$age,
    datestart = paste0(age_info$Yr, "-01-01"),
    dateend = paste0(age_info$Yr, "-12-31"),
    value = age_info$value + 0.001, # add constant to avoid 0 values
    unit = "",
#Q: should uncertainty here be the total sample size across bins, or the samples within the bin?    
    #uncertainty = round(age_info$Nsamp * age_info$value)
    uncertainty = round(age_info$Nsamp)
  )

  # combine all data sources
  res <- rbind(res, landings, indices, agecomps)
}


# read SS3 input files from petrale sole assessment on github
petrale_input <- r4ss::SS_read("https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/")

# # reading output doesn't work from github, so just using hard-wired values from model output
# petrale_output <- r4ss::SS_output("https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a034.001/")

# generic names for SS3 data and control files could be useful in future generalized version of this code
ss3dat <- petrale_input$dat
ss3ctl <- petrale_input$ctl

# define the dimensions
years <- seq(ss3dat$styr, ss3dat$endyr)
nyears <- length(years)
nseasons <- 1
#ages <- 0:ss3dat$Nages # population ages in SS3, starts at age 0
ages <- 1:17 # same as data bins
nages <- length(ages)

# use function defined above to extract data for FIMS
mydat <- get_ss3_data(ss3dat, fleets = c(1,4))

# remove fleets 2 and 3 and rename fleet4 as fleet2
mydat <- mydat |> 
  dplyr::filter(name %in% c("fleet1", "fleet4")) |>
  dplyr::mutate(name = dplyr::case_when(
    name == "fleet1" ~ name,
    name == "fleet4" ~ "fleet2" # change fleet4 to fleet2
  ))

# filter for just years with no missing age or index data
# in spite of filling in -999 values earlier, just in case
years <- 2003:2019
nyears <- length(years)
mydat <- mydat |> dplyr::filter(datestart %in% paste0(years, "-01-01"))


age_frame <- FIMS::FIMSFrameAge(mydat) # similar to FIMSFrame() but includes ages
# in the future FIMSFrame() and FIMSFrameAge() will likely be merged
fishery_catch <- FIMS::m_landings(age_frame) # filtering for the landings only
fishery_agecomp <- FIMS::m_agecomp(age_frame, "fleet1") # filtering for ages from fleet 1
survey_index <- FIMS::m_index(age_frame, "fleet2") # filtering for index data from fleet 2
survey_agecomp <- FIMS::m_agecomp(age_frame, "fleet2") # filtering for ages from fleet 2



fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
fish_index$index_data <- fishery_catch
#Q: I'm confused about FIMSFrame being set up with age comps in proportions 
#   vs here needing age comps in numbers
# just not sorted out yet, in the future this could be made simpler
fish_age_comp$age_comp_data <- age_frame@data |> 
  dplyr::filter(type == "age" & name == "fleet1") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n) |> 
  round(1)

# switches to turn on or off estimation
estimate_fish_selex <- FALSE
estimate_survey_selex <- FALSE
estimate_q <- FALSE
estimate_F <- FALSE
estimate_recdevs <- FALSE

### set up fishery
## methods::show(DoubleLogisticSelectivity)
fish_selex <- methods::new(LogisticSelectivity)

# SS3 model had length-based selectivity which leads to sex-specific
# age-based selectivity due to sexually-dimorphic growth. 
# I didn't bother to calculate an age-based inflection point averaged over sexes
fish_selex$inflection_point$value <- 10 
fish_selex$inflection_point$is_random_effect <- FALSE
fish_selex$inflection_point$estimated <- estimate_fish_selex
fish_selex$slope$value <- 2
fish_selex$slope$is_random_effect <- FALSE
fish_selex$slope$estimated <- estimate_fish_selex

## create fleet object for fishing fleet
fish_fleet <- methods::new(Fleet)
fish_fleet$nages <- nages
fish_fleet$nyears <- nyears
fish_fleet$log_Fmort <- rep(0.1, nyears)
fish_fleet$estimate_F <- estimate_F
fish_fleet$random_F <- FALSE
fish_fleet$log_q <- 0 
fish_fleet$estimate_q <- FALSE
fish_fleet$random_q <- FALSE
fish_fleet$log_obs_error <- rep(log(sqrt(log(0.01^2 + 1))), nyears)

# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fish_fleet$SetObservedIndexData(fish_index$get_id())
fish_fleet$SetObservedAgeCompData(fish_age_comp$get_id())
fish_fleet$SetSelectivity(fish_selex$get_id())

## Setup survey
survey_fleet_index <- methods::new(Index, nyears)
survey_age_comp <- methods::new(AgeComp, nyears, nages)
survey_fleet_index$index_data <- survey_index
survey_age_comp$age_comp_data <- mydat |> 
  dplyr::filter(type == "age" & name == "fleet2") |>
  dplyr::mutate(n = value * uncertainty) |>
  dplyr::pull(n)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey_selex <- new(LogisticSelectivity)
survey_selex$inflection_point$value <- 6
survey_selex$inflection_point$is_random_effect <- FALSE
survey_selex$inflection_point$estimated <- estimate_survey_selex
survey_selex$slope$value <- 2
survey_selex$slope$is_random_effect <- FALSE
survey_selex$slope$estimated <- estimate_survey_selex

## create fleet object for survey
survey_fleet <- methods::new(Fleet)
survey_fleet$is_survey <- TRUE
survey_fleet$nages <- nages
survey_fleet$nyears <- nyears
survey_fleet$estimate_F <- FALSE
survey_fleet$random_F <- FALSE
survey_fleet$log_q <- 1.4 # petrale sole catchability estimated ~4.0 = exp(1.4)
survey_fleet$estimate_q <- estimate_q
survey_fleet$random_q <- FALSE
#Q: why can't the index uncertainty come from FIMSFrame?
survey_fleet$log_obs_error <- age_frame@data |> 
  dplyr::filter(type == "index" & name == "fleet2") |> 
  dplyr::pull(uncertainty) |> 
  log()

survey_fleet$SetAgeCompLikelihood(1)
survey_fleet$SetIndexLikelihood(1)
survey_fleet$SetSelectivity(survey_selex$get_id())
survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
survey_fleet$SetObservedAgeCompData(survey_age_comp$get_id())

# Population module

# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
#methods::show(BevertonHoltRecruitment)

# petrale sigmaR is 0.5
recruitment$log_sigma_recruit$value <- log(ss3ctl$SR_parms["SR_sigmaR", "INIT"])
# petrale log(R0) is around 9.6 (where R0 is in thousands)
#Q: do we need to account for SS3 R0 in thousands?
recruitment$log_rzero$value <- log(1000) + ss3ctl$SR_parms["SR_LN(R0)", "INIT"]
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- FALSE
# petrale steepness is fixed at 0.8
steep <- ss3ctl$SR_parms["SR_BH_steep", "INIT"]
recruitment$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE

recruitment$estimate_log_devs <- FALSE
recruitment$log_devs <- rep(1.0, nyears) # set to no deviations (multiplier) to start, just like ASAP

# growth
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages
# NOTE: getting weight-at-age vector from 
# petrale_output$wtatage |> 
#   dplyr::filter(Sex == 1 & Fleet == -1 & Yr == 1876) |> 
#   dplyr::select(paste(0:40)) |>
#   round(4)
ewaa_growth$weights <- c(
    0.0010, 0.0148, 0.0617, 0.1449, 0.2570, 0.3876, 0.5260, 0.6640, 0.7957, 0.9175, 
    1.0273, 1.1247, 1.2097, 1.2831, 1.3460, 1.3994, 1.4446, 1.4821, 1.5132, 1.5392, 
    1.5609, 1.5789, 1.5939, 1.6063, 1.6165, 1.6251, 1.6321, 1.6379, 1.6427, 1.6467, 
    1.6499, 1.6526, 1.6549, 1.6567, 1.6582, 1.6595, 1.6605, 1.6613, 1.6620, 1.6626, 
    1.6633
  )[1:nages]

# maturity
maturity <- new(LogisticMaturity)
# approximate age-based equivalent to length-based maturity in petrale model
# based on looking at model$endgrowth |> dplyr::filter(Sex == 1) |> dplyr::select(Age_Beg, Len_Mat)
maturity$inflection_point$value <- 6.5 
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- 2 # arbitrary guess
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)
# petrale natural mortality is estimated around 0.14
population$log_M <- rep(log(ss3ctl$MG_parms["NatM_p_1_Fem_GP_1", "INIT"]), nages)
population$estimate_M <- FALSE
#Q: rescale numbers at age by 1000?
population$log_init_naa <- log(1000*exp(recruitment$log_rzero$value) * exp(-ages))
population$estimate_init_naa <- FALSE 
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # fleets plus surveys
population$nseasons <- nseasons
population$nyears <- nyears
#population$proportion_female <- rep(0.5, nages)

population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())

# make FIMS model
success <- CreateTMBModel()

parameters <- list(p = get_fixed())
obj <- MakeADFun(data = list(), parameters, DLL = "FIMS", silent = FALSE)
