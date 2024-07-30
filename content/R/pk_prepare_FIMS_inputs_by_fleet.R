
set_selectivity <- function(form, initial_values, is_estimated, is_random_effect) {
  
  # Check if the current form is "LogisticSelectivity"
  if (form == "LogisticSelectivity") {
    # Create a new LogisticSelectivity object
    selectivity <- methods::new(LogisticSelectivity)

    # Set initial parameter values
    selectivity$inflection_point$value <- initial_values[1]
    selectivity$slope$value <- initial_values[2]

    # Set whether each parameter is estimated
    selectivity$inflection_point$estimated <- is_estimated[1]
    selectivity$slope$estimated <- is_estimated[2]

    # Set whether each parameter has random effects
    selectivity$inflection_point$is_random_effect <- is_random_effect[1]
    selectivity$slope$is_random_effect <- is_random_effect[2]
  }

  # Check if the current form is "DoubleLogisticSelectivity"
  if (form == "DoubleLogisticSelectivity") {
    # Create a new DoubleLogisticSelectivity object
    selectivity <- methods::new(DoubleLogisticSelectivity)

    # Set initial parameter values for both ascending and descending parts
    selectivity$inflection_point_asc$value <- initial_values[1]
    selectivity$slope_asc$value <- initial_values[2]
    selectivity$inflection_point_desc$value <- initial_values[3]
    selectivity$slope_desc$value <- initial_values[4]

    # Set whether each parameter is estimated
    selectivity$inflection_point_asc$estimated <- is_estimated[1]
    selectivity$slope_asc$estimated <- is_estimated[2]
    selectivity$inflection_point_desc$estimated <- is_estimated[3]
    selectivity$slope_desc$estimated <- is_estimated[4]

    # Set whether each parameter has random effects
    selectivity$inflection_point_asc$is_random_effect <- is_random_effect[1]
    selectivity$slope_asc$is_random_effect <- is_random_effect[2]
    selectivity$inflection_point_desc$is_random_effect <- is_random_effect[3]
    selectivity$slope_desc$is_random_effect <- is_random_effect[4]
  }
  return(selectivity)
}

set_fleet <- function(data = age_frame, is_estimated_obs_error, selectivity_ctl,
                      Fmort_ctl, catchability_ctl) {
  fishing_fleet_names <- dplyr::filter(
    .data = as.data.frame(data@data),
    type == "landings"
  ) |>
    dplyr::distinct(name) |>
    dplyr::pull(name)
  survey_fleet_names <- dplyr::filter(
    .data = as.data.frame(data@data),
    type == "index"
  ) |>
    dplyr::distinct(name) |>
    dplyr::pull(name)

  fleet_names <- c(fishing_fleet_names, survey_fleet_names)
  index_types <- c(
    rep("landings", length(fishing_fleet_names)),
    rep("index", length(survey_fleet_names))
  )
  nfleets <- length(fleet_names)

  nyears <- data@n_years
  nages <- data@n_ages

  # Initialize an empty list to store fleet objects
  index <- age_comp <- age_comp_uncertainty <-
    selectivity <- fleet <- vector(mode = "list", length = nfleets)

  for (i in 1:nfleets) {
    index[[i]] <- methods::new(Index, nyears)

    if (i %in% seq_along(fishing_fleet_names)) {
      index[[i]]$index_data <- FIMS::m_landings(data)
    } else {
      index[[i]]$index_data <- FIMS::m_index(data, fleet_names[i])
    }

    age_comp_uncertainty[[i]] <- dplyr::filter(
      .data = as.data.frame(data@data),
      name == fleet_names[i] & type == "age"
    ) |>
      dplyr::pull(uncertainty)

    age_comp[[i]] <- methods::new(AgeComp, nyears, nages)
    age_comp$age_comp_data <- FIMS::m_agecomp(data, fleet_names[i]) *
      age_comp_uncertainty[[i]]

    selectivity[[i]] <- set_selectivity(
      form = selectivity_ctl$form[[i]],
      initial_values = selectivity_ctl$initial_values[[i]], 
      is_estimated = selectivity_ctl$is_estimated[[i]], 
      is_random_effect = selectivity_ctl$is_random_effect[[i]]
    )

    fleet[[i]] <- methods::new(Fleet)

    # Set nyears and nages
    fleet[[i]]$nages <- nages
    fleet[[i]]$nyears <- nyears

    fleet[[i]]$log_obs_error <- dplyr::filter(
      .data = as.data.frame(data@data),
      name == fleet_names[i] & type == index_types[i]
    ) |>
      dplyr::pull(uncertainty)

    fleet[[i]]$estimate_obs_error <- is_estimated_obs_error[[i]]

    fleet[[i]]$is_survey <- Fmort_ctl$is_survey[[i]]
    fleet[[i]]$estimate_F <- Fmort_ctl$estimate_F[[i]]
    fleet[[i]]$random_F <- Fmort_ctl$random_F[[i]]


    fleet[[i]]$log_q <- catchability_ctl$log_q[[i]]
    fleet[[i]]$estimate_q <- catchability_ctl$estimate_q[[i]]
    fleet[[i]]$random_q <- catchability_ctl$random_q[[i]]

    fleet[[i]]$SetAgeCompLikelihood(i)
    fleet[[i]]$SetIndexLikelihood(i)
    fleet[[i]]$SetSelectivity(selectivity[[i]]$get_id())
    fleet[[i]]$SetObservedIndexData(index[[i]]$get_id())
    fleet[[i]]$SetObservedAgeCompData(age_comp[[i]]$get_id())
  }
  return(fleet)
}

## define the dimensions and global variables
library(FIMS)

years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages
source("R/pk_prepare_dat.R")

## test R functions
clear()
clear_logs()
fleet <- set_fleet(
  data = age_frame,
  is_estimated_obs_error = list(FALSE, FALSE, FALSE, FALSE),
  selectivity_ctl = list(
    form = list(
      "DoubleLogisticSelectivity",
      "DoubleLogisticSelectivity",
      "LogisticSelectivity",
      "DoubleLogisticSelectivity"
    ),
    initial_values = list(
      c(
        parfinal$inf1_fsh_mean, exp(parfinal$log_slp1_fsh_mean),
        parfinal$inf2_fsh_mean, exp(parfinal$log_slp2_fsh_mean)
      ),
      c(
        parfinal$inf1_srv2, exp(parfinal$log_slp1_srv2),
        parfinal$inf2_srv2, exp(parfinal$log_slp2_srv2)
      ),
      c(parfinal$inf1_srv3, exp(parfinal$log_slp1_srv3)),
      c(
        parfinal$inf1_srv6, exp(parfinal$log_slp1_srv6),
        parfinal$inf2_srv6, exp(parfinal$log_slp2_srv6)
      )
    ),
    is_estimated = list(
      rep(TRUE, 4),
      rep(TRUE, 4),
      rep(TRUE, 2),
      rep(TRUE, 4)
    ),
    is_random_effect = list(
      rep(FALSE, 4),
      rep(FALSE, 4),
      rep(FALSE, 2),
      rep(FALSE, 4)
    )
  ),
  Fmort_ctl = list(
    is_survey = list(FALSE, TRUE, TRUE, TRUE),
    estimate_F = list(TRUE, FALSE, FALSE, FALSE),
    random_F = list(FALSE, FALSE, FALSE, FALSE)
  ),
  catchability_ctl = list(
    log_q = list(
      0, parfinal$log_q2_mean,
      parfinal$log_q3_mean, parfinal$inf1_srv6
    ),
    estimate_q = list(FALSE, TRUE, TRUE, TRUE),
    random_q = list(FALSE, FALSE, FALSE, FALSE)
  )
)

# Population module
estimate_recdevs <- TRUE
# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
## methods::show(BevertonHoltRecruitment)
recruitment$log_sigma_recruit$value <- log(parfinal$sigmaR)
recruitment$log_rzero$value <- parfinal$mean_log_recruit + log(1e9)
recruitment$log_rzero$is_random_effect <- FALSE
recruitment$log_rzero$estimated <- TRUE
## note: do not set steepness exactly equal to 1, use 0.99 instead in ASAP run
recruitment$logit_steep$value <-
  -log(1.0 - .99999) + log(.99999 - 0.2)
recruitment$logit_steep$is_random_effect <- FALSE
recruitment$logit_steep$estimated <- FALSE
recruitment$estimate_log_devs <- estimate_recdevs
recruitment$log_devs <-  parfinal$dev_log_recruit[-1]

## growth  -- assumes single WAA vector for everything, based on
## Srv1 above
waa <- pkinput$dat$wt_srv1
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages <- ages
# NOTE: FIMS currently cannot use matrix of WAA, so have to ensure constant WAA over time in ASAP file for now
ewaa_growth$weights <- waa[1, ]
## NOTE: FIMS assumes SSB calculated at the start of the year, so
## need to adjust ASAP to do so as well for now, need to make
## timing of SSB calculation part of FIMS later
## maturity
## NOTE: for now tricking FIMS into thinking age 0 is age 1, so need to adjust A50 for maturity because FIMS calculations use ages 0-5+ instead of 1-6
maturity <- new(LogisticMaturity)
maturity$inflection_point$value <- 4.5
maturity$inflection_point$is_random_effect <- FALSE
maturity$inflection_point$estimated <- FALSE
maturity$slope$value <- 1.5
maturity$slope$is_random_effect <- FALSE
maturity$slope$estimated <- FALSE

# population
population <- new(Population)
population$log_M <-
  log(as.numeric(t(matrix(
    rep(pkfitfinal$rep$M, each = nyears), nrow = nyears
  ))))
population$estimate_M <- FALSE
population$log_init_naa <-
  c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)
population$estimate_init_naa <-
  FALSE # TRUE , NOTE: fixing at ASAP estimates to test SSB calculations
population$nages <- nages
population$ages <- ages
population$nfleets <- 2 # 1 fleet and 1 survey
population$nseasons <- nseasons
population$nyears <- nyears
## population$prop_female <- 1.0 # ASAP assumption
population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())


## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj3 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
## report values for the two models
rep3 <- obj3$report()

## Parameters are in a different order but seem to match
opt3 <- with(obj3, nlminb(par,fn,gr))
clear()
clear_logs()
rep3
rm(list = ls())
