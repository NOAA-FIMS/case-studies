library(FIMS)
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
}

## define the dimensions and global variables
years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages
## nfleets <- 1
## This will fit the models bridging to FIMS (simplifying)
## source("fit_bridge_models.R")
## compare changes to model
pkfitfinal <- readRDS("content/data_files/pkfitfinal.RDS")
pkfit0 <- readRDS("content/data_files/pkfit0.RDS")
parfinal <- pkfitfinal$obj$env$parList()
pkinput0 <- readRDS("content/data_files/pkinput0.RDS")
fimsdat <- pkdat0 <- pkinput0$dat
pkinput <- readRDS("content/data_files/pkinput.RDS")

## build a FIMS and PK data set that match
##  need to fill missing years with -999 so it's ignored in FIMS
ind2 <- 0 * pkfit0$rep$Eindxsurv2 - 999
ind2[which(years %in% fimsdat$srvyrs2)] <- fimsdat$indxsurv2
CV2 <- rep(1, length = nyears) # actually SE in log space
CV2[which(years %in% fimsdat$srvyrs2)] <- fimsdat$indxsurv_log_sd2
paa2 <- pkfit0$rep$Esrvp2 * 0 - 999
paa2[which(years %in% fimsdat$srv_acyrs2), ] <- fimsdat$srvp2
Npaa2 <- rep(1, nyears)
Npaa2[which(years %in% fimsdat$srv_acyrs2)] <- fimsdat$multN_srv2

ind3 <- 0 * pkfit0$rep$Eindxsurv3 - 999
ind3[which(years %in% fimsdat$srvyrs3)] <- fimsdat$indxsurv3
CV3 <- rep(1, length = nyears) # actually SE in log space
CV3[which(years %in% fimsdat$srvyrs3)] <- fimsdat$indxsurv_log_sd3
paa3 <- pkfit0$rep$Esrvp3 * 0 - 999
paa3[which(years %in% fimsdat$srv_acyrs3), ] <- fimsdat$srvp3
Npaa3 <- rep(1, nyears)
Npaa3[which(years %in% fimsdat$srv_acyrs3)] <- fimsdat$multN_srv3

ind6 <- 0 * pkfit0$rep$Eindxsurv6 - 999
ind6[which(years %in% fimsdat$srvyrs6)] <- fimsdat$indxsurv6
CV6 <- rep(1, length = nyears) # actually SE in log space
CV6[which(years %in% fimsdat$srvyrs6)] <- fimsdat$indxsurv_log_sd6
paa6 <- pkfit0$rep$Esrvp6 * 0 - 999
paa6[which(years %in% fimsdat$srv_acyrs6), ] <- fimsdat$srvp6
Npaa6 <- rep(1, nyears)
Npaa6[which(years %in% fimsdat$srv_acyrs6)] <- fimsdat$multN_srv6

## repeat with fish catch at age, using expected in missing years
caa <- pkfit0$rep$Ecatp * 0 - 999
caa[which(years %in% fimsdat$fshyrs), ] <- fimsdat$catp
Ncaa <- rep(1, nyears)
Ncaa[which(years %in% fimsdat$fshyrs)] <- fimsdat$multN_fsh



## put into fims friendly form
res <- data.frame(
  type = character(),
  name = character(),
  age = integer(),
  datestart = character(),
  dateend = character(),
  value = double(),
  unit = character(),
  uncertainty = double()
)
landings <- data.frame(
  type = "landings",
  name = "fleet1",
  age = NA,
  datestart = paste0(seq(fimsdat$styr, fimsdat$endyr), "-01-01"),
  dateend = paste0(seq(fimsdat$styr, fimsdat$endyr), "-12-31"),
  value = as.numeric(fimsdat$cattot) * 1e3,
  unit = "mt",
  uncertainty = fimsdat$cattot_log_sd[1]
)
index2 <- data.frame(
  type = "index",
  name = "survey2",
  age = NA,
  datestart = paste0(seq(fimsdat$styr, fimsdat$endyr), "-01-01"),
  dateend = paste0(seq(fimsdat$styr, fimsdat$endyr), "-12-31"),
  value = ifelse(ind2 > 0, ind2 * 1e9, ind2),
  unit = "",
  uncertainty = CV2
)
index3 <- data.frame(
  type = "index",
  name = "survey3",
  age = NA,
  datestart = paste0(seq(fimsdat$styr, fimsdat$endyr), "-01-01"),
  dateend = paste0(seq(fimsdat$styr, fimsdat$endyr), "-12-31"),
  value = ifelse(ind3 > 0, ind3 * 1e9, ind3),
  unit = "",
  uncertainty = CV3
)
index6 <- data.frame(
  type = "index",
  name = "survey6",
  age = NA,
  datestart = paste0(seq(fimsdat$styr, fimsdat$endyr), "-01-01"),
  dateend = paste0(seq(fimsdat$styr, fimsdat$endyr), "-12-31"),
  value = ifelse(ind6 > 0, ind6 * 1e9, ind6),
  unit = "",
  uncertainty = CV6
)
## these have -999 for missing data years
catchage <- data.frame(
  type = "age",
  name = "fleet1",
  age = rep(seq(1, nages), nyears),
  datestart = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-01-01"
  ), each = nages),
  dateend = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-12-31"
  ), each = nages),
  value = as.numeric(t(caa)),
  unit = "",
  uncertainty = rep(Ncaa, each = nages)
)
indexage2 <- data.frame(
  type = "age",
  name = "survey2",
  age = rep(seq(1, nages), nyears),
  datestart = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-01-01"
  ), each = nages),
  dateend = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-12-31"
  ), each = nages),
  value = as.numeric(t(paa2)),
  unit = "",
  uncertainty = rep(Npaa2, each = nages)
)
indexage3 <- data.frame(
  type = "age",
  name = "survey3",
  age = rep(seq(1, nages), nyears),
  datestart = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-01-01"
  ), each = nages),
  dateend = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-12-31"
  ), each = nages),
  value = as.numeric(t(paa3)),
  unit = "",
  uncertainty = rep(Npaa3, each = nages)
)
indexage6 <- data.frame(
  type = "age",
  name = "survey6",
  age = rep(seq(1, nages), nyears),
  datestart = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-01-01"
  ), each = nages),
  dateend = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-12-31"
  ), each = nages),
  value = as.numeric(t(paa6)),
  unit = "",
  uncertainty = rep(Npaa6, each = nages)
)
indexage <- rbind(indexage2, indexage3, indexage6)
index <- rbind(index2, index3, index6)
## indexage=indexage2
## index=index2
res <- rbind(res, landings, index, catchage, indexage)
## rm(landings, index, catchage, indexage)

age_frame <- FIMS::FIMSFrame(res)

set_fleet(
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
obj <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
## report values for the two models
rep0 <- pkfitfinal$rep
rep1 <- obj$report() # FIMS initial values
## try fitting the model
opt <- TMBhelper::fit_tmb(obj, getsd=FALSE, newtonsteps=0, control=list(trace=100))
## opt <- with(obj, nlminb(start=par, objective=fn, gradient=gr))
max(abs(obj$gr())) # from Cole, can use TMBhelper::fit_tmb to get val to <1e-10
rep2 <- obj$report(obj$env$last.par.best) ## FIMS after estimation
