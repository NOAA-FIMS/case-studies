## Quick function to compare models
get_long_outputs <- function(fims, tmb) {
  ## estimaed catch is pretty close I think
  naa <- matrix(fims$naa[[1]], ncol = nages, byrow = TRUE) / 1e9
  catch <-
    data.frame(
      year = years,
      name = 'catch',
      FIMS = fims$landings_exp[[1]] / 1e3,
      TMB = tmb$Ecattot
    )
  ## spawnbio
  ssb <- data.frame(
    year = years,
    name = 'SSB',
    FIMS = fims$ssb[[1]][-55] / 1e9,
    TMB = tmb$Espawnbio
  )
  bio <-
    data.frame(
      year = years,
      name = 'biomass',
      FIMS = fims$biomass[[1]][-55] / 1e9,
      TMB = tmb$Etotalbio
    )
  fmort <-
    data.frame(
      year = years,
      name = 'F',
      FIMS = fims$F_mort[[1]],
      TMB = tmb$F
    )
  recruit <-
    data.frame(
      year = years,
      name = 'recruit',
      FIMS = naa[-55, 1],
      TMB = tmb$recruit
    )
  ## expected index survey 2
  eind2 <-
    data.frame(
      year = years,
      name = 'Index2',
      FIMS = fims$index_exp[[2]] / 1e9,
      TMB = tmb$Eindxsurv2
    )
  eind3 <-
    data.frame(
      year = years,
      name = 'Index3',
      FIMS = fims$index_exp[[3]] / 1e9,
      TMB = tmb$Eindxsurv3
    )
  eind6 <-
    data.frame(
      year = years,
      name = 'Index6',
      FIMS = fims$index_exp[[4]] / 1e9,
      TMB = tmb$Eindxsurv6
    )
  xx <- rbind(catch, ssb, bio, fmort, eind2, eind3, eind6, recruit) |>
    tidyr::pivot_longer(cols = -c(name, year), names_to = 'platform') |>
    dplyr::group_by(year, name) |>
    dplyr::mutate(
      relerror = (value - value[platform == 'TMB']) / 
        value[platform == 'TMB']) |>
    dplyr::ungroup()
  return(xx)
}

get_acomp_fits <- function(tmb, fims1, fims2, fleet, years) {
  ind <- which(tmb$years %in% years)
  if (fleet == 1) {
    y <- tmb$res_fish[, 11:20]
    obs <- tmb$res_fish[, 1:10]
  } else if (fleet == 2) {
    y <- tmb$res_srv2[, 11:20]
    obs <- tmb$res_srv2[, 1:10]
  } else if (fleet == 3) {
    y <- tmb$res_srv3[, 11:20]
    obs <- tmb$res_srv3[, 1:10]
  } else if (fleet == 4) {
    y <- tmb$res_srv6[, 11:20]
    obs <- tmb$res_srv6[, 1:10]
  } else {
    stop("bad fleet")
  }

  lab <- c('Fishery', 'Survey 2', 'Survey 3', 'Survey 6')[fleet]
  x1 <- matrix(fims1$landings_naa[[fleet]], ncol = 10, byrow = TRUE)[ind, ]
  x1 <- x1 / rowSums(x1)
  x2 <- matrix(fims2$landings_naa[[fleet]], ncol = 10, byrow = TRUE)[ind, ]
  x2 <- x2 / rowSums(x2)
  dimnames(y) <-  dimnames(x1) <- dimnames(x2) <-
    list(year = years, age = 1:10)
  dimnames(obs) <- list(year = years, age = 1:10)
  x1 <- reshape2::melt(x1, value.name = 'paa') |>
    cbind(platform = 'FIMS init', type = lab)
  x2 <- reshape2::melt(x2, value.name = 'paa') |>
    cbind(platform = 'FIMS opt', type = lab)
  y <- reshape2::melt(y, value.name = 'paa') |>
    cbind(platform = 'TMB', type = lab)
  obs <- reshape2::melt(obs, value.name = 'paa') |>
    cbind(platform = 'obs', type = lab)
  out <- rbind(x1, x2, y, obs)
  out
}

## build a FIMS and PK data set that match
##  need to fill missing years with -999 so it's ignored in FIMS
# TODO: FIMS now supports automatically filling in missing values. 
# We can test this feature using the case study to evaluate its functionality. 
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

timingfishery <- data.frame(
  datestart = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-01-01"
  ), each = nages),
  dateend = rep(paste0(
    seq(fimsdat$styr, fimsdat$endyr), "-12-31"
  ), each = nages)
)
weightsfishery <- data.frame(
  type = "weight-at-age",
  name = "fleet1",
  age = seq(1, nages),
  value = pkinput$dat$wt_srv1[1,],
  uncertainty = NA,
  unit = "mt"
)
weightatage_data <- merge(timingfishery, weightsfishery)

res <- rbind(res, weightatage_data)

data_4_model <- FIMS::FIMSFrame(res)

fishery_catch <- FIMS::m_landings(data_4_model, 'fleet1')
fishery_agecomp <- FIMS::m_agecomp(data_4_model, "fleet1")
survey_index2 <- FIMS::m_index(data_4_model, "survey2")
survey_agecomp2 <- FIMS::m_agecomp(data_4_model, "survey2")
survey_index3 <- FIMS::m_index(data_4_model, "survey3")
survey_agecomp3 <- FIMS::m_agecomp(data_4_model, "survey3")
survey_index6 <- FIMS::m_index(data_4_model, "survey6")
survey_agecomp6 <- FIMS::m_agecomp(data_4_model, "survey6")
# need to think about how to deal with multiple fleets - only using 1 fleet for now
# TODO: FIMS now supports multiple fishing fleets. 
# We can test this feature using the case study to evaluate its functionality. 
fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
purrr::walk(
  seq_along(fishery_catch),
  \(x) fish_index$index_data$set(x - 1, fishery_catch[x])
)
purrr::walk(
  seq_along(fishery_agecomp),
  \(x) fish_age_comp$age_comp_data$set(
    x - 1,
    (fishery_agecomp * catchage$uncertainty)[x]
  )
)

### set up fishery
## fleet selectivity: converted from time-varying ascending
## slope/intercept to constant double-logistic
## methods::show(DoubleLogisticSelectivity)
fish_selex <- methods::new(DoubleLogisticSelectivity)
fish_selex$inflection_point_asc[1]$value <- parfinal$inf1_fsh_mean
fish_selex$inflection_point_asc[1]$estimation_type <- estimate_fish_selex
fish_selex$inflection_point_desc[1]$value <- parfinal$inf2_fsh_mean
fish_selex$inflection_point_desc[1]$estimation_type <- estimate_fish_selex
fish_selex$slope_asc[1]$value <- exp(parfinal$log_slp1_fsh_mean)
fish_selex$slope_asc[1]$estimation_type <- estimate_fish_selex
fish_selex$slope_desc[1]$value <- exp(parfinal$log_slp2_fsh_mean)
fish_selex$slope_desc[1]$estimation_type <- estimate_fish_selex

## create fleet object
fish_fleet <- methods::new(Fleet)
fish_fleet$nages$set(nages)
fish_fleet$nyears$set(nyears)

fish_fleet$log_Fmort$resize(nyears)
for (y in 1:nyears) {
  # Log-transform OM fishing mortality
  fish_fleet$log_Fmort[y]$value <- log(pkfitfinal$rep$F[y])
}
fish_fleet$log_Fmort$set_all_estimable(TRUE)
fish_fleet$log_q[1]$value <- 0 # why is this length two in Chris' case study?
fish_fleet$log_q[1]$estimation_type <- "constant"

# Set Index, AgeComp, and Selectivity using the IDs from the modules defined above
fish_fleet$SetObservedIndexData(fish_index$get_id())
fish_fleet$SetObservedAgeCompData(fish_age_comp$get_id())
fish_fleet$SetSelectivity(fish_selex$get_id())

# Set up fishery index data using the lognormal
fish_fleet_index_distribution <- methods::new(DlnormDistribution)
# lognormal observation error transformed on the log scale
fish_fleet_index_distribution$log_sd$resize(nyears)
for (y in 1:nyears) {
  # Compute lognormal SD from OM coefficient of variation (CV)
  fish_fleet_index_distribution$log_sd[y]$value <- log(landings$uncertainty[y])
}
fish_fleet_index_distribution$log_sd$set_all_estimable(FALSE)
# Set Data using the IDs from the modules defined above
fish_fleet_index_distribution$set_observed_data(fish_fleet$GetObservedIndexDataID())
fish_fleet_index_distribution$set_distribution_links("data", fish_fleet$log_expected_index$get_id())

# Set up fishery age composition data using the multinomial
fish_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
fish_fleet_agecomp_distribution$set_observed_data(fish_fleet$GetObservedAgeCompDataID())
fish_fleet_agecomp_distribution$set_distribution_links("data", fish_fleet$proportion_catch_numbers_at_age$get_id())

## Setup survey 2
survey2_fleet_index <- methods::new(Index, nyears)
survey2_age_comp <- methods::new(AgeComp, nyears, nages)
purrr::walk(
  seq_along(survey_index2),
  \(x) survey2_fleet_index$index_data$set(x - 1, survey_index2[x])
)
purrr::walk(
  seq_along(survey_agecomp2),
  \(x) survey2_age_comp$age_comp_data$set(
    x - 1,
    (survey_agecomp2 * indexage2$uncertainty)[x]
  )
)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey2_selex <- methods::new(DoubleLogisticSelectivity)
survey2_selex$inflection_point_asc[1]$value <- parfinal$inf1_srv2
survey2_selex$inflection_point_asc[1]$estimation_type <- estimate_survey_selex
survey2_selex$slope_asc[1]$value <- exp(parfinal$log_slp1_srv2)
survey2_selex$slope_asc[1]$estimation_type <- estimate_survey_selex
## not estimated to make it ascending only, fix at input values
survey2_selex$inflection_point_desc[1]$value <- parfinal$inf2_srv2
survey2_selex$inflection_point_desc[1]$estimation_type <- "constant"
survey2_selex$slope_desc[1]$value <- exp(parfinal$log_slp2_srv2)
survey2_selex$slope_desc[1]$estimation_type <- "constant"

survey2_fleet <- methods::new(Fleet)
survey2_fleet$nages$set(nages)
survey2_fleet$nyears$set(nyears)
survey2_fleet$log_q[1]$value <- parfinal$log_q2_mean
survey2_fleet$log_q[1]$estimation_type <- "fixed_effects"
survey2_fleet$SetSelectivity(survey2_selex$get_id())
survey2_fleet$SetObservedIndexData(survey2_fleet_index$get_id())
survey2_fleet$SetObservedAgeCompData(survey2_age_comp$get_id())

survey2_fleet_index_distribution <- methods::new(DlnormDistribution)
# lognormal observation error transformed on the log scale
survey2_fleet_index_distribution$log_sd$resize(nyears)
for (y in 1:nyears) {
 # Compute lognormal SD from OM coefficient of variation (CV)
 survey2_fleet_index_distribution$log_sd[y]$value <- log(index2$uncertainty)[y]
}
survey2_fleet_index_distribution$log_sd$set_all_estimable(FALSE)
# Set Data using the IDs from the modules defined above
survey2_fleet_index_distribution$set_observed_data(survey2_fleet$GetObservedIndexDataID())
survey2_fleet_index_distribution$set_distribution_links("data", survey2_fleet$log_expected_index$get_id())
# Set up fishery age composition data using the multinomial
survey2_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
survey2_fleet_agecomp_distribution$set_observed_data(survey2_fleet$GetObservedAgeCompDataID())
survey2_fleet_agecomp_distribution$set_distribution_links("data", survey2_fleet$proportion_catch_numbers_at_age$get_id())
 
## Setup survey 3
survey3_fleet_index <- methods::new(Index, nyears)
survey3_age_comp <- methods::new(AgeComp, nyears, nages)
purrr::walk(
  seq_along(survey_index3),
  \(x) survey3_fleet_index$index_data$set(x - 1, survey_index3[x])
)
purrr::walk(
  seq_along(survey_agecomp3),
  \(x) survey3_age_comp$age_comp_data$set(
    x - 1,
    (survey_agecomp3 * indexage3$uncertainty)[x]
  )
)
## survey selectivity: ascending logistic
## methods::show(LogisticSelectivity)
survey3_selex <- methods::new(LogisticSelectivity)
survey3_selex$inflection_point[1]$value <- parfinal$inf1_srv3
survey3_selex$inflection_point[1]$estimation_type <- estimate_survey_selex
survey3_selex$slope[1]$value <- exp(parfinal$log_slp1_srv3)
survey3_selex$slope[1]$estimation_type <- estimate_survey_selex

survey3_fleet <- methods::new(Fleet)
survey3_fleet$nages$set(nages)
survey3_fleet$nyears$set(nyears)
survey3_fleet$log_q[1]$value <- parfinal$log_q3_mean
survey3_fleet$log_q[1]$estimation_type <- "fixed_effects"
survey3_fleet$SetSelectivity(survey3_selex$get_id())
survey3_fleet$SetObservedIndexData(survey3_fleet_index$get_id())
survey3_fleet$SetObservedAgeCompData(survey3_age_comp$get_id())

# sd = sqrt(log(cv^2 + 1)), sd is log transformed
survey3_fleet_index_distribution <- methods::new(DlnormDistribution)
# lognormal observation error transformed on the log scale
survey3_fleet_index_distribution$log_sd$resize(nyears)
for (y in 1:nyears) {
  # Compute lognormal SD from OM coefficient of variation (CV)
  survey3_fleet_index_distribution$log_sd[y]$value <- log(index3$uncertainty)[y]
}
survey3_fleet_index_distribution$log_sd$set_all_estimable(FALSE)
# Set Data using the IDs from the modules defined above
survey3_fleet_index_distribution$set_observed_data(survey3_fleet$GetObservedIndexDataID())
survey3_fleet_index_distribution$set_distribution_links("data", survey3_fleet$log_expected_index$get_id())
# Set up fishery age composition data using the multinomial
survey3_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
survey3_fleet_agecomp_distribution$set_observed_data(survey3_fleet$GetObservedAgeCompDataID())
survey3_fleet_agecomp_distribution$set_distribution_links("data", survey3_fleet$proportion_catch_numbers_at_age$get_id())

## Setup survey 6
survey6_fleet_index <- methods::new(Index, nyears)
survey6_age_comp <- methods::new(AgeComp, nyears, nages)
purrr::walk(
  seq_along(survey_index6),
  \(x) survey6_fleet_index$index_data$set(x - 1, survey_index6[x])
)
purrr::walk(
  seq_along(survey_agecomp6),
  \(x) survey6_age_comp$age_comp_data$set(
    x - 1,
    (survey_agecomp6 * indexage6$uncertainty)[x]
  )
)

## survey selectivity: ascending logistic
## methods::show(DoubleLogisticSelectivity)
survey6_selex <- methods::new(DoubleLogisticSelectivity)
survey6_selex$inflection_point_asc[1]$value <- parfinal$inf1_srv6
survey6_selex$inflection_point_asc[1]$estimation_type <- "constant"
survey6_selex$slope_asc[1]$value <- exp(parfinal$log_slp1_srv6)
survey6_selex$slope_asc[1]$estimation_type <- "constant"
## not estimated to make it ascending only, fix at input values
survey6_selex$inflection_point_desc[1]$value <- parfinal$inf2_srv6
survey6_selex$inflection_point_desc[1]$estimation_type <-
  estimate_survey_selex
survey6_selex$slope_desc[1]$value <- exp(parfinal$log_slp2_srv6)
survey6_selex$slope_desc[1]$estimation_type <- estimate_survey_selex

survey6_fleet <- methods::new(Fleet)
survey6_fleet$nages$set(nages)
survey6_fleet$nyears$set(nyears)
survey6_fleet$log_q[1]$value <- parfinal$log_q6
survey6_fleet$log_q[1]$estimation_type <- "fixed_effects"
survey6_fleet$SetSelectivity(survey6_selex$get_id())
survey6_fleet$SetObservedIndexData(survey6_fleet_index$get_id())
survey6_fleet$SetObservedAgeCompData(survey6_age_comp$get_id())

survey6_fleet_index_distribution <- methods::new(DlnormDistribution)
# lognormal observation error transformed on the log scale
survey6_fleet_index_distribution$log_sd$resize(nyears)
for (y in 1:nyears) {
  # Compute lognormal SD from OM coefficient of variation (CV)
  survey6_fleet_index_distribution$log_sd[y]$value <- log(index6$uncertainty)[y]
}
survey6_fleet_index_distribution$log_sd$set_all_estimable(FALSE)
# Set Data using the IDs from the modules defined above
survey6_fleet_index_distribution$set_observed_data(survey6_fleet$GetObservedIndexDataID())
survey6_fleet_index_distribution$set_distribution_links("data", survey6_fleet$log_expected_index$get_id())
# Set up fishery age composition data using the multinomial
survey6_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
survey6_fleet_agecomp_distribution$set_observed_data(survey6_fleet$GetObservedAgeCompDataID())
survey6_fleet_agecomp_distribution$set_distribution_links("data", survey6_fleet$proportion_catch_numbers_at_age$get_id())

# Population module
# recruitment
recruitment <- methods::new(BevertonHoltRecruitment)
recruitment_process <- new(LogDevsRecruitmentProcess)
recruitment$SetRecruitmentProcess(recruitment_process$get_id())
## methods::show(BevertonHoltRecruitment)
#recruitment$log_sigma_recruit[1]$value <- log(parfinal$sigmaR)
recruitment$log_rzero[1]$value <- parfinal$mean_log_recruit + log(1e9)
recruitment$log_rzero[1]$estimation_type <- "fixed_effects"
## note: do not set steepness exactly equal to 1, use 0.99 instead in ASAP run
recruitment$logit_steep[1]$value <-
  -log(1.0 - .99999) + log(.99999 - 0.2)
recruitment$logit_steep[1]$estimation_type <- "constant"
recruitment$log_devs$resize(nyears-1)
for (y in 1:(nyears-1)) {
  recruitment$log_devs[y]$value <- parfinal$dev_log_recruit[y+1]
}
recruitment$log_devs$set_all_estimable(estimate_recdevs)
recruitment$log_devs$set_all_random(TRUE)
recruitment_distribution <- methods::new(DnormDistribution)
# set up logR_sd using the normal log_sd parameter
# logR_sd is NOT logged. It needs to enter the model logged b/c the exp() is
# taken before the likelihood calculation
recruitment_distribution$log_sd <- methods::new(ParameterVector, 1)
recruitment_distribution$log_sd[1]$value <- log(parfinal$sigmaR)
recruitment_distribution$log_sd[1]$estimation_type <- "constant"
recruitment_distribution$x$resize(nyears-1)
recruitment_distribution$expected_values$resize(nyears-1)
for (i in 1:(nyears-1)) {
  recruitment_distribution$x[i]$value <- 0
  recruitment_distribution$expected_values[i]$value <- 0
}
recruitment_distribution$set_distribution_links("random_effects", recruitment$log_devs$get_id())

## growth  -- assumes single WAA vector for everything, based on
## Srv1 above
waa <- pkinput$dat$wt_srv1
ewaa_growth <- methods::new(EWAAgrowth)
ewaa_growth$ages$resize(nages)
purrr::walk(
  seq_along(ages),
  \(x) ewaa_growth$ages$set(x - 1, ages[x])
)
# NOTE: FIMS currently cannot use matrix of WAA, so have to ensure constant WAA over time in ASAP file for now
ewaa_growth$weights$resize(nages)
purrr::walk(
  seq_along(waa[1, ]),
  \(x) ewaa_growth$weights$set(x - 1, waa[1, x])
)
## NOTE: FIMS assumes SSB calculated at the start of the year, so
## need to adjust ASAP to do so as well for now, need to make
## timing of SSB calculation part of FIMS later
## maturity
## NOTE: for now tricking FIMS into thinking age 0 is age 1, so need to adjust A50 for maturity because FIMS calculations use ages 0-5+ instead of 1-6

maturity <- new(LogisticMaturity)
maturity$inflection_point[1]$value <- 4.5
maturity$inflection_point[1]$estimation_type <- "constant"
maturity$slope[1]$value <- 1.5
maturity$slope[1]$estimation_type <- "constant"

# population
population <- new(Population)
tmpM <- log(as.numeric(t(matrix(
  rep(pkfitfinal$rep$M, each = nyears), nrow = nyears
))))
population$log_M$resize(nyears * nages)
for (i in 1:(nyears * nages)) {
  population$log_M[i]$value <- tmpM[i]
}
population$log_M$set_all_estimable(FALSE)
population$log_init_naa$resize(nages)
initNAA <- c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)
for (i in seq_along(nages)) {
  population$log_init_naa[i]$value <- initNAA[i]
}
population$log_init_naa$set_all_estimable(FALSE)# NOTE: fixing at ASAP estimates to test SSB calculations
population$nages$set(nages)
population$ages$resize(nages)
purrr::walk(
  seq_along(ages),
  \(x) population$ages$set(x - 1, ages[x])
)
population$nfleets$set(4)
population$nseasons$set(nseasons)
population$nyears$set(nyears)
population$SetMaturity(maturity$get_id())
population$SetGrowth(ewaa_growth$get_id())
population$SetRecruitment(recruitment$get_id())
