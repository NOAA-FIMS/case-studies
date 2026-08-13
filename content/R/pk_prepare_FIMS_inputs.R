## Quick function to compare models
get_long_outputs <- function(fims, tmb, output) {
  ## estimated catch is pretty close I think
  naa <- matrix(fims$numbers_at_age[[1]], ncol = n_ages, byrow = TRUE) / 1e9
  catch <-
    data.frame(
      year = years,
      name = "catch",
      FIMS = dplyr::filter(
        output,
        label == "catch_expected",
        estimated != 0
      ) |>
      dplyr::pull(estimated) / 1e3,
      TMB = tmb$Ecattot
    )
  ## spawnbio
  ssb <- data.frame(
    year = years,
    name = "SSB",
    FIMS = fims$spawning_biomass[[1]][-55] / 1e9,
    TMB = tmb$Espawnbio
  )
  bio <-
    data.frame(
      year = years,
      name = "biomass",
      FIMS = fims$biomass[[1]][-55] / 1e9,
      TMB = tmb$Etotalbio
    )
  fmort <-
    data.frame(
      year = years,
      name = "F",
      FIMS = dplyr::filter(
        output,
        label == "log_Fmort",
        estimation_type == "fixed_effects"
      ) |>
        dplyr::pull(estimated) |>
        exp(),
      TMB = tmb$F
    )
  recruit <-
    data.frame(
      year = years,
      name = "recruit",
      FIMS = naa[-55, 1],
      TMB = tmb$recruit
    )
  ## expected index survey 2
  eind2 <-
    data.frame(
      year = years,
      name = "Index2",
      FIMS = fims$index_expected[[3]] / 1e9,
      TMB = tmb$Eindxsurv2
    )
  eind3 <-
    data.frame(
      year = years,
      name = "Index3",
      FIMS = fims$index_expected[[1]] / 1e9,
      TMB = tmb$Eindxsurv3
    )
  eind6 <-
    data.frame(
      year = years,
      name = "Index6",
      FIMS = fims$index_expected[[4]] / 1e9,
      TMB = tmb$Eindxsurv6
    )
  xx <- rbind(catch, ssb, bio, fmort, eind2, eind3, eind6, recruit) |>
    tidyr::pivot_longer(cols = -c(name, year), names_to = "platform") |>
    dplyr::group_by(year, name) |>
    dplyr::mutate(
      relerror = (value - value[platform == "TMB"]) / 
        value[platform == "TMB"]) |>
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

  # TODO: fix the labels
  fleet <- dplyr::case_when(
    fleet == 1 ~ 2,
    fleet == 2 ~ 3,
    fleet == 3 ~ 1,
    FALSE ~ fleet
  )
  lab <- c("Fishery", "Survey 2", "Survey 3", "Survey 6")[fleet]
  x1 <- matrix(fims1$catch_numbers_at_age[[fleet]], ncol = 10, byrow = TRUE)[ind, ]
  x1 <- x1 / rowSums(x1)
  x2 <- matrix(fims2$catch_numbers_at_age[[fleet]], ncol = 10, byrow = TRUE)[ind, ]
  x2 <- x2 / rowSums(x2)
  dimnames(y) <-  dimnames(x1) <- dimnames(x2) <-
    list(year = years, age = 1:10)
  dimnames(obs) <- list(year = years, age = 1:10)
  x1 <- reshape2::melt(x1, value.name = "paa") |>
    cbind(platform = "FIMS init", type = lab)
  x2 <- reshape2::melt(x2, value.name = "paa") |>
    cbind(platform = "FIMS opt", type = lab)
  y <- reshape2::melt(y, value.name = "paa") |>
    cbind(platform = "TMB", type = lab)
  obs <- reshape2::melt(obs, value.name = "paa") |>
    cbind(platform = "obs", type = lab)
  out <- rbind(x1, x2, y, obs)
  out
}

## build a FIMS and PK data set that match
##  need to fill missing years with -999 so it's ignored in FIMS
# TODO: FIMS now supports automatically filling in missing values.
# We can test this feature using the case study to evaluate its functionality.
prepare_pollock_data <- function(
    pkfitfinal,
    pkfit0,
    parfinal,
    fimsdat,
    pkinput,
    years,
    n_years,
    ages,
    n_ages
) {
  ind2 <- 0 * pkfit0$rep$Eindxsurv2 - 999
  ind2[which(years %in% fimsdat$srvyrs2)] <- fimsdat$indxsurv2
  CV2 <- rep(1, length = n_years) # actually SE in log space
  CV2[which(years %in% fimsdat$srvyrs2)] <- fimsdat$indxsurv_log_sd2
  paa2 <- pkfit0$rep$Esrvp2 * 0 - 999
  paa2[which(years %in% fimsdat$srv_acyrs2), ] <- fimsdat$srvp2
  Npaa2 <- rep(1, n_years)
  Npaa2[which(years %in% fimsdat$srv_acyrs2)] <- fimsdat$multN_srv2

  ind3 <- 0 * pkfit0$rep$Eindxsurv3 - 999
  ind3[which(years %in% fimsdat$srvyrs3)] <- fimsdat$indxsurv3
  CV3 <- rep(1, length = n_years) # actually SE in log space
  CV3[which(years %in% fimsdat$srvyrs3)] <- fimsdat$indxsurv_log_sd3
  paa3 <- pkfit0$rep$Esrvp3 * 0 - 999
  paa3[which(years %in% fimsdat$srv_acyrs3), ] <- fimsdat$srvp3
  Npaa3 <- rep(1, n_years)
  Npaa3[which(years %in% fimsdat$srv_acyrs3)] <- fimsdat$multN_srv3

  ind6 <- 0 * pkfit0$rep$Eindxsurv6 - 999
  ind6[which(years %in% fimsdat$srvyrs6)] <- fimsdat$indxsurv6
  CV6 <- rep(1, length = n_years) # actually SE in log space
  CV6[which(years %in% fimsdat$srvyrs6)] <- fimsdat$indxsurv_log_sd6
  paa6 <- pkfit0$rep$Esrvp6 * 0 - 999
  paa6[which(years %in% fimsdat$srv_acyrs6), ] <- fimsdat$srvp6
  Npaa6 <- rep(1, n_years)
  Npaa6[which(years %in% fimsdat$srv_acyrs6)] <- fimsdat$multN_srv6

  ## repeat with fish catch at age, using expected in missing years
  caa <- pkfit0$rep$Ecatp * 0 - 999
  caa[which(years %in% fimsdat$fshyrs), ] <- fimsdat$catp
  Ncaa <- rep(1, n_years)
  Ncaa[which(years %in% fimsdat$fshyrs)] <- fimsdat$multN_fsh

  ## put into fims friendly form
  res <- data.frame(
    type = character(),
    fleet = character(),
    age = integer(),
    timing = double(),
    observed = double(),
    unit = character(),
    uncertainty = character()
  )

  catch <- data.frame(
    type = "catch",
    fleet = "fleet1",
    age = NA,
    timing = seq(fimsdat$styr, fimsdat$endyr),
    observed = as.numeric(fimsdat$cattot) * 1e3,
    unit = "mt",
    uncertainty = paste0(
      "~dlnorm(meanlog = log_catch_expected, sdlog = ",
      fimsdat$cattot_log_sd[1],
      ")"
    )
  )
  index2 <- data.frame(
    type = "index",
    fleet = "survey2",
    age = NA,
    timing = seq(fimsdat$styr, fimsdat$endyr),
    observed = ifelse(ind2 > 0, ind2 * 1e9, ind2),
    unit = "",
    uncertainty = paste0(
      "~dlnorm(meanlog = log_index_expected, sdlog = ",
      CV2,
      ")"
    )
  )
  index3 <- data.frame(
    type = "index",
    fleet = "survey3",
    age = NA,
    timing = seq(fimsdat$styr, fimsdat$endyr),
    observed = ifelse(ind3 > 0, ind3 * 1e9, ind3),
    unit = "",
    uncertainty = paste0(
      "~dlnorm(meanlog = log_index_expected, sdlog = ",
      CV3,
      ")"
    )
  )
  index6 <- data.frame(
    type = "index",
    fleet = "survey6",
    age = NA,
    timing = seq(fimsdat$styr, fimsdat$endyr),
    observed = ifelse(ind6 > 0, ind6 * 1e9, ind6),
    unit = "",
    uncertainty = paste0(
      "~dlnorm(meanlog = log_index_expected, sdlog = ",
      CV6,
      ")"
    )
  )
  ## these have -999 for missing data years
  catchage <- data.frame(
    type = "age_comp",
    fleet = "fleet1",
    age = rep(seq(1, n_ages), n_years),
    timing = rep(
      seq(fimsdat$styr, fimsdat$endyr),
      each = n_ages
    ),
    observed = as.numeric(t(caa)),
    unit = "",
    uncertainty = paste0(
      "~dmultinom(prob = agecomp_proportion, size = ",
      rep(Ncaa, each = n_ages),
      ")"
    )
  )
  indexage2 <- data.frame(
    type = "age_comp",
    fleet = "survey2",
    age = rep(seq(1, n_ages), n_years),
    timing = rep(
      seq(fimsdat$styr, fimsdat$endyr),
      each = n_ages
    ),
    observed = as.numeric(t(paa2)),
    unit = "",
    uncertainty = paste0(
      "~dmultinom(prob = agecomp_proportion, size = ",
      rep(Npaa2, each = n_ages),
      ")"
    )
  )
  indexage3 <- data.frame(
    type = "age_comp",
    fleet = "survey3",
    age = rep(seq(1, n_ages), n_years),
    timing = rep(
      seq(fimsdat$styr, fimsdat$endyr),
      each = n_ages
    ),
    observed = as.numeric(t(paa3)),
    unit = "",
    uncertainty = paste0(
      "~dmultinom(prob = agecomp_proportion, size = ",
      rep(Npaa3, each = n_ages),
      ")"
    )
  )
  indexage6 <- data.frame(
    type = "age_comp",
    fleet = "survey6",
    age = rep(seq(1, n_ages), n_years),
    timing = rep(
      seq(fimsdat$styr, fimsdat$endyr),
      each = n_ages
    ),
    observed = as.numeric(t(paa6)),
    unit = "",
    uncertainty = paste0(
      "~dmultinom(prob = agecomp_proportion, size = ",
      rep(Npaa6, each = n_ages),
      ")"
    )
  )
  indexage <- rbind(indexage2, indexage3, indexage6)
  index <- rbind(index2, index3, index6)
  ## indexage=indexage2
  ## index=index2
  res <- rbind(res, catch, index, catchage, indexage)
  ## rm(catch, index, catchage, indexage)

  timingfishery <- data.frame(
    timing = rep(
      seq(fimsdat$styr, fimsdat$endyr),
      each = n_ages
    )
  )
  weightsfishery <- do.call(rbind, replicate(
    length(years) + 1,
    pkinput$dat$wt_srv1[1, ],
    simplify = FALSE
  ))
  colnames(weightsfishery) <- ages
  rownames(weightsfishery) <- c(years, max(years) + 1)
  weightatage_data <- tidyr::pivot_longer(
    as.data.frame(weightsfishery) |>
      tibble::rownames_to_column(var = "timing"),
    !timing,
    names_to = "age"
  ) |>
    dplyr::mutate(
      type = "weight_at_age",
      unit = "mt",
      timing = as.numeric(timing),
      age = as.numeric(age)
    ) |>
    dplyr::rename(observed = value)

  res <- dplyr::bind_rows(res, weightatage_data)

  data_4_model <- FIMS::FIMSFrame(res)
}
