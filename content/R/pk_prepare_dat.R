

## build a FIMS and PK data set that match

pkfitfinal <- readRDS("data_files/pkfitfinal.RDS")
pkfit0 <- readRDS("data_files/pkfit0.RDS")
parfinal <- pkfitfinal$obj$env$parList()
pkinput0 <- readRDS('data_files/pkinput0.RDS')
fimsdat <- pkdat0 <- pkinput0$dat
pkinput <- readRDS('data_files/pkinput.RDS')



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
fishery_catch <- FIMS::m_landings(age_frame)
fishery_agecomp <- FIMS::m_agecomp(age_frame, "fleet1")
survey_index2 <- FIMS::m_index(age_frame, "survey2")
survey_agecomp2 <- FIMS::m_agecomp(age_frame, "survey2")
survey_index3 <- FIMS::m_index(age_frame, "survey3")
survey_agecomp3 <- FIMS::m_agecomp(age_frame, "survey3")
survey_index6 <- FIMS::m_index(age_frame, "survey6")
survey_agecomp6 <- FIMS::m_agecomp(age_frame, "survey6")
# need to think about how to deal with multiple fleets - only using 1 fleeet for now
fish_index <- methods::new(Index, nyears)
fish_age_comp <- methods::new(AgeComp, nyears, nages)
fish_index$index_data <- fishery_catch
fish_age_comp$age_comp_data <-
  fishery_agecomp * catchage$uncertainty#rep(Ncaa, each=nages)
