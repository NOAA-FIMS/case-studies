## define the dimensions and global variables
library(FIMS)

source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_fleet.R"))

years <- 1970:2023
nyears <- length(years)
nseasons <- 1
nages <- 10
ages <- 1:nages
source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))


# test R functions: approach 1 --------------------------------------------
clear()
clear_logs()

is_estimated_obs_error <- list(FALSE, FALSE, FALSE, FALSE)
selectivity_ctl <- list(
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
)

Fmort_ctl <- list(
  is_survey = list(FALSE, TRUE, TRUE, TRUE),
  estimate_F = list(TRUE, FALSE, FALSE, FALSE),
  random_F = list(FALSE, FALSE, FALSE, FALSE)
)

catchability_ctl <- list(
  log_q = list(
    0, parfinal$log_q2_mean,
    parfinal$log_q3_mean, parfinal$inf1_srv6
  ),
  estimate_q = list(FALSE, TRUE, TRUE, TRUE),
  random_q = list(FALSE, FALSE, FALSE, FALSE)
)

fleet <- set_fleet(
  data = age_frame,
  is_estimated_obs_error = is_estimated_obs_error,
  selectivity_ctl = selectivity_ctl,
  selectivity_module_list = NULL,
  Fmort_ctl = Fmort_ctl,
  catchability_ctl = catchability_ctl
)

# Population module
# population
fishing_fleet_names <- dplyr::filter(
  .data = as.data.frame(age_frame@data),
  type == "landings"
) |>
  dplyr::distinct(name) |>
  dplyr::pull(name)
survey_fleet_names <- dplyr::filter(
  .data = as.data.frame(age_frame@data),
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

population_data <- list(
  nages = age_frame@n_ages,
  ages = age_frame@ages,
  nfleets = nfleets,
  nseasons = 1,
  nyears = age_frame@n_years
)

# recruitment
recruitment_ctl <- list(
  form = "BevertonHoltRecruitment",
  initial_values = list(
    log_rzero = parfinal$mean_log_recruit + log(1e9),
    logit_steep = -log(1.0 - .99999) + log(.99999 - 0.2),
    log_sigma_recruit = log(parfinal$sigmaR),
    log_devs = parfinal$dev_log_recruit[-1]
  ),
  is_estimated = list(
    log_rzero = TRUE,
    logit_steep = FALSE,
    log_sigma_recruit = FALSE,
    log_devs = TRUE
  ),
  is_random_effect = list(
    log_rzero = FALSE,
    logit_steep = FALSE,
    log_sigma_recruit = FALSE
  )
)

## growth  -- assumes single WAA vector for everything, based on
## Srv1 above

growth_ctl <- list(
  data = population_data,
  form = "EWAAgrowth",
  initial_values = list(weights = pkinput$dat$wt_srv1)
)

## NOTE: FIMS assumes SSB calculated at the start of the year, so
## need to adjust ASAP to do so as well for now, need to make
## timing of SSB calculation part of FIMS later
## maturity
## NOTE: for now tricking FIMS into thinking age 0 is age 1, so need to adjust A50 for maturity because FIMS calculations use ages 0-5+ instead of 1-6
maturity_ctl <- list(
  form = "LogisticMaturity",
  initial_values = list(
    inflection_point = 4.5,
    slope = 1.5
  ),
  is_estimated = list(
    inflection_point = FALSE,
    slope = FALSE
  ),
  is_random_effect = list(
    inflection_point = FALSE,
    slope = FALSE
  )
)

population_initial_values <- list(
  log_M = log(as.numeric(t(matrix(
    rep(pkfitfinal$rep$M, each = nyears),
    nrow = nyears
  )))),
  log_init_naa =
    c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)
)

population_is_estimated <- list(
  log_M = FALSE,
  log_init_naa = FALSE
)
population <- set_population(
  data = population_data,
  initial_values = population_initial_values,
  is_estimated = population_is_estimated,
  maturity = maturity_ctl,
  growth = growth_ctl,
  recruitment = recruitment_ctl
)

## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj3 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
## report values for the two models
rep3 <- obj3$report()

## Parameters are in a different order but seem to match
opt3 <- with(obj3, nlminb(par, fn, gr))
clear()
clear_logs()


# test R functions: approach 2 --------------------------------------------
# selectivity_double_logistic <- set_selectivity(
#   form = "DoubleLogisticSelectivity",
#   initial_values = c(
#     parfinal$inf1_fsh_mean, exp(parfinal$log_slp1_fsh_mean),
#     parfinal$inf2_fsh_mean, exp(parfinal$log_slp2_fsh_mean)
#   ),
#   is_estimated = rep(TRUE, 4),
#   is_random_effect = rep(FALSE, 4)
# )
# 
# selectivity_logistic <- set_selectivity(
#   form = "LogisticSelectivity",
#   initial_values = c(
#     parfinal$inf1_fsh_mean, exp(parfinal$log_slp1_fsh_mean)
#   ),
#   is_estimated = rep(TRUE, 2),
#   is_random_effect = rep(FALSE, 2)
# )
# 
# selectivity <- vector(mode = "list", length = 4)
# selectivity[[1]] <- selectivity[[2]] <- selectivity_double_logistic
# selectivity[[3]] <- selectivity_logistic
# selectivity[[4]] <- selectivity_double_logistic # Order matters because the selectivity modules assign unique IDs.
# 
# fleet <- set_fleet(
#   data = age_frame,
#   is_estimated_obs_error = list(FALSE, FALSE, FALSE, FALSE),
#   selectivity_ctl = NULL,
#   selectivity_module_list = selectivity,
#   Fmort_ctl = list(
#     is_survey = list(FALSE, TRUE, TRUE, TRUE),
#     estimate_F = list(TRUE, FALSE, FALSE, FALSE),
#     random_F = list(FALSE, FALSE, FALSE, FALSE)
#   ),
#   catchability_ctl = list(
#     log_q = list(
#       0, parfinal$log_q2_mean,
#       parfinal$log_q3_mean, parfinal$inf1_srv6
#     ),
#     estimate_q = list(FALSE, TRUE, TRUE, TRUE),
#     random_q = list(FALSE, FALSE, FALSE, FALSE)
#   )
# )
# 
# # Population module
# # recruitment
# recruitment <- set_recruitment(
#   form = "BevertonHoltRecruitment",
#   initial_values = list(
#     log_rzero = parfinal$mean_log_recruit + log(1e9),
#     logit_steep = -log(1.0 - .99999) + log(.99999 - 0.2),
#     log_sigma_recruit = log(parfinal$sigmaR),
#     log_devs = parfinal$dev_log_recruit[-1]
#   ),
#   is_estimated = list(
#     log_rzero = TRUE,
#     logit_steep = FALSE,
#     log_sigma_recruit = FALSE,
#     log_devs = TRUE
#   ),
#   is_random_effect = list(
#     log_rzero = FALSE,
#     logit_steep = FALSE,
#     log_sigma_recruit = FALSE
#   )
# )
# 
# ## growth  -- assumes single WAA vector for everything, based on
# ## Srv1 above
# growth <- set_growth(
#   data = population_data,
#   form = "EWAAgrowth",
#   initial_values = list(weights = pkinput$dat$wt_srv1)
# )
# ## NOTE: FIMS assumes SSB calculated at the start of the year, so
# ## need to adjust ASAP to do so as well for now, need to make
# ## timing of SSB calculation part of FIMS later
# ## maturity
# ## NOTE: for now tricking FIMS into thinking age 0 is age 1, so need to adjust A50 for maturity because FIMS calculations use ages 0-5+ instead of 1-6
# maturity <- set_maturity(
#   form = maturity_ctl$form,
#   initial_values = maturity_ctl$initial_values,
#   is_estimated = maturity_ctl$is_estimated,
#   is_random_effect = maturity_ctl$is_random_effect
# )
# 
# # population
# population <- set_population(
#   data = population_data,
#   initial_values = population_initial_values,
#   is_estimated = population_is_estimated,
#   maturity = maturity,
#   growth = growth,
#   recruitment = recruitment
# )
# 
# ## make FIMS model
# success <- CreateTMBModel()
# parameters <- list(p = get_fixed())
# obj4 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
# ## report values for the two models
# rep4 <- obj4$report()
# 
# ## Parameters are in a different order but seem to match
# opt4 <- with(obj4, nlminb(par, fn, gr))
# clear()
# clear_logs()
# # rep4
# # rm(list = ls())
# 
# ## Test that the two models are identical
# all.equal(rep3, rep4)
