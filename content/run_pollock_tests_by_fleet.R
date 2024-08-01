## define the dimensions and global variables
# library(FIMS)
# 
# source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs_by_fleet.R"))
# 
# years <- 1970:2023
# nyears <- length(years)
# nseasons <- 1
# nages <- 10
# ages <- 1:nages
# source(file.path(getwd(), "content", "R", "pk_prepare_dat.R"))

# test R functions: approach 1 --------------------------------------------
clear()
clear_logs()



## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj3 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
## report values for the two models
rep3 <- obj3$report()


clear()
clear_logs()
# source("R/pk_prepare_FIMS_inputs.R")
estimate_fish_selex <- TRUE
estimate_survey_selex <- TRUE
estimate_q2 <- TRUE
estimate_q3 <- TRUE
estimate_q6 <- TRUE
estimate_F <- TRUE
estimate_recdevs <- TRUE
source(file.path(getwd(), "content", "R", "pk_prepare_FIMS_inputs.R"))
## make FIMS model
success <- CreateTMBModel()
parameters <- list(p = get_fixed())
obj1 <- TMB::MakeADFun(data = list(), parameters, DLL = "FIMS", silent = TRUE)
rep1 <- obj1$report()

## Parameters are in a different order but seem to match
opt1 <- with(obj1, nlminb(par, fn, gr))
opt3 <- with(obj3, nlminb(par, fn, gr))
clear()
clear_logs()


## Test that the two models are identical
all.equal(rep1,rep3)
all.equal(obj1$report(opt1$par), obj3$report(opt3$par))
sum(obj1$par)-sum(obj3$par)

print(cbind(sort(opt1$par)- sort(opt3$par)))

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
