source(file.path(getwd(), "content", "run_pollock_tests.R"))
# Function to print arguments
print_arguments <- function(...) {
  cat("Following arguments were used:\n\n")
  print(list(...))
}

# Option 1: Organize function parameters by individual recruitment model parameters --------
setup_BevertonHolt_recruitment_opt1 <- function(log_rzero,
                                                logit_steep,
                                                log_sigma_recruit,
                                                log_devs) {
  recruitment_m <- methods::new(BevertonHoltRecruitment)

  # Set up log_rzero parameters
  recruitment_m$log_rzero$value <- log_rzero$value
  recruitment_m$log_rzero$estimated <- log_rzero$estimated
  recruitment_m$log_rzero$is_random_effect <- log_rzero$is_randome_effect

  # Set up logit_steep parameters
  recruitment_m$logit_steep$value <- logit_steep$value
  recruitment_m$logit_steep$estimated <- logit_steep$estimated
  recruitment_m$logit_steep$is_random_effect <- logit_steep$is_randome_effect

  # Set up log_sigma_recruit parameters
  recruitment_m$log_sigma_recruit$value <- log_sigma_recruit$value
  recruitment_m$log_sigma_recruit$estimated <- log_sigma_recruit$estimated
  recruitment_m$log_sigma_recruit$is_random_effect <- log_sigma_recruit$is_randome_effect

  # Set up log_devs parameters
  recruitment_m$log_devs <- log_devs$log_devs
  recruitment_m$estimate_log_devs <- log_devs$estimate_log_devs

  # Print the arguments used
  print_arguments(
    log_rzero = log_rzero,
    logit_steep = logit_steep,
    log_sigma_recruit = log_sigma_recruit,
    log_devs = log_devs
  )
  
  return(recruitment_m)
}

# Example: prepare inputs for option 1
log_rzero <- list(
  value = exp(parfinal$mean_log_recruit + log(1e9)),
  estimated = TRUE,
  is_randome_effect = FALSE
)

logit_steep <- list(
  value = .99999,
  estimated = FALSE,
  is_randome_effect = FALSE
)

log_sigma_recruit <- logit_steep
log_sigma_recruit$value <- parfinal$sigmaR

log_devs <- list(
  log_devs = parfinal$dev_log_recruit[-1],
  estimate_log_devs = TRUE
)

# Apply option 1 function
recruitment_opt1 <- setup_BevertonHolt_recruitment_opt1(
  log_rzero,
  logit_steep,
  log_sigma_recruit,
  log_devs
)

# Option 2: organize function parameters by fields of FIMS parameters (using a list of vectors) -----------------------------------------
setup_BevertonHolt_recruitment_opt2 <- function(recruitment) {
  r <- recruitment
  recruitment_m <- methods::new(BevertonHoltRecruitment)
  recruitment_m$log_sigma_recruit$value <- log(r$init_pars[3])
  recruitment_m$log_rzero$value <- log(r$init_pars[1])
  recruitment_m$log_rzero$is_random_effect <- FALSE
  recruitment_m$log_rzero$estimated <- TRUE ## !(1 %in% r$fix_pars)
  steep <- r$init_pars[2]
  recruitment_m$logit_steep$value <- -log(1.0 - steep) + log(steep - 0.2)
  recruitment_m$logit_steep$is_random_effect <- FALSE
  recruitment_m$logit_steep$estimated <- FALSE ## !(2 %in% r$fix_pars)
  recruitment_m$estimate_log_devs <- TRUE
  recruitment_m$log_devs <- r$init_recdevs
  print_arguments(recruitment = recruitment)
  return(recruitment_m)
}

# Example: prepare inputs for option 2
recruitment_inputs_opt2 <- list(
  ## R0, steepness, sigmaR
  init_pars = c(exp(parfinal$mean_log_recruit + log(1e9)), .99999, parfinal$sigmaR),
  fix_pars = c(2:3),
  init_recdevs = parfinal$dev_log_recruit[-1],
  re = c("none", "none")
)

# Apply optoin 2 function
recruitment_opt2 <- setup_BevertonHolt_recruitment_opt2(recruitment_inputs_opt2)

# Option 3: Organize parameters by fields of FIMS parameters (using a list of named lists) -------------------------------------

setup_BevertonHolt_recruitment_opt3 <- function(initial_values, is_estimated, is_random_effect) {
  recruitment_m <- methods::new(BevertonHoltRecruitment)

  # Set initial parameter values
  recruitment_m$log_rzero$value <- initial_values$log_rzero
  recruitment_m$logit_steep$value <- initial_values$logit_steep
  recruitment_m$log_sigma_recruit$value <- initial_values$log_sigma_recruit
  recruitment_m$log_devs <- initial_values$log_devs

  # Set whether each parameter is estimated
  recruitment_m$log_rzero$estimated <- is_estimated$log_rzero
  recruitment_m$logit_steep$estimated <- is_estimated$logit_steep
  recruitment_m$log_sigma_recruit$estimated <- is_estimated$log_sigma_recruit
  recruitment_m$estimate_log_devs <- is_estimated$log_devs

  # Set whether each parameter has random effects
  recruitment_m$log_rzero$is_random_effect <- is_random_effect$log_rzero
  recruitment_m$logit_steep$is_random_effect <- is_random_effect$logit_steep
  recruitment_m$log_sigma_recruit$estimated <- is_estimated$log_sigma_recruit

  print_arguments(
    initial_values = initial_values,
    is_estimated = is_estimated,
    is_random_effect = is_random_effect
  )
  
  return(recruitment_m)
}

# Example: prepare inputs for option 3
recruitment_inputs_opt3 <- list(
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

# Apply option 3 function
recruitment_opt3 <- setup_BevertonHolt_recruitment_opt3(
  initial_values = recruitment_ctl$initial_values,
  is_estimated = recruitment_ctl$is_estimated,
  is_random_effect = recruitment_ctl$is_random_effect
)
