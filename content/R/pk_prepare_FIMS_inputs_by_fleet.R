# Helper functions -----------------------------------------------

# This function sets the initial parameter values, estimated flags, and random effect flags
# for a logistic form (e.g., selectivity module or maturity module) within a module.
use_logistic_form <- function(module, initial_values,
                              is_estimated, is_random_effect) {
  # Set initial parameter values for each parameter
  module$inflection_point$value <- initial_values$inflection_point
  module$slope$value <- initial_values$slope

  # Set whether each parameter is estimated
  module$inflection_point$estimated <- is_estimated$inflection_point
  module$slope$estimated <- is_estimated$slope

  # Set whether each parameter has random effects
  module$inflection_point$is_random_effect <- is_random_effect$inflection_point
  module$slope$is_random_effect <- is_random_effect$slope

  return(module)
}

# This function sets the initial parameter values, estimated flags, and random effect flags
# for a double logistic form within a module.
use_double_logistic_form <- function(module, initial_values,
                                     is_estimated, is_random_effect) {
  # Set initial parameter values for both ascending and descending parts
  module$inflection_point_asc$value <- initial_values$inflection_point_asc
  module$slope_asc$value <- initial_values$slope_asc
  module$inflection_point_desc$value <- initial_values$inflection_point_desc
  module$slope_desc$value <- initial_values$slope_desc

  # Set whether each parameter is estimated
  module$inflection_point_asc$estimated <- is_estimated$inflection_point_asc
  module$slope_asc$estimated <- is_estimated$slope_asc
  module$inflection_point_desc$estimated <- is_estimated$inflection_point_desc
  module$slope_desc$estimated <- is_estimated$slope_desc

  # Set whether each parameter has random effects
  module$inflection_point_asc$is_random_effect <- is_random_effect$inflection_point_asc
  module$slope_asc$is_random_effect <- is_random_effect$slope_asc
  module$inflection_point_desc$is_random_effect <- is_random_effect$inflection_point_desc
  module$slope_desc$is_random_effect <- is_random_effect$slope_desc

  return(module)
}

# This function sets the fleet data, including selectivity, fishing mortality,
# catchability, and observational error for each fleet.
set_fleet <- function(fleet_data, population_data, is_estimated_obs_error, selectivity,
                      Fmort, catchability) {
  nfleets <- population_data$nfleets

  fishing_fleet_names <- population_data$fishing_fleet_names
  survey_fleet_names <- population_data$survey_fleet_names
  fleet_names <- population_data$fleet_names

  # Initialize an empty list to store fleet objects
  index_m <- age_comp_m <- age_comp_uncertainty <-
    selectivity_m <- fleet_m <- vector(mode = "list", length = nfleets)

  for (i in 1:nfleets) {
    index_m[[i]] <- methods::new(Index, nyears)

    if (fleet_names[i] %in% fishing_fleet_names) {
      index_m[[i]]$index_data <- FIMS::m_landings(fleet_data)
    } else {
      index_m[[i]]$index_data <- FIMS::m_index(fleet_data, fleet_names[i])
    }

    age_comp_uncertainty[[i]] <- dplyr::filter(
      .data = as.data.frame(fleet_data@data),
      name == fleet_names[i] & type == "age"
    ) |>
      dplyr::pull(uncertainty)

    age_comp_m[[i]] <- methods::new(AgeComp, nyears, nages)
    age_comp_m[[i]]$age_comp_data <- FIMS::m_agecomp(fleet_data, fleet_names[i]) *
      age_comp_uncertainty[[i]]

    if (selectivity[[i]]$form == "LogisticSelectivity") {
      selectivity_temp <- methods::new(LogisticSelectivity)
      selectivity_m[[i]] <- use_logistic_form(
        module = selectivity_temp,
        initial_values = selectivity[[i]]$initial_values,
        is_estimated = selectivity[[i]]$is_estimated,
        is_random_effect = selectivity[[i]]$is_random_effect
      )
    }

    if (selectivity[[i]]$form == "DoubleLogisticSelectivity") {
      selectivity_temp <- methods::new(DoubleLogisticSelectivity)
      selectivity_m[[i]] <- use_double_logistic_form(
        module = selectivity_temp,
        initial_values = selectivity[[i]]$initial_values,
        is_estimated = selectivity[[i]]$is_estimated,
        is_random_effect = selectivity[[i]]$is_random_effect
      )
    }

    fleet_m[[i]] <- methods::new(Fleet)

    # Set nyears and nages
    fleet_m[[i]]$nages <- population_data$nages
    fleet_m[[i]]$nyears <- population_data$nyears

    obs_error <- dplyr::filter(
      .data = as.data.frame(fleet_data@data),
      name == fleet_names[i] & type == population_data$index_types[i]
    ) |>
      dplyr::pull(uncertainty)
    fleet_m[[i]]$log_obs_error <- log(obs_error)

    fleet_m[[i]]$estimate_obs_error <- is_estimated_obs_error[[i]]

    fleet_m[[i]]$is_survey <- Fmort_ctl$is_survey[[i]]
    fleet_m[[i]]$log_Fmort <- Fmort_ctl$initial_values[[i]]
    fleet_m[[i]]$estimate_F <- Fmort_ctl$estimate_F[[i]]
    fleet_m[[i]]$random_F <- Fmort_ctl$random_F[[i]]

    fleet_m[[i]]$log_q <- catchability_ctl$log_q[[i]]
    fleet_m[[i]]$estimate_q <- catchability_ctl$estimate_q[[i]]
    fleet_m[[i]]$random_q <- catchability_ctl$random_q[[i]]

    fleet_m[[i]]$SetAgeCompLikelihood(i)
    fleet_m[[i]]$SetIndexLikelihood(i)
    fleet_m[[i]]$SetSelectivity(selectivity_m[[i]]$get_id())
    fleet_m[[i]]$SetObservedIndexData(index_m[[i]]$get_id())
    fleet_m[[i]]$SetObservedAgeCompData(age_comp_m[[i]]$get_id())
  }
  return(fleet_m)
}

# This function sets the recruitment parameters, including initial values,
# estimated flags, and random effect flags, for a specified recruitment form.
set_recruitment <- function(form, initial_values, is_estimated, is_random_effect) {
  if (form == "BevertonHoltRecruitment") {
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
  }
  return(recruitment_m)
}

# This function sets the growth parameters, including initial values,
# estimated flags, and random effect flags, for a specified growth form.
set_growth <- function(population_data, form, initial_values,
                       is_estimated = NULL, is_random_effect = NULL) {
  if (form == "EWAAgrowth") {
    growth_m <- methods::new(EWAAgrowth)
    growth_m$ages <- population_data$ages
    growth_m$weights <- initial_values$weights
  }
  return(growth_m)
}

# This function sets the maturity parameters, including initial values,
# estimated flags, and random effect flags, for a specified maturity form.
set_maturity <- function(form, initial_values, is_estimated, is_random_effect) {
  if (form == "LogisticMaturity") {
    maturity_temp <- methods::new(LogisticMaturity)
    maturity_m <- use_logistic_form(
      module = maturity_temp,
      initial_values = initial_values,
      is_estimated = is_estimated,
      is_random_effect = is_random_effect
    )
  }

  return(maturity_m)
}

# This function sets up the population parameters, including initial values,
# estimated flags, maturity, growth, and recruitment input. Users can use
# the set_maturity(), set_growth(), and set_recruitment() functions to set up modules
# before passing them into set_population(). See the example of using set_growth()
# for setting a module and passing it into set_population().
set_population <- function(population_data, initial_values, is_estimated,
                           maturity, growth, recruitment) {
  # Create a new population object
  population_m <- methods::new(Population)

  # Set basic population data
  population_m$nages <- population_data$nages
  population_m$ages <- population_data$ages
  population_m$nfleets <- population_data$nfleets
  population_m$nseasons <- population_data$nseasons
  population_m$nyears <- population_data$nyears

  # Set initial values for log_M and log_init_naa
  population_m$log_M <- initial_values$log_M
  population_m$log_init_naa <- initial_values$log_init_naa

  # Set flags indicating which parameters should be estimated
  population_m$estimate_M <- is_estimated$log_M
  population_m$estimate_init_naa <- is_estimated$log_init_naa

  # Set up the maturity module
  if (typeof(maturity) == "S4") {
    maturity_m <- maturity
  } else {
    maturity_m <- set_maturity(
      form = maturity$form,
      initial_values = maturity$initial_values,
      is_estimated = maturity$is_estimated,
      is_random_effect = maturity$is_random_effect
    )
  }

  # Set up the growth module
  if (typeof(growth) == "S4") {
    growth_m <- growth
  } else {
    growth_m <- set_growth(
      population_data = population_data,
      form = growth$form,
      initial_values = growth$initial_values
    )
  }

  # Set up the recruitment module
  if (typeof(recruitment) == "S4") {
    recruitment_m <- recruitment
  } else {
    recruitment_m <- set_recruitment(
      form = recruitment$form,
      initial_values = recruitment$initial_values,
      is_estimated = recruitment$is_estimated,
      is_random_effect = recruitment$is_random_effect
    )
  }

  # Assign the maturity, growth, and recruitment modules to the population
  population_m$SetMaturity(maturity_m$get_id())
  population_m$SetGrowth(growth_m$get_id())
  population_m$SetRecruitment(recruitment_m$get_id())

  return(population_m)
}

# Examples for using helper functions -------------------------------------

# set up fleet data
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

# set up population data
population_data <- list(
  nages = age_frame@n_ages,
  ages = age_frame@ages,
  nfleets = nfleets,
  fishing_fleet_names = fishing_fleet_names,
  survey_fleet_names = survey_fleet_names,
  fleet_names = fleet_names,
  index_types = index_types,
  nseasons = 1,
  nyears = age_frame@n_years
)

# Define whether observation error should be estimated for each fleet
is_estimated_obs_error <- list(FALSE, FALSE, FALSE, FALSE)

# Define selectivity for fishing fleet and surveys using Double Logistic and Logistic forms
selectivity_fsh_mean <-
  selectivity_srv2 <- selectivity_srv6 <-
  list(
    form = list("DoubleLogisticSelectivity"),
    initial_values = list(
      inflection_point_asc = parfinal$inf1_fsh_mean,
      slope_asc = exp(parfinal$log_slp1_fsh_mean),
      inflection_point_desc = parfinal$inf2_fsh_mean,
      slope_desc = exp(parfinal$log_slp2_fsh_mean)
    ),
    is_estimated = list(
      inflection_point_asc = TRUE,
      slope_asc = TRUE,
      inflection_point_desc = TRUE,
      slope_desc = TRUE
    ),
    is_random_effect = list(
      inflection_point_asc = FALSE,
      slope_asc = FALSE,
      inflection_point_desc = FALSE,
      slope_desc = FALSE
    )
  )

# Adjust initial values for selectivity for Survey 2
selectivity_srv2$initial_values <- list(
  inflection_point_asc = parfinal$inf1_srv2,
  slope_asc = exp(parfinal$log_slp1_srv2),
  inflection_point_desc = parfinal$inf2_srv2,
  slope_desc = exp(parfinal$log_slp2_srv2)
)

# Set estimated flags for descending inflection point and slope to FALSE for Survey 2
selectivity_srv2$is_estimated$inflection_point_desc <-
  selectivity_srv2$is_estimated$slope_desc <- FALSE

# Adjust initial values for selectivity for Survey 6
selectivity_srv6$initial_values <- list(
  inflection_point_asc = parfinal$inf1_srv6,
  slope_asc = exp(parfinal$log_slp1_srv6),
  inflection_point_desc = parfinal$inf2_srv6,
  slope_desc = exp(parfinal$log_slp2_srv6)
)

# Set estimated flags for ascending inflection point and slope to FALSE for Survey 6
selectivity_srv6$is_estimated$inflection_point_asc <-
  selectivity_srv6$is_estimated$slope_asc <- FALSE

# Define selectivity for Survey 3 using Logistic form
selectivity_srv3 <- list(
  form = list("LogisticSelectivity"),
  initial_values = list(
    inflection_point = parfinal$inf1_srv3,
    slope = exp(parfinal$log_slp1_srv3)
  ),
  is_estimated = list(
    inflection_point = TRUE,
    slope = TRUE
  ),
  is_random_effect = list(
    inflection_point = FALSE,
    slope = FALSE
  )
)

# Combine all selectivity controls into a list (order matters!)
selectivity_ctl <- list(
  selectivity_fsh_mean,
  selectivity_srv2,
  selectivity_srv3,
  selectivity_srv6
)

# Define fishing mortality controls
Fmort_ctl <- list(
  is_survey = list(FALSE, TRUE, TRUE, TRUE),
  initial_values = list(log(pkfitfinal$rep$F), 0, 0, 0),
  estimate_F = list(TRUE, FALSE, FALSE, FALSE),
  random_F = list(FALSE, FALSE, FALSE, FALSE)
)

# Define catchability controls
catchability_ctl <- list(
  log_q = list(
    0, parfinal$log_q2_mean,
    parfinal$log_q3_mean, parfinal$log_q6
  ),
  estimate_q = list(FALSE, TRUE, TRUE, TRUE),
  random_q = list(FALSE, FALSE, FALSE, FALSE)
)

# Set up the fleet module using the defined controls
fleet <- set_fleet(
  fleet_data = age_frame,
  population_data = population_data,
  is_estimated_obs_error = is_estimated_obs_error,
  selectivity = selectivity_ctl,
  Fmort = Fmort_ctl,
  catchability = catchability_ctl
)

# Define recruitment controls using Beverton-Holt form
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

# Define growth controls using EWAAgrowth form
# growth_ctl <- list(
#   data = population_data,
#   form = "EWAAgrowth",
#   initial_values = list(weights = pkinput$dat$wt_srv1[1, ])
# )

growth_m <- set_growth(
  population_data = population_data,
  form = "EWAAgrowth",
  initial_values = list(weights = pkinput$dat$wt_srv1[1, ])
)

# Define maturity controls using logistic form
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

# Define initial values for log_M and log__init_naa
population_initial_values <- list(
  log_M = log(as.numeric(t(matrix(
    rep(pkfitfinal$rep$M, each = population_data$nyears),
    nrow = population_data$nyears
  )))),
  log_init_naa =
    c(log(pkfitfinal$rep$recruit[1]), log(pkfitfinal$rep$initN)) + log(1e9)
)

# Define estimated flags for log_M and log__init_naa
population_is_estimated <- list(
  log_M = FALSE,
  log_init_naa = FALSE
)

# Set up the population module using the defined controls and initial values
population <- set_population(
  population_data = population_data,
  initial_values = population_initial_values,
  is_estimated = population_is_estimated,
  maturity = maturity_ctl,
  growth = growth_m,
  recruitment = recruitment_ctl
)
