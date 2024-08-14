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