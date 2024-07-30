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
                      selectivity_module_list, Fmort_ctl, catchability_ctl) {
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

    if (!is.null(selectivity_ctl)) {
      selectivity[[i]] <- set_selectivity(
        form = selectivity_ctl$form[[i]],
        initial_values = selectivity_ctl$initial_values[[i]],
        is_estimated = selectivity_ctl$is_estimated[[i]],
        is_random_effect = selectivity_ctl$is_random_effect[[i]]
      )
    }

    if (!is.null(selectivity_module_list)) {
      selectivity[[i]] <- selectivity_module_list[[i]]
    }

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

set_recruitment <- function(form, initial_values, is_estimated, is_random_effect) {
  if (form == "BevertonHoltRecruitment") {
    recruitment <- methods::new(BevertonHoltRecruitment)

    # Set initial parameter values
    recruitment$log_rzero$value <- initial_values$log_rzero
    recruitment$logit_steep$value <- initial_values$logit_steep
    recruitment$log_sigma_recruit$value <- initial_values$log_sigma_recruit
    recruitment$log_devs <- initial_values$log_devs

    # Set whether each parameter is estimated
    recruitment$log_rzero$estimated <- is_estimated$log_rzero
    recruitment$logit_steep$estimated <- is_estimated$logit_steep
    recruitment$log_sigma_recruit$estimated <- is_estimated$log_sigma_recruit
    recruitment$estimate_log_devs <- is_estimated$log_devs

    # Set whether each parameter has random effects
    recruitment$log_rzero$is_random_effect <- is_random_effect$log_rzero
    recruitment$logit_steep$is_random_effect <- is_random_effect$logit_steep
    recruitment$log_sigma_recruit$estimated <- is_estimated$log_sigma_recruit
  }
  return(recruitment)
}

set_growth <- function(data, form, initial_values,
                       is_estimated = NULL, is_random_effect = NULL) {
  if (form == "EWAAgrowth") {
    growth <- methods::new(EWAAgrowth)
    growth$ages <- data$ages
    # NOTE: FIMS currently cannot use matrix of WAA, so have to ensure constant WAA over time in ASAP file for now
    growth$weights <- initial_values$weights
  }
  return(growth)
}

set_maturity <- function(form, initial_values, is_estimated, is_random_effect) {
  if (form == "LogisticMaturity") {
    maturity <- methods::new(LogisticMaturity)

    # Set initial parameter values
    maturity$inflection_point$value <- initial_values$inflection_point
    maturity$slope$value <- initial_values$slope

    # Set whether each parameter is estimated
    maturity$inflection_point$estimated <- is_estimated$inflection_point
    maturity$slope$estimated <- is_estimated$slope

    # Set whether each parameter has random effects
    maturity$inflection_point$is_random_effect <- is_random_effect$inflection_point
    maturity$slope$is_random_effect <- is_random_effect$slope
  }

  return(maturity)
}

set_population <- function(data, initial_values, is_estimated,
                           maturity, growth, recruitment) {
  population <- methods::new(Population)

  population$nages <- data$nages
  population$ages <- data$ages
  population$nfleets <- data$nfleets
  population$nseasons <- data$nseasons
  population$nyears <- data$nyears

  # Set initial parameter values
  population$log_M <- initial_values$log_M
  population$log_init_naa <- initial_values$log_init_naa

  # Set whether each parameter is estimated
  population$estimate_M <- is_estimated$log_M
  population$estimate_init_naa <- is_estimated$log_init_naa

  if (typeof(maturity) == "S4") {
    maturity_module <- maturity
  } else {
    maturity_module <- set_maturity(
      form = maturity$form,
      initial_values = maturity$initial_values,
      is_estimated = maturity$is_estimated,
      is_random_effect = maturity$is_random_effect
    )
  }

  if (typeof(growth) == "S4") {
    growth_module <- growth
  } else {
    growth_module <- set_growth(
      data = data,
      form = growth$form,
      initial_values = growth$initial_values
    )
  }

  if (typeof(recruitment) == "S4") {
    recruitment_module <- recruitment
  } else {
    recruitment_module <- set_recruitment(
      form = recruitment$form,
      initial_values = recruitment$initial_values,
      is_estimated = recruitment$is_estimated,
      is_random_effect = recruitment$is_random_effect
    )
  }

  population$SetMaturity(maturity_module$get_id())
  population$SetGrowth(growth_module$get_id())
  population$SetRecruitment(recruitment_module$get_id())

  return(population)
}
