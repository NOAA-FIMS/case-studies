#' @examples
#' execute_double_logistic(x = 1:15, 0.5, 0.1, 5, 1) |>
#'   ggplot2::ggplot(ggplot2::aes(x = x, y = value)) +
#'   ggplot2::geom_line()
execute_double_logistic <- function(
  x,
  slope_asc,
  inflection_point_asc,
  slope_desc,
  inflection_point_desc
) {
  out <- (1.0) /
  (1.0 + exp(-1.0 * slope_asc * (x - inflection_point_asc))) *
  (1 -
    (1.0 /
     (1.0 + exp(-1.0 * slope_desc * (x - inflection_point_desc)))
    )
  )
  return(
    dplyr::tibble(
      x = x,
      value = out
    )
  )
}

#' @examples
#' execute_logistic(x = 0:15, 1.5, 4.5) |>
#'   ggplot2::ggplot(ggplot2::aes(x = x, y = value)) +
#'   ggplot2::geom_line()
execute_logistic <- function(x, slope, inflection_point) {
  out <- (1.0) /
  (1.0 + exp(-1.0 * slope * (x - inflection_point)))
  return(
    dplyr::tibble(
      x = x,
      value = out
    )
  )
}

execute_double_logistic <- function(
  x,
  slope_asc,
  inflection_point_asc,
  slope_desc,
  inflection_point_desc
) {
  out <- (
    (1) /
    (1.0 + exp(-1.0 * slope_asc * (x - inflection_point_asc)))
  ) *
  (1.0 - 
    (1.0) /
    (1.0 + exp(-1.0 * slope_desc * (x - inflection_point_desc)))
  )
  return(
    dplyr::tibble(
      x = x,
      value = out
    )
  )
}

#' Get parameters from the estimates table of a fitted object
#'
#' @param string A regular expression you want to search for in names.
#' @param fit A FIMSFit object.
#' @return
#' A named vector of parameter estimates.
get_parameter <- function(string, fit) {
  out <- FIMS::get_estimates(fit) |>
    dplyr::filter(grepl(string, label, ignore.case = TRUE))
  pars <- dplyr::pull(out, estimated)
  names(pars) <- dplyr::pull(out, label)
  return(pars)
}

#' Add an additional year of data to a long data frame
#'
#' Add an additional, i.e., terminal year of data onto a long data frame that
#' you plan on passing to FIMSFrame. This is particularly useful for
#' weight_at_age_data where you need terminal year + 1 for the final report of
#' spawning biomass that comes from the model.
#'
#' @param data A long data frame.
#' @param timing_to_average A vector of values that are present in the timing
#'   column that you want to average over. For example the last five years of
#'   your data.
#' @param type_to_filter The type of data that you want to add to. For example,
#'   `"weight_at_age"`, which is the default. Available types are
#'   `r glue::glue_collapse(FIMS::fims_input_types, sep = ", ", last = ", and ")`.
#'   The default is `"age_comp"`
#' @param ... Unquoted columns that you want to group over. The typical value is
#'   age.
#' @return
#' A tibble with the same columns as `data`.
add_additional_year <- function(
  data,
  timing_to_average,
  type_to_filter = "weight_at_age",
  ...) {
  grouping_columns <- rlang::enquos(...)
  type_to_filter <- rlang::arg_match(
    type_to_filter,
    values = FIMS::fims_input_types
  )

  filtered_data <- dplyr::filter(
    data,
    type == type_to_filter,
    timing %in% timing_to_average
  )
  terminal_year <- max(filtered_data[["timing"]])

  filtered_data |>
    dplyr::group_by(!!!grouping_columns) |>
    dplyr::summarize(
      dplyr::across(
        -c(value, timing),
        ~ if (dplyr::n_distinct(.x, na.rm = FALSE) == 1) dplyr::first(.x) else NA
      ),
      value = mean(value),
      timing = terminal_year + 1
    ) |>
    dplyr::select(colnames(data))
}

map_time_varying <- function(
  parameter_names,
  parameter_name,
  new_name,
  indexing
) {
  parameter_indices <- grep(parameter_name, parameter_names)
  year_sequence <- seq(parameter_indices)
  change_these_entries <- parameter_indices[indexing]
  parameter_names[change_these_entries] <- gsub(
    "\\d+$",
    new_name,
    parameter_names[change_these_entries]
  )
  parameter_names
}

make_hake_rds_smaller <- function(full_rds_file, out_rds_file) {
  hake <- readRDS(full_rds_file)
  out <- hake[c(
    "startyr",
    "endyr",
    "N_forecast_yrs",
    "catch",
    "FleetNames",
    "mcmc"
  )]
  out[["mcmc"]] <- apply(out[["mcmc"]], 2, median)
  out[["index"]] <- hake[["extra_mcmc"]][["index_fit_posts"]] |>
    tidyr::pivot_longer(
      cols = -c(yr, fleet),
      names_to = "draw",
      values_to = "median"
    ) |>
    dplyr::summarise(
      median = median(median),
      .by = c(yr, fleet)
    )
  out[["spawning_biomass"]] <- out[["mcmc"]][
    grep("SSB_\\d+", names(out[["mcmc"]]), value = TRUE)
  ]
  names(out[["spawning_biomass"]]) <- gsub(
    "SSB_",
    "",
    names(out[["spawning_biomass"]])
  )
  out[["recruitment"]] <- out[["mcmc"]][
    grep("^Recr_\\d+", names(out[["mcmc"]]), value = TRUE)
  ]
  names(out[["recruitment"]]) <- gsub(
    "Recr_",
    "",
    names(out[["recruitment"]])
  )
  out[["rec_dev"]] <- out[["mcmc"]][
    grep("^Main_RecrDev_\\d+", names(out[["mcmc"]]), value = TRUE)
  ]
  names(out[["rec_dev"]]) <- gsub(
    "^Main_RecrDev_",
    "",
    names(out[["rec_dev"]])
  )
  out[["log_Fmort"]] <- log(out[["mcmc"]][
    grep("^F_\\d+", names(out[["mcmc"]]), value = TRUE)
  ])
  names(out[["log_Fmort"]]) <- gsub(
    "F_",
    "",
    names(out[["log_Fmort"]])
  )
  saveRDS(out, out_rds_file)
}
