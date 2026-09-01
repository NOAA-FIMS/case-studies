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
        -c(observed, timing),
        ~ if (dplyr::n_distinct(.x, na.rm = FALSE) == 1) dplyr::first(.x) else NA
      ),
      observed = mean(observed),
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

#' @param rdat An R object that was read in using [dget()] on a file saved from
#'   ADMB2R.
#' @details
#' Inside the function, the object `param catch_weight_conversion` is created
#' based on the value in `rdat[[info]][[units.landings]]` to convert your
#' catch time series to metric tons. Check that the conversion was done right.
bam_data_to_fims <- function(rdat) {
  # CV is arithmetic space and what is used to fit in BAM, which we convert to
  # standard deviation and create_default_DlnormDistribution takes the log of it
  # for you so we want sqrt(log(1.0 + fleet1_catch_cv^2))

  catch_weight_conversion <- dplyr::case_when(
    rdat[["info"]][["units.landings"]] == "1000 lb whole" ~ 0.453592,
    rdat[["info"]][["units.landings"]] == "mt" ~ 1.0
  )

  catch <- dplyr::select(
    rdat$t.series,
    year,
    dplyr::matches("^L.*\\.ob|cv\\.L.*", ignore.case = FALSE)
  ) |> 
    dplyr::rename_with(.f = \(x) gsub("\\.ob", "", x)) |>
    tidyr::pivot_longer(
      cols = -year,
      names_to = c(".value", "fleet"),
      names_pattern = "^(cv\\.L|L)\\.(.*)"
    ) |>
    dplyr::filter(!is.na(L)) |>
    dplyr::mutate(
      sd = sqrt(log(1.0 + cv.L^2)),
      uncertainty = glue::glue(
        "~dlnorm(meanlog = log_catch_expected, sdlog = {sd})"
      ),
      type = "catch",
      L = L * catch_weight_conversion,
      unit = "mt" # Original units were "1000 lbs" for cobia
    ) |>
    dplyr::select(-cv.L, -sd) |>
    dplyr::rename(observed = L, timing = year)

  index <- dplyr::select(
    rdat$t.series,
    year,
    dplyr::matches("^U.*\\.ob|cv\\.U.*", ignore.case = FALSE)
  ) |> 
    dplyr::rename_with(.f = \(x) gsub("\\.ob", "", x)) |>
    tidyr::pivot_longer(
      cols = -year,
      names_to = c(".value", "fleet"),
      names_pattern = "^(cv\\.U|U)\\.(.*)"
    ) |>
    dplyr::filter(!is.na(U)) |>
    dplyr::mutate(
      sd = sqrt(log(1.0 + cv.U^2)),
      uncertainty = glue::glue(
        "~dlnorm(meanlog = log_catch_expected, sdlog = {sd})"
      ),
      type = "index"
    ) |>
    dplyr::select(-cv.U, -sd) |>
    dplyr::rename(observed = U, timing = year)

  age_comp <- purrr::imap_dfr(
    rdat$comp.mats[grep("acomp.*ob", names(rdat$comp.mats))], ~ {
    .x |> 
      as.data.frame() |>
      tibble::rownames_to_column(var = "timing") |> 
      dplyr::mutate(fleet = gsub("acomp.|.ob", "", .y))
  }) |>
    tidyr::pivot_longer(
      cols = -c(timing, fleet),
      names_to = "age",
      values_to = "observed"
    ) |>
    dplyr::mutate(timing = as.numeric(timing)) |>
    dplyr::left_join(
      rdat$t.series |>
        dplyr::select(year, dplyr::matches("acomp.*n$")) |>
        dplyr::rename_with(
          .fn = \(x) gsub(pattern = "acomp\\.|\\.n$", "", x = x)
        ) |>
        tidyr::pivot_longer(
          cols = -c(year),
          names_to = "fleet",
          values_to = "n"
        ),
      by = c("timing" = "year", "fleet")
    ) |>
    dplyr::mutate(
      age = as.numeric(age),
      unit = "proportion",
      uncertainty = dplyr::case_when(
        n == -99999 ~ NA_character_,
        is.na(n) ~ NA_character_,
        n > 0 ~ glue::glue("~dmultinom(prob = agecomp_proportion, size = {n})")
      )
    ) |>
    dplyr::select(-n)

  if (length(grep("lcomp.*ob", names(rdat$comp.mats))) > 0) {
    length_comp <- purrr::imap_dfr(
      rdat$comp.mats[grep("lcomp.*ob", names(rdat$comp.mats))], ~ {
      .x |> 
        as.data.frame() |>
        tibble::rownames_to_column(var = "timing") |> 
        dplyr::mutate(fleet = gsub("lcomp.|.ob", "", .y))
    }) |>
      tidyr::pivot_longer(
        cols = -c(timing, fleet),
        names_to = "length",
        values_to = "observed"
      ) |>
      dplyr::mutate(timing = as.numeric(timing)) |>
      dplyr::left_join(
        rdat$t.series |>
          dplyr::select(year, dplyr::matches("lcomp.*n$")) |>
          dplyr::rename_with(
            .fn = \(x) gsub(pattern = "lcomp\\.|\\.n$", "", x = x)
          ) |>
          tidyr::pivot_longer(
            cols = -c(year),
            names_to = "fleet",
            values_to = "n"
          ),
        by = c("timing" = "year", "fleet")
      ) |>
      dplyr::mutate(
        length = as.numeric(length),
        unit = "proportion",
        uncertainty = dplyr::case_when(
          n == -99999 ~ NA_character_,
          is.na(n) ~ NA_character_,
          n > 0 ~ glue::glue(
            "~dmultinom(prob = lengthcomp_proportion, size = {n})"
          )
        )
      ) |>
      dplyr::select(-n)
  } else {
    length_comp <- data.frame()
  }

  weight_at_age <- data.frame(
    type = "weight_at_age",
    fleet = NA_character_,
    age = seq(rdat$a.series$age),
    timing = NA_integer_,
    observed = rdat$a.series$wgt.mt,
    unit = "mt",
    uncertainty = NA_character_
  )

  data_4_model <- FIMS::FIMSFrame(
    dplyr::bind_rows(catch, index, age_comp, length_comp, weight_at_age)
  )
}
