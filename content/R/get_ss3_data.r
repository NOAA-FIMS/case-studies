#' Convert SS3 data into format required by FIMS (only works for petrale so far)
#'
#' Uses output from `r4ss::SS_read()` or `r4ss::SS_readdat()` and does
#' filtering, simplifying, and reformatting.
#'
#' @param dat The `dat` element of the list created by `r4ss::SS_read()` or the 
#' output from running `r4ss::SS_readdat()` directly.
#' @param fleets Which fleets to include in the processed output.
#' @return A data frame that can be passed to `FIMS::FIMSFrameAge()`
#' @author Ian G. Taylor
#' @export

get_ss3_data <- function(dat, fleets) {
  # create empty data frame
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

  # Q: is it true that we can only have a single landings fleet? 
  #    m_landings() doesn't accept a fleet name.

  # aggregate landings across fleets
  catch_by_year <- dat$catch |>
    dplyr::group_by(year) |>
    dplyr::summarize(catch = sum(catch), uncertainty = mean(catch_se))

  # convert landings to FIMSFrame format
  landings <- data.frame(
    type = "landings",
    name = paste0("fleet1"), # landings aggregated to fleet 1
    age = NA,
    datestart = paste0(catch_by_year$year, "-01-01"),
    dateend = paste0(catch_by_year$year, "-12-31"),
    value = catch_by_year$catch,
    unit = "mt",
    uncertainty = catch_by_year$uncertainty
  )

  # check for any gaps in landings time series
  years <- min(catch_by_year$year):max(catch_by_year$year)
  if (!all(years %in% catch_by_year$year)) {
    stop("missing years in landings")
  }

  # convert indices to FIMSFrame format
  index_info <- dat$CPUE |>
    dplyr::filter(index %in% fleets) |>
    dplyr::select(year, index, obs, se_log)

  # add -999 for missing years
  # create empty data frame for all combinations of year and fleet
  index_info_empty <- tidyr::expand_grid(
    year = years,
    index = fleets
  ) |> dplyr::mutate(obs = -999, se_log = 1)
  # combine the two data frames and remove redundant rows
  index_info <- rbind(index_info, index_info_empty) |>
    dplyr::distinct(year, index, .keep_all = TRUE) |>
    dplyr::arrange(index, year)

  indices <- data.frame(
    type = "index",
    name = paste0("fleet", index_info$index),
    age = NA,
    datestart = paste0(index_info$year, "-01-01"),
    dateend = paste0(index_info$year, "-12-31"),
    value = index_info$obs,
    unit = "",
    uncertainty = index_info$se_log
  )

  # partially convert age comps (filter, make into long table)

  # first rescale females to sum to 1.0
  # (data processing step had females + males sum to 100 for no good reason)
  dat$agecomp$sum_fem <-
    dat$agecomp |>
    dplyr::select(dplyr::starts_with("f", ignore.case = FALSE)) |> # get female comps
    rowSums()
  # couldn't figure out dplyr approach to rescaling the subset of columns
  # with female proportions to sum to 1.0
  dat$agecomp[, names(dat$agecomp) %in% paste0("f", ages)] <-
    dat$agecomp[, names(dat$agecomp) %in% paste0("f", ages)] /
      dat$agecomp$sum_fem

  # further processing
  age_info <-
    dat$agecomp |>
    # specific to petrale:
    # fleet 4 used conditional age-at-length data with marginal observations
    # entered as fleet == -4 (to exclude from likelihood due to redundancy)
    # using only marginals in this case and exclude CAAL data
    dplyr::filter(FltSvy %in% c(1, 2, 3, -4)) |>
    dplyr::mutate(FltSvy = abs(FltSvy)) |> # convert negative fleet to positive
    dplyr::filter(FltSvy %in% fleets) |> # subset for fleets requested
    dplyr::select(!dplyr::starts_with("m", ignore.case = FALSE)) |> # exclude male comps
    tidyr::pivot_longer( # convert columns f1...f17 to values in a new "age" colum of a longer table
      cols = dplyr::starts_with("f", ignore.case = FALSE),
      names_to = "age",
      values_to = "value"
    ) |>
    dplyr::mutate(age = as.numeric(substring(age, first = 2))) |> # convert "f17" to 17
    dplyr::select(Yr, FltSvy, Nsamp, age, value)

  # add -999 for missing years
  # create empty data frame for all combinations of year, fleet, and age
  age_info_empty <- tidyr::expand_grid(
    Yr = years,
    FltSvy = fleets,
    age = ages
  ) |> dplyr::mutate(Nsamp = 1, value = -999 - 0.001)
  # combine the two data frames and remove redundant rows
  # NOTE: this removes some age comp data because there
  # were years with multiple observations from the same fleet
  # due to multiple ageing error matrices
  age_info <- rbind(age_info, age_info_empty) |>
    dplyr::distinct(Yr, FltSvy, age, .keep_all = TRUE) |>
    dplyr::arrange(FltSvy, Yr, age)

  # finish converting age comps to FIMSFrame format
  agecomps <- data.frame(
    type = "age",
    name = paste0("fleet", abs(age_info$FltSvy)), # abs to include fleet == -4
    age = age_info$age,
    datestart = paste0(age_info$Yr, "-01-01"),
    dateend = paste0(age_info$Yr, "-12-31"),
    value = age_info$value + 0.001, # add constant to avoid 0 values
    unit = "",
    # Q: should uncertainty here be the total sample size across bins, or the samples within the bin?
    # uncertainty = round(age_info$Nsamp * age_info$value)
    uncertainty = round(age_info$Nsamp)
  )

  # combine all data sources
  res <- rbind(res, landings, indices, agecomps)
}
