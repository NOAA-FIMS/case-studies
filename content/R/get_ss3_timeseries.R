#' Extract time series output from SS3 model and put into a long data frame
#'
#' Simple function to reformat output from the timeseries table returned by
#' `r4ss::SS_output()` to facilitate use of tidyverse style functions and
#' facilitate comparison with FIMS output. Currently only extracts F for a 
#' single fleet.
#'
#' @param model The output from `r4ss::SS_output()`
#' @param platform A user-specified label to differentiate from additional
#' output associated with FIMS or other platforms
#' @return A long data frame (actually a tibble) with time series output for 
#' 4 quantities (so far)
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#'   # read SS3 models from location on Ian's computer
#'   p1 <- r4ss::SS_output("c:/ss/Petrale/Petrale2023/petrale/models/2023.a034.001/")
#'   p2 <- r4ss::SS_output("c:/ss/Petrale/Petrale2023/petrale/models/2023.a050.002_FIMS_case-study_wtatage/")
#'   # # saving all model output creates large files (7MB for original)
#'   # saveRDS(p1, file = "content/data_files/NWFSC-petrale-SS3-original.rds")
#'   # saveRDS(p2, file = "content/data_files/NWFSC-petrale-SS3-simplified.rds")
#'   # combine SS3 model time series into data frame using function above
#'   timeseries_compare <- rbind(
#'     get_ss3_timeseries(model = p1, platform = "ss3_original"),
#'     get_ss3_timeseries(model = p2, platform = "ss3_simplified")
#'   )
#'   # save data frame of time series results to compare with FIMS
#'   saveRDS(timeseries_compare, file = "content/data_files/NWFSC-petrale-SS3-timeseries.rds")
#' }

get_ss3_timeseries <- function(model, platform = "ss3") {
  timeseries_ss3 <- model$timeseries |>
    dplyr::filter(Yr %in% timeseries$year) |> # filter for matching years only (no forecast)
    dplyr::select(Yr, Bio_all, SpawnBio, Recruit_0, "F:_1") |> # select quants of interest
    dplyr::rename( # change to names used with FIMS
      year = Yr,
      biomass = Bio_all, ssb = SpawnBio, recruitment = Recruit_0, F_mort = "F:_1"
    ) |>
    dplyr::mutate(ssb = 1000 * ssb) |>
    tidyr::pivot_longer( # convert quantities in separate columns into a single value column
      cols = -1,
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::arrange(type) |> # sort by type instead of year
    dplyr::mutate(platform = platform)
  return(timeseries_ss3)
}
