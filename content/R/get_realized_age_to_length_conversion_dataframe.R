#' Build Realized Age-to-Length Conversion Data Frame
#'
#' Converts the flattened realized age-to-length conversion vector from a FIMS
#' report into a tidy data frame with one row per length-age-year combination.
#'
#' @param report A report object returned by [FIMS::get_report()], containing
#'   `"growth_derived_age_to_length_conversion"`.
#' @param data A `FIMSFrame` object used to define model years, ages, and length
#'   bins.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{length}{Numeric length bin.}
#'   \item{age}{Numeric age.}
#'   \item{year}{Integer year.}
#'   \item{proportion}{Numeric proportion value from
#'   the realized age-to-length conversion output.}
#' }
#'
#' @examples
#' # realized_df <- get_realized_age_to_length_conversion_dataframe(
#' #   report = report_fit,
#' #   data = data_4_model
#' # )
get_realized_age_to_length_conversion_dataframe <- function(fit, data) {
  # TODO: 
  #   * can we get years, lengths, and ages from report somewhere?
  #   * should this be generalized to get the input age_to_length_conversion as well?

  report <- FIMS::get_report(fit)
  if (!("growth_derived_age_to_length_conversion" %in% names(report))) {
    cli::cli_abort(
      c(
        "report does not contain 'growth_derived_age_to_length_conversion'.",
        "Try setting `input$model$ReportGrowthDerivedALKTensor(TRUE)`."
      )
    )
  }
  age_to_length_conversion <- report[[
    "growth_derived_age_to_length_conversion"
  ]][[1]]
  years <- FIMS::get_start_year(data):FIMS::get_end_year(data)
  n_lengths <- length(data@lengths)
  n_ages <- length(data@ages)
  n_years <- length(years)

  df <- tibble::tibble(
    length = rep(data@lengths, times = n_ages * n_years),
    age = rep(rep(data@ages, each = n_lengths), times = n_years),
    year = rep(years, each = n_lengths * n_ages),
    proportion = as.numeric(age_to_length_conversion)
  )

  if (nrow(df) != length(age_to_length_conversion)) {
    cli::cli_abort(
      "realized_age_to_length_conversion dataframe size does not match source vector length"
    )
  }

  df |>
    dplyr::mutate(
      length = as.numeric(length),
      age = as.numeric(age),
      year = as.integer(year)
    )
}
