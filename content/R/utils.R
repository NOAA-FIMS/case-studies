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

#' Get parameters from the estimates table of a fitted object
#'
#' @param string A regular expression you want to search for in names.
#' @param fit A FIMSFit object.
#' @return
#' A named vector of parameter estimates.
get_parameter <- function(string, fit) {
  out <- get_estimates(fit) |>
    dplyr::filter(grepl(string, name, ignore.case = TRUE))
  pars <- dplyr::pull(out, value)
  names(pars) <- dplyr::pull(out, name)
  return(pars)
}
