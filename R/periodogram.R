#' Function to get the peak weeks from a spectral periodogram
#'
#' @param x dataset
#' @param counts variable within x containing counts or rates
#' @param start_week the first week in your dataset, defaults to c(2014, 1)
#' @param period how many units in a year (52 for weeks)
#' @output whether want to return a spectral periodogram ("periodogram") or
#' a vector of the peak weeks ("weeks")
#'
#' @author Alex Spina
#'
#' @importFrom dplyr as_tibble select
#' @importFrom zoo zooreg
#' @importFrom stats spec.pgram
#'
#' @export

periodogram <- function(x,
                        counts,
                        start_week = c(2014, 1),
                        period = 52,
                        output = "weeks") {


  ## make sure is not a tsibble, filter to project and only keep columns of interest
  prepare_data <- dplyr::as_tibble(x)
  # prepare_data <- prepare_data[prepare_data[[strata]] == j, ]
  prepare_data <- dplyr::select(prepare_data, {{counts}})

  ## create an intermediate "zoo" time series to be able to use with spec.pgram
  zoo_cases <- zoo::zooreg(prepare_data,
                           start = start_week, frequency = period)

  ## get a spectral periodogram not using fast fourier transform
  periodo <- spec.pgram(zoo_cases, fast = FALSE, plot = FALSE)

  ## return the peak weeks
  periodo_weeks <- 1 / periodo$freq[order(-periodo$spec)] * period

  if (output == "weeks") {
    periodo_weeks
  } else {
    periodo
  }

}

