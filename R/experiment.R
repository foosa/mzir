#' ---
#' @file: experiment.R
#' @author: True Merrill
#' @date: March 20 2019
#' ---


# --- Constructors for the experiment class ---

#' Create a new experiment object.
#'
#' @note An experiment is essentially a paired set of data frames, one of which
#'   stores time-series data describing the phase response of the system, and a
#'   second which describes how the time series is divided into zero or more
#'   intervals.  The intervals describe a period of time where the concentration
#'   of the analytes above the sensor surface is constant.
#'
#' @param df data frame storing a time series
#' @param ivl_df data frame storing the intervals.  If this is unspecified, the
#'   experiment object will contain no intervals.
#' @param time column containing the independent degree of freedom.  If this is
#'   unspecified, the first column is assumed to be the independent degree of
#'   freedom
new_experiment <- function(df, ivl_df = NULL, time = NULL) {
  # Create list of intervals
  ivls <- list()
  if (is.data.frame(ivl_df)) {
    for (row_index in 1:nrow(ivl_df)) {
      ivl <- new_interval_from_row(df, ivl_df, row_index, time = time)
      ivls[[row_index]] <- ivl
    }
  }

  # Create object
  obj <- structure(
    list(
      df = df,
      ivls = ivls
    ),
    class = "experiment"
  )
}

# --- Methods for the experiment class ---

#' Create a new interval within the experiment and attach the new interval to
#' the intervals list
#'
#' @param ex experiment object
#' @param time column containing the independent degree of freedom.  If this is
#'   unspecified, the first column is assumed to be the independent degree of
#'   freedom.
#' @param time_start the starting time.  If this is unspecified, `time_start` is
#'   set to the minimum element in the `time` column.
#' @param time_stop the stopping time.  If this is unspecified, `time_stop` is
#'   set to the maximum element in the `time` column.
#' @param concentrations a named numeric vector storing the concentrations for
#'   the current interval.  If this is unspecified, `concentrations` is set to
#'   an empty vector.
#' @return experiment object
#'
add_interval <- function(ex,
                         time = NULL,
                         time_start = NULL,
                         time_stop = NULL,
                         concentrations = numeric()) {
  idx <- length(intervals(ex)) + 1
  ivl <- new_interval(data_frame(ex),
                      time = time,
                      time_start = time_start,
                      time_stop = time_stop,
                      concentrations = concentrations)
  ex$ivls[[idx]] <- ivl
  ex
}

#' Remove an interval from an experiment
#'
#' @param ex experiment object
#' @param interval_index index of the interval to remove
#' @return experiment object
#'
delete_interval <- function(ex, interval_index) {
  ex$ivls[[interval_index]] <- NULL
  ex
}

# --- Getters for the experiment class ---

#' Get the data frame
#'
#' @param ex experiment object
#' @return attached data frame
#'
data_frame.experiment <- function(ex) {
  ex$df
}

#' Get the list of intervals
#'
#' @param ex experiment object
#' @return list of interval objects
#'
intervals <- function(ex) {
  ex$ivls
}
