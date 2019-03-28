#' ---
#' @file interact.R
#' @author True Merrill
#' @date March 19 2019
#' ---

#' Interactive prompt to load an experiment
#'
#' @param df data frame storing experimental time series.  If this is null, the
#'   user will be presented with a file selection dialogue where they can choose
#'   a CSV file.
#' @param ivl_df data from storing the intervals.  If this is null, the user
#'   will be presented with a prompt asking if they want to manually specify the
#'   intervals or to load the intervals from a CSV file.
#' @return experiment object
#'
interact_experiment <- function(df = NULL, ivl_df = NULL) {
  if (is.null(df)) {
    cat("Select a experimental data file:")
    filename <- file.choose()
    df <- csv.read(filename)
  }

  if (is.null(ivl_df)) {
    cat("The experimental intervals are currently unspecified:")
    selection <- menu(c("Manually create intervals", "Load intervals file"))
    if (selection == 1) {
      # Interactive loop to create new intervals
      ex <- new_experiment(df)
      # TODO: implement interactive loop to create new intervals
    } else if (selection == 2) {
      # Load intervals from file
      filename <- file.choose()
      ivl_df <- csv.read(filename)
      ex <- new_experiment(df, ivl_df)
    }
  } else {
    # User passed the interval data frame
    ex <- new_experiment(df, ivl_df)
  }

  ex
}

#' Interactive prompt to set time_start and time_stop on an interval
#'
#' @param ivl interval object
#' @param which which graphics device to display plots on
#' @param nudge_size size of "nudge" operations
#' @return new interval object
#'
interact_interval_times <- function(ivl, which = dev.cur(), nudge_size = 1.0) {
  options <- c("Return", "Adjust start time", "Adjust stop time")
  adjust_options <- c("Return", "Set new value", "Nudge left",
                      "Nudge right", "Set nudge size")
  selection <- menu(options)

  if (selection == 1) {
    return (ivl)
  }

  # Adjust the start time
  else if (selection == 2) {
    dev.set(which = which)
    repeat {
      print(plot(ivl))
      selection <- menu(adjust_options)
      if (selection == 1) {
        return (ivl)
      } else if (selection == 2) {
        time_start(ivl) <- as.numeric(readline("Enter new start time: "))
      } else if (selection == 3) {
        time_start(ivl) <- time_start(ivl) - nudge_size
      } else if (selection == 4) {
        time_start(ivl) <- time_start(ivl) + nudge_size
      } else if (selection == 5) {
        nudge_size <- as.numeric(readline("Enter new nudge size: "))
      }
    }
  }

  # Adjust the stop time
  else if (selection == 3) {
    dev.set(which = which)
    repeat {
      print(plot(ivl))
      selection <- menu(adjust_options)
      if (selection == 1) {
        return (ivl)
      } else if (selection == 2) {
        time_stop(ivl) <- as.numeric(readline("Enter new stop time: "))
      } else if (selection == 3) {
        time_stop(ivl) <- time_stop(ivl) - nudge_size
      } else if (selection == 4) {
        time_stop(ivl) <- time_stop(ivl) + nudge_size
      } else if (selection == 5) {
        nudge_size <- as.numeric(readline("Enter new nudge size: "))
      }
    }
  }

  ivl
}


