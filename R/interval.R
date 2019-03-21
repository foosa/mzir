#' ---
#' @file: interval.R
#' @author: True Merrill
#' @date: March 17 2019
#' ---

# --- Constructors for the interval class ---

#' Create a new interval.
#'
#' @note an `interval` is essentially an S3 wrapper around a data frame.  The
#'   data frame is assumed to store time series data.  We will denote `time` as
#'   the column that stores time series data.  All other columns are assumed to
#'   store components of a time-domain signal.
#'
#' @param df data frame storing a time series
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
#'
#' @return interval object
#'
new_interval <- function(df,
						             time = NULL,
						             time_start = NULL,
						             time_stop = NULL,
						             concentrations = numeric()) {

  # Get the `time` column name as a string
  if (is.null(time)) {
		time_name <- colnames(df)[1]
	} else {
	  if (is.symbol(time)) {
	    time_name <- deparse(substitute(time))
	  } else {
	    time_name <- as.character(time)
	  }
	}

  time <- df[[time_name]]
  if (is.null(time_start)) {
    time_start <- min(time)
  }

  if (is.null(time_stop)) {
    time_stop <- max(time)
  }

  # Construct the object
  obj <- structure(
    list(
      df = df,
      time_name = time_name,
      time_start = time_start,
      time_stop = time_stop,
      analytes = NULL,
      fit = NULL
    ),
    class = 'interval'
  )

  analytes(obj) <- concentrations
  obj
}

#' Internal constructor method to create an interval object from a dataframe row
#'
#' @param df data frame storing the time series
#' @param ivl_df data frame storing the intervals
#' @param row_index which row of the data frame do we want to use
#' @param time column containing the independent degree of freedom.  If this is
#'   unspecified, the first column of `df` is assumed to be the independent
#'   degree of freedom.
#' @return new interval object
#'
new_interval_from_row <- function(df, ivl_df, row_index, time = NULL) {

  # Check for required columns
  row_df <- ivl_df[row_index, ]
  expected_columns <- c("time_start", "time_stop")
  for (colname in expected_columns) {
    if (!(colname %in% colnames(row_df))) {
      stop(paste("Missing required column: ", colname))
    }
  }
  t_start <- row_df[1, "time_start"]
  t_stop <- row_df[1, "time_stop"]

  # Vector of analytes
  analytes_data <- c()
  analytes_names <- c()
  for (colname in colnames(row_df)) {
    if (!(colname %in% expected_columns)) {
      analytes_data <- c(analytes_data, row_df[1, colname])
      analytes_names <- c(analytes_names, colname)
    }
  }
  analytes_vector <- structure(analytes_data, names = analytes_names)

  # Construct the new interval
  new_interval(df,
               time = time,
						   time_start = t_start,
						   time_stop = t_stop,
						   concentrations = analytes_vector)
}

# --- Getters for the interval class ---

data_frame <- function(x, ...) {
  UseMethod("data_frame", x)
}

#' Get the data frame
#'
#' @param ivl interval object
#' @return attached data frame
#'
data_frame.interval <- function(ivl) {
  ivl$df
}

#' Get the time vector
#'
#' @param ivl interval object
#' @return time vector
#'
time <- function(ivl) {
  data_frame(ivl)[[independent(ivl)]]
}

#' Get the start time
#'
#' @param ivl interval object
#' @return start time
#'
time_start <- function(ivl) {
  ivl$time_start
}

#' Get the stop time
#'
#' @param ivl interval object
#' @return stop time
#'
time_stop <- function(ivl) {
  ivl$time_stop
}

#' Get the vector of concentrations
#'
#' @param ivl interval object
#' @return named num vector of concentrations
#'
analytes <- function(ivl) {
  ivl$analytes
}

#' Get the independent variable
#'
#' @param ivl interval object
#' @param as_symbol logical controlling whether the independent variable should
#'   be returned as a symbol or a string
#' @return independent variable
#'
independent <- function(ivl, as_symbol = FALSE) {
  value <- NULL
  if (as_symbol) {
    value <- as.symbol(ivl$time_name)
  } else {
    value <- ivl$time_name
  }
  value
}

#' Get the vector of dependent variables
#'
#' @param ivl interval object
#' @param as_symbol logical controlling whether the dependent vairables should
#'   be returned as a vector of symbols or strings.
#' @param vector of dependent variable names
#'
dependent <- function(ivl, as_symbol = FALSE) {
  values <- c()
  for (col_name in colnames(data_frame(ivl))) {
    if (col_name != ivl$time_name) {
      if (as_symbol) {
        values <- c(values, as.symbol(col_name))
      } else {
        values <- c(values, col_name)
      }
    }
  }
  values
}

#' Returns the baseline, e.g., the values of the dependent variables at
#' `time_start`.
#'
#' @param ivl interval object
#' @return baseline vector
#'
baseline <- function(ivl) {
  values <- c()
  idx <- match(time_start(ivl), time(ivl))
  df <- data_frame(ivl)

  # Extract the initial values at the start time
  for (col_name in dependent(ivl)) {
    values <- c(values, df[[idx, col_name]])
  }
  values
}

#' Recenters the data around a set of means for each channel
#'
#' @param ivl interval object
#' @param means vector of means
#' @return ivl
#'
center <- function(ivl, means = baseline(ivl)) {
  col_names <- colnames(data_frame(ivl))
  for (idx in length(means)) {
    col_name <- col_names[idx]
    ivl$df[[idx, col_name]] <- ivl$df[[idx, col_name]] - means[idx]
  }
  ivl
}

#' Calculates the mean of the dependent variables over the interval
#'
#' @param ivl interval object
#' @return means
#'
mean.interval <- function(ivl) {
  mean(data_frame(ivl)[[dependent(ivl)]])
}

#' Summarizes the interval's data frame
#'
#' @param ivl interval object
#' @return summary
#'
summary.interval <- function(ivl) {
  summary(data_frame(ivl))
}

# --- Setters for the interval class ---

"time_start<-" <- function(ivl, value) {
  ivl$time_start <- value
  ivl
}

"time_stop<-" <- function(ivl, value) {
  ivl$time_stop <- value
  ivl
}

"analytes<-" <- function(ivl, value) {
  if (!is.vector(value, mode = "numeric")) {
    stop(paste("Not a numeric vector:", value))
  }

  if (length(value) > 0) {
    if (is.null(attr(value, "names"))) {
      stop(paste("Not a named vector:", value))
    }
  }

  ivl$analytes <- value
  ivl
}

# --- Fitting ---

#' #' Fit an interval to a non-linear model
#' #'
#' #' @note In the default implementation, we fit to the model
#' #'   \deqn{
#' #'     y(t) = (y_0 - y_1) exp(-t/T) + y_1
#' #'   }
#' #'
#' #'   This describes a simple exponential decay at a rate constant `T`.  Several
#' #'   observables in the MZI experiment that I wish to study can be modeled with
#' #'   exponential functions.  Please feel free to replace the model to whatever
#' #'   you wish ...
#' #'
#' #' @param ivl interval object to fit
#' #' @param mdl model object.  Usually created with `nls`.
#' #' @return ivl
#' #'
#' fit <- function(ivl,
#'                 mdl = nls(y ~ SSasymp(t, y1, y0, log_alpha),
#'                           data = data_frame(ivl))) {
#'   ivl$fit <- mdl
#'   ivl
#' }

# --- Plotting ---

#' Plot the interval
#'
#' @param ivl interval object to plot
#' @param dependents array column names representing the dependent variables in
#'   the data frame.  If this is unspecified, the default is to use
#'   `dependent(ivl)`.
#' @param mapping the ggplot2 aesthetic mapping to use for the plot.  If this is
#'   unspecified, we use `aes_string(x = independent(ivl))`.
#' @param line.colors a sequence of hex colour codes to be used to render each
#'   channel.  Once we run out of color codes, we loop around and start from the
#'   beginning.
#' @param guide.time_start logical controls whether the `time_start` guide line
#'   is drawn
#' @param guide.time_start.mapping aesthetic mapping to use for the `time_start`
#'   guide line
#' @param guide.time_start.padding how much to plot before `time_start`,
#'   measured in fractional units of `(time_stop - time_start)`.
#' @param guide.time_stop logical controls whether the `time_stop` guide line
#'   is drawn
#' @param guide.time_stop.mapping aesthetic mapping to use for the `time_stop`
#'   guide line
#' @param guide.time_stop.padding how much to plot after `time_stop`, measured
#'   in fractional units of `(time_stop - time_start)`.
#'
plot.interval <- function(ivl,
                          dependents = dependent(ivl),
                          mapping = NULL,
                          line.colours = c("#000000", "#E69F00", "#56B4E9",
                                           "#009E73", "#F0E442", "#0072B2",
                                           "#D55E00", "#CC79A7"),
                          guide.time_start = TRUE,
                          guide.time_start.mapping = NULL,
                          guide.time_start.padding = 0,
                          guide.time_stop = TRUE,
                          guide.time_stop.mapping = NULL,
                          guide.time_stop.padding = 0) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes_string(x = independent(ivl))
  }

  # Create plot object
  plt <- ggplot2::ggplot(data_frame(ivl), mapping)
  idx <- 0
  for (col_name in dependents) {
    idx <- (idx + 1) %% length(line.colours)
    col_color <- line.colours[idx]
    col_mapping <- ggplot2::aes_string(y = col_name)
    plt <- plt +
      ggplot2::geom_line(col_mapping, colour = col_color)
  }

  if (guide.time_start) {
    if (is.null(guide.time_start.mapping)) {
      guide.time_start.mapping <- ggplot2::aes(xintercept = time_start(ivl),
                                               colour = "#E69F00")
    }
    plt <- plt +
      ggplot2::geom_vline(guide.time_start.mapping, linetype = "dashed")
  }

  if (guide.time_stop) {
    if (is.null(guide.time_stop.mapping)) {
      guide.time_stop.mapping <- ggplot2::aes(xintercept = time_stop(ivl),
                                              colour = "#E69F00")
    }
    plt <- plt +
      ggplot2::geom_vline(guide.time_stop.mapping, linetype = "dashed")
  }

  dt <- time_stop(ivl) - time_start(ivl)
  t1 <- time_start(ivl) - guide.time_start.padding * dt
  t2 <- time_stop(ivl) + guide.time_stop.padding * dt

  plt <- plt +
    ggplot2::coord_cartesian(xlim = c(t1, t2)) +
    ggplot2::xlab("Time (sec)") +
    ggplot2::ylab("Phase (rad)")
  plt
}
