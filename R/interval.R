#' ---
#' @file interval.R
#' @author True Merrill
#' @date March 17 2019
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
#' @export
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

#' @export
data_frame <- function(x, ...) {
  UseMethod("data_frame", x)
}

#' Get the data frame
#'
#' @param ivl interval object
#' @return attached data frame
#' @export
data_frame.interval <- function(ivl) {
  ivl$df
}

#' Get the time vector
#'
#' @param ivl interval object
#' @return time vector
#' @export
time <- function(ivl) {
  data_frame(ivl)[[independent(ivl)]]
}

#' Get the start time
#'
#' @param ivl interval object
#' @return start time
#' @export
time_start <- function(ivl) {
  ivl$time_start
}

#' Get the stop time
#'
#' @param ivl interval object
#' @return stop time
#' @export
time_stop <- function(ivl) {
  ivl$time_stop
}

#' Get the vector of concentrations
#'
#' @param ivl interval object
#' @return named num vector of concentrations
#' @export
analytes <- function(ivl) {
  ivl$analytes
}

#' Get the independent variable
#'
#' @param ivl interval object
#' @param as_symbol logical controlling whether the independent variable should
#'   be returned as a symbol or a string
#' @return independent variable
#' @export
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
#' @return vector of dependent variable names
#' @export
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
#' @return baseline named vector
#' @export
baseline <- function(ivl) {
  idx <- match(time_start(ivl), time(ivl))
  df <- data_frame(ivl)

  # Extract the initial values at the start time
  values <- c()
	for (col_name in dependent(ivl)) {
    values <- c(values, df[[idx, col_name]])
  }

	# Create a named vector
  values <- structure(values, names = dependent(ivl))
	values
}

#' Recenters the data around a set of means for each channel
#'
#' @param ivl interval object
#' @param means named vector of means
#' @return interval object
#' @export
center <- function(ivl, means = baseline(ivl)) {
  df <- data_frame(ivl)
	for (colname in names(means)) {
		ivl$df[ , colname] <- ivl$df[ , colname] - means[colname]
	}
  ivl
}

#' Calculates the mean of the dependent variables over the interval
#'
#' @param ivl interval object
#' @return means
#' @export
mean.interval <- function(ivl) {
  mean(data_frame(ivl)[[dependent(ivl)]])
}

#' Summarizes the interval's data frame
#'
#' @param ivl interval object
#' @return summary
#' @export
summary.interval <- function(ivl) {
  smry <- coef(ivl) %>%
    dplyr::mutate(time_start = time_start(ivl)) %>%
    dplyr::mutate(time_stop = time_stop(ivl)) %>%
    dplyr::select(time_start, time_stop, dplyr::everything())
  smry
}

# Generic
#' @export
chop <- function(x, ...) {
  UseMethod("chop", x)
}

#' @export
chop.data.frame <- function(df,
                            time = df[ , 1],
                            time_start = time[1],
                            time_stop = time[length(time)]) {
  mask <- ((time >= time_start) & (time <= time_stop))
  df[mask, ]
}

#' Chop the interval's data frame to include only data between `time_start` and
#' `time_stop`
#'
#' @param ivl interval object
#' @param time
#' @param time_start
#' @param time_stop
#' @return interval object
#' @export
chop.interval <- function(ivl,
                          time = NULL,
                          time_start = NULL,
                          time_stop = NULL) {
  if (is.null(time)) time <- time(ivl)
  if (is.null(time_start)) time_start <- time_start(ivl)
  if (is.null(time_stop)) time_stop <- time_stop(ivl)
  mask <- ((time >= time_start) & (time <= time_stop))
  ivl$df <- data_frame(ivl)[mask, ]
  ivl
}

# --- Setters for the interval class ---

#' @export
"time_start<-" <- function(ivl, value) {
  ivl$time_start <- value
  ivl
}

#' @export
"time_stop<-" <- function(ivl, value) {
  ivl$time_stop <- value
  ivl
}

#' @export
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

#' Fit an interval to a non-linear model
#'
#' @note In the default implementation, we fit to the model
#'   \deqn{
#'     y(t) = (y_0 - y_1) exp(-t/T) + y_1
#'   }
#'
#'   This describes a simple exponential decay at a rate constant `T`.  Several
#'   observables in the MZI experiment that I wish to study can be modeled with
#'   exponential functions.  Please feel free to replace the model to whatever
#'   you wish ...
#'
#' @param ivl interval object to fit
#' @return interval object
#' @export
fit <- function(ivl) {
  models <- list()
  chopped_ivl <- chop(ivl)
  for (colname in dependent(ivl)) {
    t <- time(chopped_ivl)
    y <- chopped_ivl$df[ , colname]
    mdl <- NULL
    result <- tryCatch({
      mdl <- nls(y ~ SSasymp(t, y1, y0, log_alpha))
    }, error = function(e) {
      warning(e)
    })

    models[[colname]] <- mdl
  }

  ivl$fit <- models
  ivl
}

#' @export
is_fit <- function(ivl) {
  !(is.null(ivl$fit))
}

#' @export
coef.interval <- function(ivl, dependents = dependent(ivl)) {
  if (!(is_fit(ivl))) {
    ivl <- fit(ivl)
  }

  coeffs <- data.frame()
  for (colname in dependents) {
    if (!(is.null(ivl$fit[[colname]]))) {
      col_summary <- summary(ivl$fit[[colname]])
      col_coeffs <- as.data.frame(col_summary$coefficients)

      tmp <- data.frame(param = rownames(col_coeffs),
                        dependent = colname)
      rownames(col_coeffs) <- NULL
      tmp <- cbind(tmp, col_coeffs)
      coeffs <- rbind(coeffs, tmp)
    }
  }

  coeffs
}

# --- Plotting ---


# StatInterval <- ggplot2::ggproto("StatInterval", Stat,
#   compute_group = function(data, scales) {
#     chopped_data <- chop(data)
#     x <- chopped_data$x
#     y <- chopped_data$y
#     result <- tryCatch({
#       mdl <- nls(y ~ SSasymp(t, y1, y0, log_alpha))
#     }, error = function(e) {
#       warning(e)
#       mdl <- NULL
#     })
#   }
# )



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
#' @return plot object
#' @export
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

  # Chop
  chopped_ivl <- chop(ivl)

  # Create plot object
  plt <- ggplot2::ggplot(data_frame(ivl), mapping)
  idx <- 0
  for (col_name in dependents) {
    idx <- (idx + 1) %% length(line.colours)
    col_color <- line.colours[idx]
    col_mapping <- ggplot2::aes_string(y = col_name)
    plt <- plt +
      ggplot2::geom_line(col_mapping, colour = col_color, alpha = 0.5)

    if (is_fit(ivl)) {
       mdl <- ivl$fit[[col_name]]
       if (!(is.null(mdl))) {
         tmp <- data.frame(
           t = time(chopped_ivl),
           y = predict(mdl)
         )
         plt <- plt +
           ggplot2::geom_line(
             mapping = ggplot2::aes(x = t, y = y),
             data = tmp,
             show.legend = FALSE,
             colour = col_color,
             size = 1
           )
       }
    }
  }

  if (guide.time_start) {
    if (is.null(guide.time_start.mapping)) {
      guide.time_start.mapping <- ggplot2::aes(xintercept = time_start(ivl))
    }
    plt <- plt +
      ggplot2::geom_vline(guide.time_start.mapping,
                          linetype = "dotted",
                          colour = "black",
                          alpha = 0.5)
  }

  if (guide.time_stop) {
    if (is.null(guide.time_stop.mapping)) {
      guide.time_stop.mapping <- ggplot2::aes(xintercept = time_stop(ivl))
    }
    plt <- plt +
      ggplot2::geom_vline(guide.time_stop.mapping,
                          linetype = "dotted",
                          colour = "black",
                          alpha = 0.5)
  }

  dt <- time_stop(ivl) - time_start(ivl)
  t1 <- time_start(ivl) - guide.time_start.padding * dt
  t2 <- time_stop(ivl) + guide.time_stop.padding * dt
  y1 <- data_frame(chopped_ivl) %>%
    dplyr::select(dependents) %>% min
  y2 <- data_frame(chopped_ivl) %>%
    dplyr::select(dependents) %>% max

  plt <- plt +
    ggplot2::coord_cartesian(xlim = c(t1, t2),
                             ylim = c(y1, y2)) +
    ggplot2::xlab("Time (sec)") +
    ggplot2::ylab("Phase (rad)") +
    ggplot2::theme_classic()
  plt
}
