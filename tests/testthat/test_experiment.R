context("Experiment")
library(mzir)


test_that("Experiments hold references to data frames", {
	df <- read.csv("data/data.csv")
	ex <- new_experiment(df)

	expect_equal(class(ex), "experiment")
	expect_equal(data_frame(ex), df)
})

test_that("Experiments have a list of intervals", {
	df <- read.csv("data/data.csv")
	ivl_df <- read.csv("data/intervals.csv")
	ex <- new_experiment(df, ivl_df)

	expect_length(intervals(ex), 3)
	ivls <- intervals(ex)

	# Check the first interval
	ivl <- ivls[[1]]
	expect_equal(time_start(ivl), 26)
	expect_equal(time_stop(ivl), 130)
	expect_equal(analytes(ivl), c(tnt = 0, rdx = 0))

	# Check the second interval
	ivl <- ivls[[2]]
	expect_equal(time_start(ivl), 150)
	expect_equal(time_stop(ivl), 500)
	expect_equal(analytes(ivl), c(tnt = 20, rdx = 0))
})

test_that("Experiments can add and delete intervals", {
  df <- read.csv("data/data.csv")
  ex <- new_experiment(df)

  expect_length(intervals(ex), 0)
  ex <- add_interval(ex,
                     time_start = 30,
                     time_stop = 100,
                     concentrations = c(tnt = 20))
  expect_length(intervals(ex), 1)
  ivl <- intervals(ex)[[1]]
  expect_equal(time_start(ivl), 30)
  expect_equal(time_stop(ivl), 100)
  expect_equal(analytes(ivl), c(tnt = 20))
  ex <- delete_interval(ex, 1)
  expect_length(intervals(ex), 0)
})

test_that("Pipes work for experiment operations", {
  df <- read.csv("data/data.csv")
  ex <- new_experiment(df)

  ex <- ex %>%
    add_interval(time_start = 1, time_stop = 2) %>%
    add_interval(time_start = 2, time_stop = 3) %>%
    add_interval(time_start = 3, time_stop = 4) %>%
    add_interval(time_start = 4, time_stop = 5) %>%
    add_interval(time_start = 5, time_stop = 6)

  expect_length(intervals(ex), 5)
})
