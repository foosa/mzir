context("Interval")
library(mzir)


test_that("Intervals hold references to dataset", {
  df <- read.csv("data/data.csv")
  ivl <- new_interval(df)

  expect_equal(class(ivl), "interval")
  expect_equal(data_frame(ivl), df)
})

test_that("Intervals have getter methods", {
	df <- read.csv("data/data.csv")
	ivl <- new_interval(df)

	expect_equal(time_start(ivl), 0)
	expect_equal(time_stop(ivl), 1000)
	expect_equal(time(ivl), time_start(ivl):time_stop(ivl))
	expect_equal(length(analytes(ivl)), 0)
	expect_equal(independent(ivl), c("time"))
	expect_equal(dependent(ivl), c("ch0", "ch1", "ch2", "ch3"))

	bline <- c(ch0 = 0.0139209546650463,
	           ch1 = 0.0510858040855859,
						 ch2 = 0.0751889785766925,
						 ch3 = 0.0811048669258656)
	expect_equal(baseline(ivl), bline)
})

test_that("Intervals have setter methods", {
	df <- read.csv("data/data.csv")
	ivl <- new_interval(df)

	expect_equal(time_start(ivl), 0)
	time_start(ivl) <- 1
	expect_equal(time_start(ivl), 1)
	expect_equal(time_stop(ivl), 1000)
	time_stop(ivl) <- 900
	expect_equal(time_stop(ivl), 900)

	# analytes
	expect_equal(analytes(ivl), numeric())
	analytes(ivl) <- c(tnt = 0, rdx = 10)
	expect_equal(analytes(ivl), c(tnt = 0, rdx = 10))
})

test_that("Intervals can be fitted", {
  df <- read.csv("data/data.csv")
  ivl <- new_interval(df)
  time_start(ivl) <- 26
  time_stop(ivl) <- 130

  expect_false(is_fit(ivl))
  ivl <- fit(ivl)
  expect_true(is_fit(ivl))

  plt <- plot(ivl)
  ggplot2::ggsave("test_interval_fitted.png",
                  plot = plt,
                  width = 4,
                  height = 3,
                  units = "in")

  print(coef(ivl))
})

test_that("Intervals can be centered", {
	df <- read.csv("data/data.csv")
	ivl <- new_interval(df)
	bline <- baseline(ivl)

	ivl_centered <- center(ivl)
	ivl_centered_df <- data_frame(ivl_centered)

	for (colname in dependent(ivl)) {
		expect_equal(ivl_centered_df[ , colname],
								 df[ , colname] - bline[colname])
	}
})

test_that("Intervals can be plotted", {
	df <- read.csv("data/data.csv")
	ivl <- new_interval(df)

	plt <- plot(ivl)
	expect_equal(class(plt), class(ggplot2::ggplot()))
	ggplot2::ggsave("test_interval.png",
									plot = plt,
									width = 4,
									height = 3,
									units = "in")
})

