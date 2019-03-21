context("Interval")
library(mzir)


test_that("Intervals hold references to datasets", {
  data <- read.csv("data/data.csv")
  intervals <- read.csv("data/intervals.csv")

  # Create a datset and intervals
  ds <- dataset(data)
  iv <- interval(data, intervals)

  expect_equal(class(ds), "dataset")
  expect_equal(class(iv), "interval")
  expect_equal(dataset(iv), ds)
  expect_equal(iv, interval(ds, intervals))
})

test_that("Intervals can be sliced by row", {
  data <- read.csv("data/data.csv")
  intervals <- read.csv("data/intervals.csv")

  # Create a datset and intervals
  ds <- dataset(data)
  iv <- interval(data, intervals)

  iv1 <- iv
})
