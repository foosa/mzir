mzir
================

## Overview

`mzir` is an R package for plotting datasets from the Mach-Zehnder
interferometric chemical sensor project. Datasets are structured file
folders in the following format:

    dataset/            # Folder name is unimportant
      meta.json         # Stores metadata (chip ID, film ID, notes, etc.)
      intervals.csv     # Stores intervals data (time vs concentration)
      data.csv          # Raw interferometer output (time vs phase shift)

Data analysis consists of the following steps:

1.  Load and validate the dataset
2.  Adjust the intervals
3.  Align the intervals
4.  Fit the data within each interval to a model
5.  Sensitivity analysis
6.  Save processed results and plots

## Usage

`mzir` can be run in an interactive mode where it walks the user through
the data analysis process, or as a regular R library.

## Tickets

  - \[ \] create an `interval` class.
  - \[ \] create an `intervals` class. This is a data frame that
    supports `nextElem.intervals()`.
  - \[ \] create a `dataset` class. The dataset should have
    `data(dataset)` and `intervals(dataset)`.
  - \[ \] create `plot_interval(interval)`.
  - \[ \] create live-adjustable plots.
  - \[ \] create `fit_interval(interval)`

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Readme_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
