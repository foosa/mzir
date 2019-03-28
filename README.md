mzir
================

[![Build
Status](https://travis-ci.org/foosa/mzir.svg?branch=master)](https://travis-ci.org/foosa/mzir)

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
