# birdproofr (v1.0.0)
Bird Banding Data Validator
## About
birdproofr is a package of R tools for bird banding data validation under a set of rules written by Heidi Ware Carlisle, Intermountain Bird Observatory. The validator can be ran as a Shiny app for convenience, which includes utilities for viewing and downloading flagged data. Individual attributes can also be validated through function calls from the R console - please see IBO ruleset. The current birdproofr build has been updated for Fall 2018 banding. Support for a hummingbird ruleset is planned.
## Live App
[https://twotailstats.github.io/dev/birdproofr](https://twotailstats.github.io/dev/birdproofr)
## Usage / Dependencies (R Package)
Installing birdproofr requires a working installation of R (>= 3.5.0), which can be accessed through the command line or RStudio. To install from the console, ensure the [devtools](https://github.com/r-lib/devtools) package is present and loaded:
### Install and Load devtools
> install.packages("devtools")

> library(devtools)

Additionally, birdproofr depends on [shiny](https://github.com/rstudio/shiny) and [dplyr](https://github.com/tidyverse/dplyr). If these packages are not yet present on the machine, install them first.
### Install and Load shiny
> install.packages("shiny")

> library(shiny)
### Install and Load dplyr
> install.packages("dplyr")

> library(dplyr)
### Install and Load birdproofr
> devtools::install_github("twotailstats/birdproofr")

> library(birdproofr)

The birdproofr functions are now ready for use. For example, run the Shiny app:

> run_birdproofr_app()

## Functions
Please see the [birdproofr manual](https://github.com/twotailstats/birdproofr/manual.pdf) for more information.

## Changelog
### v1.0.0 (December 31, 2018)
- Convert 2017 Java build to R
- Launch R package on GitHub
- Launch live app on twotailstats site






