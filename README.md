
<!-- README.md is generated from README.Rmd. Please edit that file -->

# siera

<!-- badges: start -->
<!-- [![CRAN -->
<!-- status](https://www.r-pkg.org/badges/version/siera)](https://CRAN.R-project.org/package=siera) -->

[![R-CMD-check](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clymbclinical/siera/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Are you looking for a way to automate TFLs?

With siera, users ingest Analysis Results Standard - ARS (a CDISC
Foundational standard) metadata and auto-generate R scripts that, when
run in with provided ADaM datasets, provide Analysis Results Datasets
(ARDs).

In order to use the readARS() function, users will need to provide the
following:

1.  A Functional JSON file, representing ARS Metadata for a Reporting
    Event (to get started, see TFL Designer)
2.  An output directory where the R scripts will be placed
3.  A folder containing the related ADaM datasets for the ARDs to be
    generated

## Installation

The current version (0.1.0) of siera can be installed from
[GitHub](https://github.com/clymbclinical/siera) with:

``` r
# install.packages("devtools")
Sys.unsetenv("GITHUB_PAT")
devtools::install_github("https://github.com/clymbclinical/siera")
```

## Example

``` r
library(siera)
```

siera includes several example files, which we use throughout the
documentation. These include a JSON ARS file, as well as some csv ADaMs
(ADSL and ADAE) which can be run with the R scripts produced by readARS
function. Use the helper ARS_example() with no arguments to list them or
call it with an example filename to get the path.

``` r
ARS_example()
#> [1] "ADAE.csv"                           "ADSL.csv"                          
#> [3] "ARS_V1_Common_Safety_Displays.json"

ARS_example("ARS_V1_Common_Safety_Displays.json")
#> [1] "C:/Users/mbosm/AppData/Local/R/win-library/4.4/siera/extdata/ARS_V1_Common_Safety_Displays.json"
```

Using an example JSON ARS file, run readARS() function to produce the
ARD programs.

In this example, the following is used: 1. JSON ARS file from CDISC
Pilot Study

``` r
# the ARS JSON File. 
json_path <- ARS_example("ARS_V1_Common_Safety_Displays.json")

# store ARD scripts in this folder (we will use a temporary directory, 
# but you may set to desired path):
output_folder <- tempdir()

# this folder contains ADaM datasets to produce ARD (we will use temporary 
# directory, but to make it workable, download the ADaMs required and use the location 
# they are stored in):
ADaM_folder <- tempdir()

# run the readARS function with these 3 parameters.  This creates R scripts (1 for each output in output_folder)
readARS(json_path, output_folder, adam_path = ADaM_folder)
```

Once the R programs are created, they can be individually run, provided
that the ADaM datasets are in the location as provided to the readARS
function.
