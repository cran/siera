## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(siera)

## ----cards example, eval=FALSE------------------------------------------------
# # example of 'cards' code in AnalysisMethodCodeTemplate, using the ard_continuous function:
# 
# Analysis_ARD = ard_continuous(
#   data = filtered_ADSL,
#   by = c(byvariables_here),
#   variables = analysisvariable_here
# )

## ----cards example output, eval=FALSE-----------------------------------------
# # example of 'cards' code in AnalysisMethodCodeTemplate, populated with AnalysisMethodCodeParameters:
# 
# Analysis_ARD = ard_continuous(
#   data = filtered_ADSL,
#   by = c(TRT01A),
#   variables = AGE
# )

## ----cardsconstructs, eval=FALSE----------------------------------------------
# ARS_example("cards_constructs.xlsx")

