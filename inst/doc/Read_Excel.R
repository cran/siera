## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----library, warning=FALSE---------------------------------------------------
library(siera)

## ----cards example, eval=FALSE------------------------------------------------
# # example of 'cards' code in AnalysisMethodCodeTemplate, using the ard_continuous function:
# 
# Analysis_ARD = ard_continuous(
#   data = filtered_ADSL,
#   by = c(byvariables_here),
#   variables = analysisvariable_here
# )

## ----cardsconstructs, eval=FALSE----------------------------------------------
# ARS_example("cards_constructs.xlsx")

## ----Excel ARS metadata example, message=FALSE--------------------------------

# Path to the Excel ARS metadata file: 
ARS_path <- ARS_example("Common_Safety_Displays_cards.xlsx")

# Path to a folder which will contain the Output meta-programmed R scripts (recommended to update
# to a more suitable local path)
output_folder <- tempdir()

# Path to the folder containing ADaM datasets in csv format (we will use temporary 
# directory tempdir() to make the code run in this vignette, but it's recommended to 
# 1. download the ADaMs required (csv ADSL and ADAE available using e.g. ARS_example("ADSL.csv"))
# 2. store it in a folder (the directly downloaded location can be found using dirname(ARS_example("ADSL.csv")), for example.  Use this location, or manually store the ADaM somewhere else)
# 3. use the folder path instead of tempdir() below.
ADaM_folder <- tempdir()

# run the readARS function with these 3 parameters.  This creates R scripts 
# (1 for each Output in output_folder)
readARS_xl(ARS_path, output_folder, ADaM_folder)

## ----install show-------------------------------------------------------------


## ----run ARD_xxx, message=FALSE, warning=FALSE, eval=FALSE--------------------
# 
# # Step 1: open ARD_xxx.R file
# # Step 2: Confirm the location of ADaM dataset(s) is correct in the code section "Load ADaM".
# # For the sake of simplicity, the only update made to the ARD_Out14-1-1.R
# # script was to point to the ADaM folder to ARS_example("ADSL.csv")
# # Step 3: Run the code and enjoy automated analysis results generation.
# # Note: the ARD is contained in the object "df4" (which contains the appended)
# # mini-ARDs from all individual Analyses ARDs.
# 
# example_ARD_script = ARD_script_example("ARD_Out14-1-1.R")
# source(example_ARD_script)
# head(df4)

