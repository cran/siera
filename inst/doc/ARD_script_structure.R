## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(siera)

## ----high-level program overview----------------------------------------------
# Section 1: Program header

# Section 2: Load libraries

# Section 3: Load ADaM datasets

# Section 4a (first Analysis): Code to calculate results as an ARD

# Section 4b (subsequent Analyses): Code to calculate results as an ARD

# Section 5: Append Analysis-level ARDs

## ----Analysis Sets, message=FALSE, eval=FALSE---------------------------------
# overlap <- intersect(names(ADSL), names(ADAE))
# overlapfin <- setdiff(overlap, 'USUBJID')
# 
# df_pop <- dplyr::filter(ADSL,
#             SAFFL == 'Y') |>
#             merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),
#                   by = 'USUBJID',
#                   all = FALSE)
# 
# df_poptot = dplyr::filter(ADSL,
#             SAFFL == 'Y')

## ----Data Subsets, message=FALSE, eval=FALSE----------------------------------
# df2_An07_03_SerTEAE_Summ_ByTrt <- df_pop |>
#         dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')

## ----MethodExample, eval=FALSE------------------------------------------------
# 
# # intermediate step: Prepare Denominator Dataset for `cards` function
# denom_dataset = df2_An01_05_SAF_Summ_ByTrt |>
#   dplyr::select(TRT01A)
# 
# # intermediate step: Prepare input dataset for `cards` function
# in_data = df2_An03_05_Race_Summ_ByTrt |>
#   dplyr::distinct(TRT01A, RACE, USUBJID) |>
#   dplyr::mutate(dummy = 'dummyvar')
# 
# # pass calculate subjects counts and % (based on big N) grouped by treatment and race
# cards::ard_categorical(
#   data = in_data,
#   by = c('TRT01A', 'RACE'),
#   variables = 'dummy',
#   denominator = denom_dataset)
# 
# # select relevant statistics as defined by the Method, and assign operation Ids
# df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt|>
#   dplyr::filter(stat_name %in% c('n', 'p')) |>
#   dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'Mth01_1_n',
#                                                stat_name == 'p' ~ 'Mth01_2_pct'))
# 
# # add ARS metadata IDs to the dataset to enable tracing each result back to ARS metadata
# df3_An03_05_Race_Summ_ByTrt <- df3_An03_05_Race_Summ_ByTrt |>
#   dplyr::mutate(AnalysisId = 'An03_05_Race_Summ_ByTrt',
#                 MethodId = 'Mth01',
#                 OutputId = 'Out14-1-1')

## ----append, message=FALSE, eval=FALSE----------------------------------------
# # combine analyses to create ARD ----
# ARD <- dplyr::bind_rows(df3_An01_05_SAF_Summ_ByTrt,
# df3_An03_01_Age_Summ_ByTrt,
# df3_An03_01_Age_Comp_ByTrt,
# df3_An03_02_AgeGrp_Summ_ByTrt,
# df3_An03_02_AgeGrp_Comp_ByTrt,
# df3_An03_03_Sex_Summ_ByTrt,
# df3_An03_03_Sex_Comp_ByTrt,
# df3_An03_04_Ethnic_Summ_ByTrt,
# df3_An03_04_Ethnic_Comp_ByTrt,
# df3_An03_05_Race_Summ_ByTrt,
# df3_An03_05_Race_Comp_ByTrt,
# df3_An03_06_Height_Summ_ByTrt,
# df3_An03_06_Height_Comp_ByTrt)

## ----example ARD script, message=FALSE, warning=FALSE, eval=FALSE-------------
# # see location of script:
# ARD_script_example("ARD_Out14-1-1.R")
# ARD_script_example("ARD_Out14-3-1-1.R")

## ----open ARD script, message=FALSE, warning=FALSE, eval=FALSE----------------
# # open script to inspect:
# file.edit(ARD_script_example("ARD_Out14-1-1.R"))
# file.edit(ARD_script_example("ARD_Out14-3-1-1.R"))

## ----run ARD script, message=FALSE, warning=FALSE, eval=FALSE-----------------
# # run script locally:
# source(ARD_script_example("ARD_Out14-1-1.R"))
# source(ARD_script_example("ARD_Out14-3-1-1.R"))

