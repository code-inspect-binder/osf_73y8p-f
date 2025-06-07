## -----------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------
# title of the article: Validation of the Rage Attack Questionnaire-Revised (RAQ-R) in a mixed psychiatric population
# authors of the r scrpit: Lisa Palm, Alexander Braumann
# date: 24 Apr 2021
## -----------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------


# note 1: This scrpit shows only the analysis of missing values and the multiple imputation (in order to handle missing values)
# note 2: The mids generated via multiple imputation can include different imputated data each time it is run.
#         These different imputated data can result in slighty different results of the analyses.

### Set-up R session

# Download the file *RAQ_df_raw.csv* in one directory and set this directory as working directory in [R]

# *RAQ_df_raw.csv* - This file contains the questionnare data from all three samples (PG, CG, TS), n(PG)=156, n(CG)=611, n(TS)=127. 
# note 1: Inverted Variables in BIS-15: 1,4,5,7,8,15
# note 2: -1 = no information

## load data
RAQ_df_raw <- read.csv("RAQ_df_raw.csv")

## load and install the folllowing R package
install.package("mice")
library("mice")

# look at the data
colnames(RAQ_df_raw)
str(RAQ_df_raw)

# pattern of missing values
md.pattern(RAQ_df_raw)


### multiple imputation with 5 iterations resulting in a mids
RAQ_mids <- mice(RAQ_df_raw, seed=1)

