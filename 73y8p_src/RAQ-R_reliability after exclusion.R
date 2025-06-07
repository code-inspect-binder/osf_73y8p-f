#-----------------------------------------------------------------------------------------------------------------------------
# Evaluating reliability of the RAQ-R in the PG without patients with psychotic disorders or bipolar affective disorder
#-----------------------------------------------------------------------------------------------------------------------------
library(psy)
library(dplyr)
library(semTools)

load("RAQ_df_complete.rda")
subsample_PG <- RAQ_df_complete[RAQ_df_complete$Sample == 1, ]

# subsample of the PG without patients with schizophrenia or other psychotic disorders (F20.- or F23.-)
subsample_PGwithoutF20_23 <- subsample_PG[subsample_PG$F20_23==0, ]

# cronbach's alpha can't be calculated with a mids. We calculate the coefficients for each iteration and check for differences
RAQ_PGwithoutF20_23_1 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$.imp == 1, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_2 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$.imp == 2, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_3 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$.imp == 3, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_4 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$.imp == 4, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_5 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$.imp == 5, paste0("RAQ_", 1:22)]

# cronbach's alpha
alpha_1_withoutF20_23 <- cronbach(RAQ_PGwithoutF20_23_1)
alpha_2_withoutF20_23 <- cronbach(RAQ_PGwithoutF20_23_2)
alpha_3_withoutF20_23 <- cronbach(RAQ_PGwithoutF20_23_3)
alpha_4_withoutF20_23 <- cronbach(RAQ_PGwithoutF20_23_4)
alpha_5_withoutF20_23 <- cronbach(RAQ_PGwithoutF20_23_5)

## additionally, we check cronbach's alpha after excluding patients with F20, F23 and F31 (bipolar affective disorder)

subsample_PGwithoutF20_23_31 <- subsample_PGwithoutF20_23[subsample_PGwithoutF20_23$F31==0, ]

RAQ_PGwithoutF20_23_31_1 <- subsample_PGwithoutF20_23_31[subsample_PGwithoutF20_23_31$.imp == 1, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_31_2 <- subsample_PGwithoutF20_23_31[subsample_PGwithoutF20_23_31$.imp == 2, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_31_3 <- subsample_PGwithoutF20_23_31[subsample_PGwithoutF20_23_31$.imp == 3, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_31_4 <- subsample_PGwithoutF20_23_31[subsample_PGwithoutF20_23_31$.imp == 4, paste0("RAQ_", 1:22)]
RAQ_PGwithoutF20_23_31_5 <- subsample_PGwithoutF20_23_31[subsample_PGwithoutF20_23_31$.imp == 5, paste0("RAQ_", 1:22)]

# cronbach's alpha
alpha_1_withoutF20_23_31 <- cronbach(RAQ_PGwithoutF20_23_31_1)
alpha_2_withoutF20_23_31 <- cronbach(RAQ_PGwithoutF20_23_31_2)
alpha_3_withoutF20_23_31 <- cronbach(RAQ_PGwithoutF20_23_31_3)
alpha_4_withoutF20_23_31 <- cronbach(RAQ_PGwithoutF20_23_31_4)
alpha_5_withoutF20_23_31 <- cronbach(RAQ_PGwithoutF20_23_31_5)
