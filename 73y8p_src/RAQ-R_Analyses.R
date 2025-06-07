## -----------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------
# title of the article: Validation of the Rage Attack Questionnaire-Revised (RAQ-R) in a mixed psychiatric population
# authors of the r scrpit: Lisa Palm, Alexander Braumann
# date: 24 Apr 2021
## -----------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------

# note: the analysis of missng values and the multiple imputation are shown in the R script *RAQ-R_multiple imputation*


### Set-up R session

# Download the file *RAQ_mids.rda* in one directory and set this directory as working directory in [R]: 

# *RAQ_mids.rda* - This file contains the multiple imputated questionnare data from all three samples (PG, CG, TS) with five iterations
# note 1: Inverted Variables in BIS-15: 1,4,5,7,8,15
# note 2: -1 = no information

## load data
load("RAQ_mids.rda")

## load the folllowing R packages
install.packages("mice",
                 "psy", 
                 "dplyr", 
                 "semTools",
                 "haven",
                 "plyr",
                 "ggplot2",
                 "plm")

#-------------------------------------------------------------------------------------------------------------------------------
# developing a data frame and a mids with all questionnaire scores and multiple imputated data
#-------------------------------------------------------------------------------------------------------------------------------

### converting the mids in a complete data frame including all five iterations. We do this for the calculation of the questionnaire scores which is not possible in a mids
RAQ_df_complete <- complete(imp_RAQ, action="long", include = TRUE)

# look at the data
colnames(RAQ_df_complete)
str(RAQ_df_complete)
# note: since the file contains the data from all five imputations the number of each sample is multipled by five. The column *.imp* shows the iteration number of the data.

### calculating and pasting scores into the complete data frame

# RAQ-R totalscore (calculated as sum)
RAQ_Totalscore_col <- RAQ_df_complete[, paste0("RAQ_", 1:22)]
RAQ_df_complete$RAQ_Totalscore <- apply(RAQ_Totalscore_col, 1, sum)

# BSI scores (calculated as means)
BSI_GSI_col <- RAQ_df_complete[, paste0("BSI_", 1:53)]
RAQ_df_complete$BSI_GSI <- apply(BSI_GSI_col, 1, mean)
BSI_Somatization_col <- RAQ_df_complete[, paste0("BSI_", c(2, 7, 23, 29, 30, 33, 37))]
RAQ_df_complete$BSI_Somatization <- apply(BSI_Somatization_col, 1, mean)
BSI_Obsessive_Compulsion_col <- RAQ_df_complete[, paste0("BSI_", c(5, 15, 26, 27, 32, 36))]
RAQ_df_complete$BSI_Obsessive_Compulsion <- apply(BSI_Obsessive_Compulsion_col, 1, mean)
BSI_Interpersonal_Sensitivity_col <- RAQ_df_complete[, paste0("BSI_", c(20, 21, 22, 42))]
RAQ_df_complete$BSI_Interpersonal_Sensitivity <- apply(BSI_Interpersonal_Sensitivity_col, 1, mean)
BSI_Depression_col <- RAQ_df_complete[, paste0("BSI_", c(9, 16, 17, 18, 35, 50))]
RAQ_df_complete$BSI_Depression <- apply(BSI_Depression_col, 1, mean)
BSI_Anxiety_col <- RAQ_df_complete[, paste0("BSI_", c(1, 12, 19, 38, 45, 49))]
RAQ_df_complete$BSI_Anxiety <- apply(BSI_Anxiety_col, 1, mean)
BSI_Aggression_Hostility_col <- RAQ_df_complete[, paste0("BSI_", c(6, 13, 40, 41, 46))]
RAQ_df_complete$BSI_Aggression_Hostility <- apply(BSI_Aggression_Hostility_col, 1, mean)
BSI_Phobic_Anxiety_col <- RAQ_df_complete[, paste0("BSI_", c(8, 28, 31, 43, 47))]
RAQ_df_complete$BSI_Phobic_Anxiety <- apply(BSI_Phobic_Anxiety_col, 1, mean)
BSI_Paranoid_Ideation_col <- RAQ_df_complete[, paste0("BSI_", c(4, 10, 24, 48, 51))]
RAQ_df_complete$BSI_Paranoid_Ideation <- apply(BSI_Paranoid_Ideation_col, 1, mean)
BSI_Psychoticism_col <- RAQ_df_complete[, paste0("BSI_", c(3, 14, 34, 44, 53))]
RAQ_df_complete$BSI_Psychoticism <- apply(BSI_Psychoticism_col, 1, mean)

# BIS-15 scores (calculated as means)
BIS15_Totalscore_col <- RAQ_df_complete[, paste0("BIS15_", 1:15)]
RAQ_df_complete$BIS_15_Totalscore <- apply(BIS15_Totalscore_col, 1, mean)
BIS15_Non_Planning_col <- RAQ_df_complete[, paste0("BIS15_", c(1,5,7,8,15))]
RAQ_df_complete$BIS15_Non_Planning <- apply(BIS15_Non_Planning_col, 1, mean)
BIS15_Motor_col <- RAQ_df_complete[, paste0("BIS15_", c(2,9,10,12,13))]
RAQ_df_complete$BIS15_Motor <- apply(BIS15_Motor_col, 1, mean)
BIS15_Attentional_col <- RAQ_df_complete[, paste0("BIS15_", c(3,4,6,11,14))]
RAQ_df_complete$BIS15_Attentional <- apply(BIS15_Attentional_col, 1, mean)

# I8 scores (calculated as means)
I8_Totalscore_col <- RAQ_df_complete[, paste0("I8_", 1:8)]
RAQ_df_complete$I8_Totalscore <- apply(I8_Totalscore_col, 1, mean)
I8_Urgence_col <- RAQ_df_complete[, paste0("I8_", c(1,2))]
RAQ_df_complete$I8_Urgence <- apply(I8_Urgence_col, 1, mean)
I8_Intention_col <- RAQ_df_complete[, paste0("I8_", c(3,4))]
RAQ_df_complete$I8_Intention <- apply(I8_Intention_col, 1, mean)
I8_Endurance_col <- RAQ_df_complete[, paste0("I8_", c(5,6))]
RAQ_df_complete$I8_Endurance <- apply(I8_Endurance_col, 1, mean)
I8_Risk_Taking_col <- RAQ_df_complete[, paste0("I8_", c(7,8))]
RAQ_df_complete$I8_Risk_Taking <- apply(I8_Risk_Taking_col, 1, mean)

# ADHSSB totalscore (calculated as sum)
ADHSSB_Totalscore_col <- RAQ_df_complete[, paste0("ADHSSB_", 1:22)]
RAQ_df_complete$ADHSSB_Totalscore <- apply(ADHSSB_Totalscore_col, 1, sum)

# BAI totalscore (calculated as sum)
BAI_Totalscore_col <- RAQ_df_complete[, paste0("BAI_", 1:21)]
RAQ_df_complete$BAI_Totalscore <- apply(BAI_Totalscore_col, 1, sum)

# BDIII totalscore (calculated as sum)
BDIII_Totalscore_col <- RAQ_df_complete[, paste0("BDIII_", 1:21)]
RAQ_df_complete$BDIII_Totalscore <- apply(BDIII_Totalscore_col, 1, sum)

# HSRQ scores (calculated as means)
HSRQ_Frustrtol_Impulsecontrol_col <- RAQ_df_complete[, paste0("HSRQ_", c(1,6,9,15,18,23,32))]
RAQ_df_complete$HSRQ_Frustrtol_Impulsecontrol <- apply(HSRQ_Frustrtol_Impulsecontrol_col, 1, mean)
HSRQ_Affectdiff_tol_col <- RAQ_df_complete[, paste0("HSRQ_", c(2,5,13,19,21,25,30))]
RAQ_df_complete$HSRQ_Affectdiff_tol <- apply(HSRQ_Affectdiff_tol_col, 1, mean)
HSRQ_Interpersonal_Disturbances_col <- RAQ_df_complete[, paste0("HSRQ_", c(3,8,11,17,22,27,33))]
RAQ_df_complete$HSRQ_Interpersonal_Disturbances <- apply(HSRQ_Interpersonal_Disturbances_col, 1, mean)
HSRQ_Identity_Disturbances_col <- RAQ_df_complete[, paste0("HSRQ_", c(4,24,26,28,29,31,34))]
RAQ_df_complete$HSRQ_Identity_Disturbances <- apply(HSRQ_Identity_Disturbances_col, 1, mean)
HSRQ_Selfesteem_col <- RAQ_df_complete[, paste0("HSRQ_", c(7,10,12,14,16,20,3))]
RAQ_df_complete$HSRQ_Selfesteem <- apply(HSRQ_Selfesteem_col, 1, mean)
RAQ_df_complete$HSRQ_Totalscore <- with(RAQ_df_complete, HSRQ_Frustrtol_Impulsecontrol + HSRQ_Affectdiff_tol + HSRQ_Interpersonal_Disturbances + HSRQ_Identity_Disturbances + HSRQ_Selfesteem)

#save(RAQ_df_complete, file ="RAQ_df_complete.rda")

### converting the complete data frame in a mids
RAQ_mids_complete <- as.mids(RAQ_df_complete)
#save(RAQ_mids_complete, file = "RAQ_mids_complete.rda")

# note: now we have two objects to do our analyses: a mids (*RAQ_mids_complete*) and a data frame (*RAQ_df_complete*)


#-----------------------------------------------------------------------------------------------------------------------------
# describing demographics and clinical characteristics of the subsamples PG, CG and TS
# calculating demographic differences between PG and the other two subsamples via chi-square tests and Cramér’s V 
#-----------------------------------------------------------------------------------------------------------------------------

subsample_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0, ]
subsample_PG <- RAQ_df_complete[RAQ_df_complete$Sample == 1, ]
subsample_TS <- RAQ_df_complete[RAQ_df_complete$Sample == 2, ]

## for describing the demographics we only look at non-imputated data (imp=0)
demographics_CG <- subsample_CG[subsample_CG$.imp == 0, ]
demographics_PG <- subsample_PG[subsample_PG$.imp == 0, ]
demographics_TS <- subsample_TS[subsample_TS$.imp == 0, ]

# n
n_CG <- nrow(demographics_CG)
n_PG <- nrow(demographics_PG)
n_TS <- nrow(demographics_TS)

# age and age groups
mv_PG_age <- mean(demographics_PG$Age_years)
sd_PG_age <- sd(demographics_PG$Age_years)

mv_PG_age_groups <- mean(demographics_PG$Age_groups)
sd_PG_age_groups <- sd(demographics_PG$Age_groups)

mv_CG_age_groups <- mean(demographics_CG$Age_groups)
sd_CG_age_groups <- sd(demographics_CG$Age_groups)

mv_TS_age_groups <- mean(demographics_TS$Age_groups)
sd_TS_age_groups <- sd(demographics_TS$Age_groups)

# gender
n_PG_male <- nrow(demographics_PG[demographics_PG$Gender == 1, ])
n_PG_female <- nrow(demographics_PG[demographics_PG$Gender == 0, ])

n_CG_male <- nrow(demographics_CG[demographics_CG$Gender == 1, ])
n_CG_female <- nrow(demographics_CG[demographics_CG$Gender == 0, ])

n_TS_male <- nrow(demographics_TS[demographics_TS$Gender == 1, ])
n_TS_female <- nrow(demographics_TS[demographics_TS$Gender == 0, ])

# education 
n_PG_no_degree <- nrow(demographics_PG[demographics_PG$Level_of_education == 1 & !is.na(demographics_PG$Level_of_education), ])
n_PG_sec_ed <- nrow(demographics_PG[demographics_PG$Level_of_education == 2 & !is.na(demographics_PG$Level_of_education), ])
n_PG_sec_cert <- nrow(demographics_PG[demographics_PG$Level_of_education == 3 & !is.na(demographics_PG$Level_of_education), ])
n_PG_gen_qual <- nrow(demographics_PG[demographics_PG$Level_of_education == 4 & !is.na(demographics_PG$Level_of_education), ])
n_PG_university <- nrow(demographics_PG[demographics_PG$Level_of_education == 5 & !is.na(demographics_PG$Level_of_education), ])

n_CG_no_degree <- nrow(demographics_CG[demographics_CG$Level_of_education == 1, ])
n_CG_sec_ed <- nrow(demographics_CG[demographics_CG$Level_of_education == 2, ])
n_CG_sec_cert <- nrow(demographics_CG[demographics_CG$Level_of_education == 3, ])
n_CG_gen_qual <- nrow(demographics_CG[demographics_CG$Level_of_education == 4, ])
n_CG_university <- nrow(demographics_CG[demographics_CG$Level_of_education == 5, ])

n_TS_no_degree <- nrow(demographics_TS[demographics_TS$Level_of_education == 1, ])
n_TS_sec_ed <- nrow(demographics_TS[demographics_TS$Level_of_education == 2, ])
n_TS_sec_cert <- nrow(demographics_TS[demographics_TS$Level_of_education == 3, ])
n_TS_gen_qual <- nrow(demographics_TS[demographics_TS$Level_of_education == 4, ])
n_TS_university <- nrow(demographics_TS[demographics_TS$Level_of_education == 5, ])

# country of birth
n_PG_Germany <- nrow(demographics_PG[demographics_PG$Country_of_birth_Germany == 1, ])
n_PG_not_Germany <- nrow(demographics_PG[demographics_PG$Country_of_birth_Germany == 0, ])

## chi-square test for differences between the distribution of demographic characteristics 

## between PG and CG
demographics_PG_CG <- rbind(demographics_PG, demographics_CG)

# age
t_PG_CG_age <- table(demographics_PG_CG$Age_groups, demographics_PG_CG$Sample)
addmargins(t_PG_CG_age)
round(addmargins(prop.table(t_PG_CG_age)), 2) 
chisq.test(t_PG_CG_age)
Cramers_V_age_PG_CG <- sqrt(21.103/((n_PG+n_CG)*(2-1)))

# gender
t_PG_CG_gender <- table(demographics_PG_CG$Gender, demographics_PG_CG$Sample)
addmargins(t_PG_CG_gender)
round(addmargins(prop.table(t_PG_CG_gender)), 2) 
chisq.test(t_PG_CG_gender) 
Cramers_V_gender_PG_CG <- sqrt(16.877/((n_PG+n_CG)*(2-1)))

# level of education
t_PG_CG_education <- table(demographics_PG_CG$Level_of_education, demographics_PG_CG$Sample)
addmargins(t_PG_CG_education)
round(addmargins(prop.table(t_PG_CG_education)), 2) 
chisq.test(t_PG_CG_education)
Cramers_V_education_PG_CG <- sqrt(170.5/((n_PG+n_CG)*(2-1)))

## between PG and TS
demographics_PG_TS <- rbind(demographics_PG, demographics_TS)

# age
t_PG_TS_age <- table(demographics_PG_TS$Age_groups, demographics_PG_TS$Sample)
addmargins(t_PG_TS_age)
round(addmargins(prop.table(t_PG_TS_age)), 2) 
chisq.test(t_PG_TS_age)
Cramers_V_age_PG_TS <- sqrt(10.159/((n_PG+n_CG)*(2-1)))

# gender
t_PG_TS_gender <- table(demographics_PG_TS$Gender, demographics_PG_TS$Sample)
addmargins(t_PG_TS_gender)
round(addmargins(prop.table(t_PG_TS_gender)), 2) 
chisq.test(t_PG_TS_gender)
format(3.887e-08, scientific = FALSE)
Cramers_V_gender_PG_TS <- sqrt(30.205/((n_PG+n_CG)*(2-1)))

# level of education
t_PG_TS_education <- table(demographics_PG_TS$Level_of_education, demographics_PG_TS$Sample)
addmargins(t_PG_TS_education)
round(addmargins(prop.table(t_PG_TS_education)), 2) 
chisq.test(t_PG_TS_education)
Cramers_V_education_PG_TS <- sqrt(11.377/((n_PG+n_CG)*(2-1)))


#-----------------------------------------------------------------------------------------------------------------------------
# validation of the RAQ-R in the PG
#-----------------------------------------------------------------------------------------------------------------------------
library(psy)
library(dplyr)
library(semTools)

## some coefficients can't be calculated with a mids. In those cases we calculate the coefficients for each iteration and check for differences
valid_RAQ_PG_1 <- subsample_PG[subsample_PG$.imp == 1, paste0("RAQ_", 1:22)]
valid_RAQ_PG_2 <- subsample_PG[subsample_PG$.imp == 2, paste0("RAQ_", 1:22)]
valid_RAQ_PG_3 <- subsample_PG[subsample_PG$.imp == 3, paste0("RAQ_", 1:22)]
valid_RAQ_PG_4 <- subsample_PG[subsample_PG$.imp == 4, paste0("RAQ_", 1:22)]
valid_RAQ_PG_5 <- subsample_PG[subsample_PG$.imp == 5, paste0("RAQ_", 1:22)]

# cronbach's alpha
alpha_1 <- cronbach(valid_RAQ_PG_1)
alpha_2 <- cronbach(valid_RAQ_PG_2)
alpha_3 <- cronbach(valid_RAQ_PG_3)
alpha_4 <- cronbach(valid_RAQ_PG_4)
alpha_5 <- cronbach(valid_RAQ_PG_5)

# PCA
PCA_1 <- princomp(valid_RAQ_PG_1)
PCA_2 <- princomp(valid_RAQ_PG_2)
PCA_3 <- princomp(valid_RAQ_PG_3)
PCA_4 <- princomp(valid_RAQ_PG_4)
PCA_5 <- princomp(valid_RAQ_PG_5)

# factor structure
plot(PCA_1)
plot(PCA_2)
plot(PCA_3)
plot(PCA_4)
plot(PCA_5)

# CFA for loadings
RAQ_items <- paste0('factor1 =~ ', paste(paste0('RAQ_', 1:22), collapse='+'))

# loadings
CFA_1 <- cfa(RAQ_items, data = valid_RAQ_PG_1)
loadings_1 <- standardizedSolution(CFA_1) %>%filter(op == "=~", lhs == "factor1") %>% select(est.std)
CFA_2 <- cfa(RAQ_items, data = valid_RAQ_PG_2)
loadings_2 <- standardizedSolution(CFA_2) %>%filter(op == "=~", lhs == "factor1") %>% select(est.std)
CFA_3 <- cfa(RAQ_items, data = valid_RAQ_PG_3)
loadings_3 <- standardizedSolution(CFA_3) %>%filter(op == "=~", lhs == "factor1") %>% select(est.std)
CFA_4 <- cfa(RAQ_items, data = valid_RAQ_PG_4)
loadings_4 <- standardizedSolution(CFA_4) %>%filter(op == "=~", lhs == "factor1") %>% select(est.std)
CFA_5 <- cfa(RAQ_items, data = valid_RAQ_PG_5)
loadings_5 <- standardizedSolution(CFA_5) %>%filter(op == "=~", lhs == "factor1") %>% select(est.std)

range(loadings_1)
range(loadings_2)
range(loadings_3)
range(loadings_4)
range(loadings_5)

# composite reliability (squared sum of all loadings divided by that same figure plus the sum of 1 minus the loadings squared)
com_rel_1 <- sum(loadings_1) ^ 2 / ((sum(loadings_1)^ 2)  + sum(1 - loadings_1^ 2))	
com_rel_2 <- sum(loadings_2) ^ 2 / ((sum(loadings_2)^ 2)  + sum(1 - loadings_2^ 2))	
com_rel_3 <- sum(loadings_3) ^ 2 / ((sum(loadings_3)^ 2)  + sum(1 - loadings_3^ 2))	
com_rel_4 <- sum(loadings_4) ^ 2 / ((sum(loadings_4)^ 2)  + sum(1 - loadings_4^ 2))	
com_rel_5 <- sum(loadings_5) ^ 2 / ((sum(loadings_5)^ 2)  + sum(1 - loadings_5^ 2))	

# reliability estimates incl. average variance extracted (sum of all factor squares divided by the number of items)
reliability(CFA_1)
reliability(CFA_2)
reliability(CFA_3)
reliability(CFA_4)
reliability(CFA_5)

## pearson correlations between RAQ-R and those self-assessments/subscales used for validation

# condition for calculating p-values: normal distribution within the PG
with(subsample_PG, qqnorm(RAQ_Totalscore[.imp==1], ylim=c(-20, 100)))
with(subsample_PG, qqline(RAQ_Totalscore[.imp==1]))

with(subsample_PG, qqnorm(RAQ_Totalscore[.imp==2], ylim=c(-20, 100)))
with(subsample_PG, qqline(RAQ_Totalscore[.imp==2]))

with(subsample_PG, qqnorm(RAQ_Totalscore[.imp==3], ylim=c(-20, 100)))
with(subsample_PG, qqline(RAQ_Totalscore[.imp==3]))

with(subsample_PG, qqnorm(RAQ_Totalscore[.imp==4], ylim=c(-20, 100)))
with(subsample_PG, qqline(RAQ_Totalscore[.imp==4]))

with(subsample_PG, qqnorm(RAQ_Totalscore[.imp==5], ylim=c(-20, 100)))
with(subsample_PG, qqline(RAQ_Totalscore[.imp==5]))

# calculating r for each iteration, r as the mean of those five r

# RAQ_R and QoL
r_QoL <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1 & Sample==1], QoL[.imp==1 & Sample==1]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==2 & Sample==1], QoL[.imp==2 & Sample==1]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==3 & Sample==1], QoL[.imp==3 & Sample==1]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==4 & Sample==1], QoL[.imp==4 & Sample==1]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==5 & Sample==1], QoL[.imp==5 & Sample==1])))/5
p_QoL <- pt((r_QoL*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_QoL)), nrow(subsample_PG)/6-2)

# RAQ_R and BSI_GSI
r_BSI_GSI <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], BSI_GSI[.imp==1]))+
                with(subsample_PG, cor(RAQ_Totalscore[.imp==2], BSI_GSI[.imp==2]))+
                with(subsample_PG, cor(RAQ_Totalscore[.imp==3], BSI_GSI[.imp==3]))+
                with(subsample_PG, cor(RAQ_Totalscore[.imp==4], BSI_GSI[.imp==4]))+
                with(subsample_PG, cor(RAQ_Totalscore[.imp==5], BSI_GSI[.imp==5])))/5
p_BSI_GSI <- 1-pt((r_BSI_GSI*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_BSI_GSI)), nrow(subsample_PG)/6-2)

# RAQ_R and BSI_Aggression_Hostility
r_BSI_Aggression_Hostility <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], BSI_Aggression_Hostility[.imp==1]))+
                                 with(subsample_PG, cor(RAQ_Totalscore[.imp==2], BSI_Aggression_Hostility[.imp==2]))+
                                 with(subsample_PG, cor(RAQ_Totalscore[.imp==3], BSI_Aggression_Hostility[.imp==3]))+
                                 with(subsample_PG, cor(RAQ_Totalscore[.imp==4], BSI_Aggression_Hostility[.imp==4]))+
                                 with(subsample_PG, cor(RAQ_Totalscore[.imp==5], BSI_Aggression_Hostility[.imp==5])))/5
p_BSI_Aggression_Hostility <- 1-pt((r_BSI_Aggression_Hostility*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_BSI_Aggression_Hostility)), nrow(subsample_PG)/6-2)

# RAQ_R and BIS15_Totalscore
r_BIS_15_Totalscore <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], BIS_15_Totalscore[.imp==1]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==2], BIS_15_Totalscore[.imp==2]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==3], BIS_15_Totalscore[.imp==3]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==4], BIS_15_Totalscore[.imp==4]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==5], BIS_15_Totalscore[.imp==5])))/5
p_BIS_15_Totalscore <- 1-pt((r_BIS_15_Totalscore*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_BIS_15_Totalscore)), nrow(subsample_PG)/6-2)

# RAQ_R and I8_Urgence
r_I8_Urgence <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], I8_Urgence[.imp==1]))+
                   with(subsample_PG, cor(RAQ_Totalscore[.imp==2], I8_Urgence[.imp==2]))+
                   with(subsample_PG, cor(RAQ_Totalscore[.imp==3], I8_Urgence[.imp==3]))+
                   with(subsample_PG, cor(RAQ_Totalscore[.imp==4], I8_Urgence[.imp==4]))+
                   with(subsample_PG, cor(RAQ_Totalscore[.imp==5], I8_Urgence[.imp==5])))/5
p_I8_Urgence <- 1-pt((r_I8_Urgence*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_I8_Urgence)), nrow(subsample_PG)/6-2)

# RAQ_R and I8_Intention
r_I8_Intention <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], I8_Intention[.imp==1]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==2], I8_Intention[.imp==2]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==3], I8_Intention[.imp==3]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==4], I8_Intention[.imp==4]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==5], I8_Intention[.imp==5])))/5
p_I8_Intention <- pt((r_I8_Intention*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_I8_Intention)), nrow(subsample_PG)/6-2)

# RAQ_R and I8_Endurance
r_I8_Endurance <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], I8_Endurance[.imp==1]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==2], I8_Endurance[.imp==2]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==3], I8_Endurance[.imp==3]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==4], I8_Endurance[.imp==4]))+
                     with(subsample_PG, cor(RAQ_Totalscore[.imp==5], I8_Endurance[.imp==5])))/5
p_I8_Endurance <- pt((r_I8_Endurance*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_I8_Endurance)), nrow(subsample_PG)/6-2)

#RAQ_R and I8_Risk_Taking
r_I8_Risk_Taking <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], I8_Risk_Taking[.imp==1]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==2], I8_Risk_Taking[.imp==2]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==3], I8_Risk_Taking[.imp==3]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==4], I8_Risk_Taking[.imp==4]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==5], I8_Risk_Taking[.imp==5])))/5
p_I8_Risk_Taking <- 1-pt((r_I8_Risk_Taking*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_I8_Risk_Taking)), nrow(subsample_PG)/6-2)

# RAQ_R and ADHSSB_Totalscore
r_ADHSSB_Totalscore <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], ADHSSB_Totalscore[.imp==1]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==2], ADHSSB_Totalscore[.imp==2]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==3], ADHSSB_Totalscore[.imp==3]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==4], ADHSSB_Totalscore[.imp==4]))+
                          with(subsample_PG, cor(RAQ_Totalscore[.imp==5], ADHSSB_Totalscore[.imp==5])))/5
p_ADHSSB_Totalscore <- 1-pt((r_ADHSSB_Totalscore*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_ADHSSB_Totalscore)), nrow(subsample_PG)/6-2)

# RAQ_R and BAI_Totalscore
r_BAI_Totalscore <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], BAI_Totalscore[.imp==1]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==2], BAI_Totalscore[.imp==2]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==3], BAI_Totalscore[.imp==3]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==4], BAI_Totalscore[.imp==4]))+
                       with(subsample_PG, cor(RAQ_Totalscore[.imp==5], BAI_Totalscore[.imp==5])))/5
p_BAI_Totalscore <- 1-pt((r_BAI_Totalscore*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_BAI_Totalscore)), nrow(subsample_PG)/6-2)

# RAQ_R and BDIII_Totalscore
r_BDIII_Totalscore <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], BDIII_Totalscore[.imp==1]))+
                         with(subsample_PG, cor(RAQ_Totalscore[.imp==2], BDIII_Totalscore[.imp==2]))+
                         with(subsample_PG, cor(RAQ_Totalscore[.imp==3], BDIII_Totalscore[.imp==3]))+
                         with(subsample_PG, cor(RAQ_Totalscore[.imp==4], BDIII_Totalscore[.imp==4]))+
                         with(subsample_PG, cor(RAQ_Totalscore[.imp==5], BDIII_Totalscore[.imp==5])))/5
p_BDIII_Totalscore <- 1-pt((r_BDIII_Totalscore*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_BDIII_Totalscore)), nrow(subsample_PG)/6-2)

# RAQ_R and HSRQ_Totalscore
r_HSRQ_Totalscore <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], HSRQ_Totalscore[.imp==1]))+
                        with(subsample_PG, cor(RAQ_Totalscore[.imp==2], HSRQ_Totalscore[.imp==2]))+
                        with(subsample_PG, cor(RAQ_Totalscore[.imp==3], HSRQ_Totalscore[.imp==3]))+
                        with(subsample_PG, cor(RAQ_Totalscore[.imp==4], HSRQ_Totalscore[.imp==4]))+
                        with(subsample_PG, cor(RAQ_Totalscore[.imp==5], HSRQ_Totalscore[.imp==5])))/5
p_HSRQ_Totalscore <- 1-pt((r_HSRQ_Totalscore*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_HSRQ_Totalscore)), nrow(subsample_PG)/6-2)

# RAQ_R and HSRQ_Frustrtol_Impulsecontrol
r_HSRQ_Frustrtol_Impulsecontrol <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], HSRQ_Frustrtol_Impulsecontrol[.imp==1]))+
                                      with(subsample_PG, cor(RAQ_Totalscore[.imp==2], HSRQ_Frustrtol_Impulsecontrol[.imp==2]))+
                                      with(subsample_PG, cor(RAQ_Totalscore[.imp==3], HSRQ_Frustrtol_Impulsecontrol[.imp==3]))+
                                      with(subsample_PG, cor(RAQ_Totalscore[.imp==4], HSRQ_Frustrtol_Impulsecontrol[.imp==4]))+
                                      with(subsample_PG, cor(RAQ_Totalscore[.imp==5], HSRQ_Frustrtol_Impulsecontrol[.imp==5])))/5
p_HSRQ_Frustrtol_Impulsecontrol <- 1-pt((r_HSRQ_Frustrtol_Impulsecontrol*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_HSRQ_Frustrtol_Impulsecontrol)), nrow(subsample_PG)/6-2)

# RAQ_R and HSRQ_Affectdiff_tol
r_HSRQ_Affectdiff_tol <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], HSRQ_Affectdiff_tol[.imp==1]))+
                            with(subsample_PG, cor(RAQ_Totalscore[.imp==2], HSRQ_Affectdiff_tol[.imp==2]))+
                            with(subsample_PG, cor(RAQ_Totalscore[.imp==3], HSRQ_Affectdiff_tol[.imp==3]))+
                            with(subsample_PG, cor(RAQ_Totalscore[.imp==4], HSRQ_Affectdiff_tol[.imp==4]))+
                            with(subsample_PG, cor(RAQ_Totalscore[.imp==5], HSRQ_Affectdiff_tol[.imp==5])))/5
p_HSRQ_Affectdiff_tol <- 1-pt((r_HSRQ_Affectdiff_tol*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_HSRQ_Affectdiff_tol)), nrow(subsample_PG)/6-2)

#-----------------------------------------------------------------------------------------------------------------------------
# group comparisons
#-----------------------------------------------------------------------------------------------------------------------------
library(mice)

### descriptives of samples and diagnostic categories
# note: excluded are diagnoses with n=1

## Control Group
with(subsample_CG, table(Sample))
descriptives_CG <- summary(pool(with(data = as.mids(subsample_CG), lm(RAQ_Totalscore~1))))
# mean value (mv)
mv_CG <- descriptives_CG$estimate
# standard deviation (sd)  (sd=sqrt(n)*std.error)
sd_CG <- descriptives_CG$std.error*sqrt(n_CG)
# variance 
var_CG <- sd_CG^2
# 95%CI
c(mv_CG - qt(0.975, n_CG-1) * sd_CG / sqrt(n_CG), mv_CG + qt(0.975, n_CG-1) * sd_CG / sqrt(n_CG))

## PG
with(subsample_PG, table(Sample))
descriptives_PG <- summary(pool(with(data = as.mids(subsample_PG), lm(RAQ_Totalscore~1))))
# mv 
mv_PG <- descriptives_PG$estimate
# sd 
sd_PG <- descriptives_PG$std.error*sqrt(n_PG)
# 95%CI
c(mv_PG - qt(0.975, n_PG-1) * sd_PG / sqrt(n_PG), mv_PG + qt(0.975, n_PG-1) * sd_PG / sqrt(n_PG))
# median of PG
x_PG <- subsample_PG[subsample_PG$.imp>0, c(".imp", "RAQ_Totalscore")]
median_PG <- tapply(x_PG$RAQ_Totalscore, list(x_PG$.imp), median)
mean(median_PG)

## TS
with(subsample_TS, table(Sample))
descriptives_TS <- summary(pool(with(data = as.mids(subsample_TS), lm(RAQ_Totalscore~1))))
# mv
mv_TS <- descriptives_TS$estimate
# sd 
sd_TS <- descriptives_TS$std.error*sqrt(n_TS)
# 95%CI
c(mv_TS - qt(0.975, n_TS-1) * sd_TS / sqrt(n_TS), mv_TS + qt(0.975, n_TS-1) * sd_TS / sqrt(n_TS))
# median
x_TS <- subsample_TS[subsample_TS$.imp>0, c(".imp", "RAQ_Totalscore")]
median_TS <- tapply(x_TS$RAQ_Totalscore, list(x_TS$.imp), median)

## F10
subsample_F10 <- RAQ_df_complete[RAQ_df_complete$F10 == 1, ]
with(subsample_F10, table(Sample, F10))
descriptives_F10 <- summary(pool(with(data = as.mids(subsample_F10), lm(RAQ_Totalscore~1))))
n_F10 <- nrow(subsample_F10[subsample_F10$.imp>0,])/5
# mv
mv_F10 <- descriptives_F10$estimate
# sd 
sd_F10 <- descriptives_F10$std.error*sqrt(n_F10)
# 95%CI
c(mv_F10 - qt(0.975, n_F10-1) * sd_F10 / sqrt(n_F10), mv_F10 + qt(0.975, n_F10-1) * sd_F10 / sqrt(n_F10))
# median
x_F10 <- subsample_F10[subsample_F10$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F10 <- tapply(x_F10$RAQ_Totalscore, list(x_F10$.imp), median)

## F11_to_19
subsample_F11_to_19 <- RAQ_df_complete[RAQ_df_complete$F11_to_19 == 1, ]
with(subsample_F11_to_19, table(Sample, F11_to_19))
descriptives_F11_to_19 <- summary(pool(with(data = as.mids(subsample_F11_to_19), lm(RAQ_Totalscore~1))))
n_F11_to_19 <- nrow(subsample_F11_to_19[subsample_F11_to_19$.imp>0,])/5
# mv
mv_F11_to_19 <- descriptives_F11_to_19$estimate
# sd of F11_to_19
sd_F11_to_19 <- descriptives_F11_to_19$std.error*sqrt(n_F11_to_19)
# 95%CI
c(mv_F11_to_19 - qt(0.975, n_F11_to_19-1) * sd_F11_to_19 / sqrt(n_F11_to_19), mv_F11_to_19 + qt(0.975, n_F11_to_19-1) * sd_F11_to_19 / sqrt(n_F11_to_19))
# median
x_F11_to_19 <- subsample_F11_to_19[subsample_F11_to_19$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F11_to_19 <- tapply(x_F11_to_19$RAQ_Totalscore, list(x_F11_to_19$.imp), median)

## F20_23
subsample_F20_23 <- RAQ_df_complete[RAQ_df_complete$F20_23 == 1, ]
with(subsample_F20_23, table(Sample, F20_23))
descriptives_F20_23 <- summary(pool(with(data = as.mids(subsample_F20_23), lm(RAQ_Totalscore~1))))
n_F20_23 <- nrow(subsample_F20_23[subsample_F20_23$.imp>0,])/5
# mv
mv_F20_23 <- descriptives_F20_23$estimate
# sd of F20_23
sd_F20_23 <- descriptives_F20_23$std.error*sqrt(n_F20_23)
# 95%CI
c(mv_F20_23 - qt(0.975, n_F20_23-1) * sd_F20_23 / sqrt(n_F20_23), mv_F20_23 + qt(0.975, n_F20_23-1) * sd_F20_23 / sqrt(n_F20_23))
# median
x_F20_23 <- subsample_F20_23[subsample_F20_23$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F20_23 <- tapply(x_F20_23$RAQ_Totalscore, list(x_F20_23$.imp), median)
mean(median_F20_23)

## F31
subsample_F31 <- RAQ_df_complete[RAQ_df_complete$F31 == 1, ]
with(subsample_F31, table(Sample, F31))
descriptives_F31 <- summary(pool(with(data = as.mids(subsample_F31), lm(RAQ_Totalscore~1))))
n_F31 <- nrow(subsample_F31[subsample_F31$Sample==1 & subsample_F31$.imp>0,])/5
# mv
mv_F31 <- descriptives_F31$estimate
# sd of F31
sd_F31 <- descriptives_F31$std.error*sqrt(n_F31)
# 95%CI
c(mv_F31 - qt(0.975, n_F31-1) * sd_F31 / sqrt(n_F31), mv_F31 + qt(0.975, n_F31-1) * sd_F31 / sqrt(n_F31))
# median
x_F31 <- subsample_F31[subsample_F31$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F31 <- tapply(x_F31$RAQ_Totalscore, list(x_F31$.imp), median)

## F32_to_34
subsample_F32_to_34 <- RAQ_df_complete[RAQ_df_complete$F32_to_34 == 1, ]
with(subsample_F32_to_34, table(Sample, F32_to_34))
descriptives_F32_to_34 <- summary(pool(with(data = as.mids(subsample_F32_to_34), lm(RAQ_Totalscore~1))))
n_F32_to_34 <- nrow(subsample_F32_to_34[subsample_F32_to_34$.imp>0,])/5
# mv
mv_F32_to_34 <- descriptives_F32_to_34$estimate
# sd
sd_F32_to_34 <- descriptives_F32_to_34$std.error*sqrt(n_F32_to_34)
# 95%CI
c(mv_F32_to_34 - qt(0.975, n_F32_to_34-1) * sd_F32_to_34 / sqrt(n_F32_to_34), mv_F32_to_34 + qt(0.975, n_F32_to_34-1) * sd_F32_to_34 / sqrt(n_F32_to_34))
# median
x_F32_to_34 <- subsample_F32_to_34[subsample_F32_to_34$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F32_to_34 <- tapply(x_F32_to_34$RAQ_Totalscore, list(x_F32_to_34$.imp), median)

## F40_41
subsample_F40_41 <- RAQ_df_complete[RAQ_df_complete$F40_41 == 1, ]
with(subsample_F40_41, table(Sample, F40_41))
descriptives_F40_41 <- summary(pool(with(data = as.mids(subsample_F40_41), lm(RAQ_Totalscore~1))))
n_F40_41 <- nrow(subsample_F40_41[subsample_F40_41$.imp>0,])/5
# mv
mv_F40_41 <- descriptives_F40_41$estimate
# sd
sd_F40_41 <- descriptives_F40_41$std.error*sqrt(n_F40_41)
# 95%CI
c(mv_F40_41 - qt(0.975, n_F40_41-1) * sd_F40_41 / sqrt(n_F40_41), mv_F40_41 + qt(0.975, n_F40_41-1) * sd_F40_41 / sqrt(n_F40_41))
# median
x_F40_41 <- subsample_F40_41[subsample_F40_41$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F40_41 <- tapply(x_F40_41$RAQ_Totalscore, list(x_F40_41$.imp), median)

## F42
subsample_F42 <- RAQ_df_complete[RAQ_df_complete$F42 == 1, ]
with(subsample_F42, table(Sample, F42))
descriptives_F42 <- summary(pool(with(data = as.mids(subsample_F42), lm(RAQ_Totalscore~1))))
n_F42 <- nrow(subsample_F42[subsample_F42$.imp>0,])/5
# mv
mv_F42 <- descriptives_F42$estimate
# sd
sd_F42 <- descriptives_F42$std.error*sqrt(n_F42)
# 95%CI
c(mv_F42 - qt(0.975, n_F42-1) * sd_F42 / sqrt(n_F42), mv_F42 + qt(0.975, n_F42-1) * sd_F42 / sqrt(n_F42))
# median
x_F42 <- subsample_F42[subsample_F42$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F42 <- tapply(x_F42$RAQ_Totalscore, list(x_F42$.imp), median)
mean(median_F42)

## F43
subsample_F43 <- RAQ_df_complete[RAQ_df_complete$F43 == 1, ]
with(subsample_F43, table(Sample, F43))
descriptives_F43 <- summary(pool(with(data = as.mids(subsample_F43), lm(RAQ_Totalscore~1))))
n_F43 <- nrow(subsample_F43[subsample_F43$.imp>0,])/5
# mv
mv_F43 <- descriptives_F43$estimate
# sd
sd_F43 <- descriptives_F43$std.error*sqrt(n_F43)
# 95%CI
c(mv_F43 - qt(0.975, n_F43-1) * sd_F43 / sqrt(n_F43), mv_F43 + qt(0.975, n_F43-1) * sd_F43 / sqrt(n_F43))
# median
x_F43 <- subsample_F43[subsample_F43$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F43 <- tapply(x_F43$RAQ_Totalscore, list(x_F43$.imp), median)

## F44_45_48
subsample_F44_45_48 <- RAQ_df_complete[RAQ_df_complete$F44_45_48 == 1, ]
with(subsample_F44_45_48, table(Sample, F44_45_48))
descriptives_F44_45_48 <- summary(pool(with(data = as.mids(subsample_F44_45_48), lm(RAQ_Totalscore~1))))
n_F44_45_48 <- nrow(subsample_F44_45_48[subsample_F44_45_48$.imp>0,])/5
# mv
mv_F44_45_48 <- descriptives_F44_45_48$estimate
# sd
sd_F44_45_48 <- descriptives_F44_45_48$std.error*sqrt(n_F44_45_48)
# 95%CI
c(mv_F44_45_48 - qt(0.975, n_F44_45_48-1) * sd_F44_45_48 / sqrt(n_F44_45_48), mv_F44_45_48 + qt(0.975, n_F44_45_48-1) * sd_F44_45_48 / sqrt(n_F44_45_48))
# median
x_F44_45_48 <- subsample_F44_45_48[subsample_F44_45_48$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F44_45_48 <- tapply(x_F44_45_48$RAQ_Totalscore, list(x_F44_45_48$.imp), median)
mean(median_F44_45_48)

## F50
subsample_F50 <- RAQ_df_complete[RAQ_df_complete$F50 == 1, ]
with(subsample_F50, table(Sample, F50))
descriptives_F50 <- summary(pool(with(data = as.mids(subsample_F50), lm(RAQ_Totalscore~1))))
n_F50 <- nrow(subsample_F50[subsample_F50$.imp>0,])/5
# mv
mv_F50 <- descriptives_F50$estimate
# sd
sd_F50 <- descriptives_F50$std.error*sqrt(n_F50)
# 95%CI
c(mv_F50 - qt(0.975, n_F50-1) * sd_F50 / sqrt(n_F50), mv_F50 + qt(0.975, n_F50-1) * sd_F50 / sqrt(n_F50))
# median
x_F50 <- subsample_F50[subsample_F50$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F50 <- tapply(x_F50$RAQ_Totalscore, list(x_F50$.imp), median)

## F54
subsample_F54 <- RAQ_df_complete[RAQ_df_complete$F54 == 1, ]
with(subsample_F54, table(Sample, F54))
descriptives_F54 <- summary(pool(with(data = as.mids(subsample_F54), lm(RAQ_Totalscore~1))))
n_F54 <- nrow(subsample_F54[subsample_F54$.imp>0,])/5
# mv
mv_F54 <- descriptives_F54$estimate
# sd
sd_F54 <- descriptives_F54$std.error*sqrt(n_F54)
# 95%CI
c(mv_F54 - qt(0.975, n_F54-1) * sd_F54 / sqrt(n_F54), mv_F54 + qt(0.975, n_F54-1) * sd_F54 / sqrt(n_F54))
# median
x_F54 <- subsample_F54[subsample_F54$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F54 <- tapply(x_F54$RAQ_Totalscore, list(x_F54$.imp), median)

## F60_61
subsample_F60_61 <- RAQ_df_complete[RAQ_df_complete$F60_61 == 1, ]
with(subsample_F60_61, table(Sample, F60_61))
descriptives_F60_61 <- summary(pool(with(data = as.mids(subsample_F60_61), lm(RAQ_Totalscore~1))))
n_F60_61 <- nrow(subsample_F60_61[subsample_F60_61$.imp>0,])/5
# mv
mv_F60_61 <- descriptives_F60_61$estimate
# sd
sd_F60_61 <- descriptives_F60_61$std.error*sqrt(n_F60_61)
# 95%CI
c(mv_F60_61 - qt(0.975, n_F60_61-1) * sd_F60_61 / sqrt(n_F60_61), mv_F60_61 + qt(0.975, n_F60_61-1) * sd_F60_61 / sqrt(n_F60_61))
#median of F60_61
x_F60_61 <- subsample_F60_61[subsample_F60_61$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F60_61 <- tapply(x_F60_61$RAQ_Totalscore, list(x_F60_61$.imp), median)

## F60.31
subsample_F60.31 <- RAQ_df_complete[RAQ_df_complete$F60.31 == 1, ]
with(subsample_F60.31, table(Sample, F60.31))
descriptives_F60.31 <- summary(pool(with(data = as.mids(subsample_F60.31), lm(RAQ_Totalscore~1))))
n_F60.31 <- nrow(subsample_F60.31[subsample_F60.31$.imp>0,])/5
# mv
mv_F60.31 <- descriptives_F60.31$estimate
# sd
sd_F60.31 <- descriptives_F60.31$std.error*sqrt(n_F60.31)
# 95%CI
c(mv_F60.31 - qt(0.975, n_F60.31-1) * sd_F60.31 / sqrt(n_F60.31), mv_F60.31 + qt(0.975, n_F60.31-1) * sd_F60.31 / sqrt(n_F60.31))
# median
x_F60.31 <- subsample_F60.31[subsample_F60.31$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F60.31 <- tapply(x_F60.31$RAQ_Totalscore, list(x_F60.31$.imp), median)

## Cluster_C_PD
subsample_Cluster_C_PD <- RAQ_df_complete[RAQ_df_complete$Cluster_C_PD == 1, ]
with(subsample_Cluster_C_PD, table(Sample, Cluster_C_PD))
descriptives_Cluster_C_PD <- summary(pool(with(data = as.mids(subsample_Cluster_C_PD), lm(RAQ_Totalscore~1))))
n_Cluster_C_PD <- nrow(subsample_Cluster_C_PD[subsample_Cluster_C_PD$.imp>0,])/5
# mv
mv_Cluster_C_PD <- descriptives_Cluster_C_PD$estimate
# sd
sd_Cluster_C_PD <- descriptives_Cluster_C_PD$std.error*sqrt(n_Cluster_C_PD)
# 95%CI
c(mv_Cluster_C_PD - qt(0.975, n_Cluster_C_PD-1) * sd_Cluster_C_PD / sqrt(n_Cluster_C_PD), mv_Cluster_C_PD + qt(0.975, n_Cluster_C_PD-1) * sd_Cluster_C_PD / sqrt(n_Cluster_C_PD))
# median
x_Cluster_C_PD <- subsample_Cluster_C_PD[subsample_Cluster_C_PD$.imp>0, c(".imp", "RAQ_Totalscore")]
median_Cluster_C_PD <- tapply(x_Cluster_C_PD$RAQ_Totalscore, list(x_Cluster_C_PD$.imp), median)

## F62
subsample_F62 <- RAQ_df_complete[RAQ_df_complete$F62 == 1, ]
with(subsample_F62, table(Sample, F62))
descriptives_F62 <- summary(pool(with(data = as.mids(subsample_F62), lm(RAQ_Totalscore~1))))
n_F62 <- nrow(subsample_F62[subsample_F62$.imp>0,])/5
# mv
mv_F62 <- descriptives_F62$estimate
# sd
sd_F62 <- descriptives_F62$std.error*sqrt(n_F62)
# 95%CI
c(mv_F62 - qt(0.975, n_F62-1) * sd_F62 / sqrt(n_F62), mv_F62 + qt(0.975, n_F62-1) * sd_F62 / sqrt(n_F62))
# median 
x_F62 <- subsample_F62[subsample_F62$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F62 <- tapply(x_F62$RAQ_Totalscore, list(x_F62$.imp), median)

## F63
subsample_F63 <- RAQ_df_complete[RAQ_df_complete$F63 == 1, ]
with(subsample_F63, table(Sample, F63))
descriptives_F63 <- summary(pool(with(data = as.mids(subsample_F63), lm(RAQ_Totalscore~1))))
n_F63 <- nrow(subsample_F63[subsample_F63$.imp>0,])/5
# mv
mv_F63 <- descriptives_F63$estimate
# sd
sd_F63 <- descriptives_F63$std.error*sqrt(n_F63)
# 95%CI
c(mv_F63 - qt(0.975, n_F63-1) * sd_F63 / sqrt(n_F63), mv_F63 + qt(0.975, n_F63-1) * sd_F63 / sqrt(n_F63))
# median
x_F63 <- subsample_F63[subsample_F63$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F63 <- tapply(x_F63$RAQ_Totalscore, list(x_F63$.imp), median)

## F90.0
subsample_F90.0 <- RAQ_df_complete[RAQ_df_complete$F90.0 == 1, ]
with(subsample_F90.0, table(Sample, F90.0))
descriptives_F90.0 <- summary(pool(with(data = as.mids(subsample_F90.0), lm(RAQ_Totalscore~1))))
n_F90.0 <- nrow(subsample_F90.0[subsample_F90.0$.imp>0,])/5
# mv
mv_F90.0 <- descriptives_F90.0$estimate
# sd
sd_F90.0 <- descriptives_F90.0$std.error*sqrt(n_F90.0)
# 95%CI
c(mv_F90.0 - qt(0.975, n_F90.0-1) * sd_F90.0 / sqrt(n_F90.0), mv_F90.0 + qt(0.975, n_F90.0-1) * sd_F90.0 / sqrt(n_F90.0))
# median
x_F90.0 <- subsample_F90.0[subsample_F90.0$.imp>0, c(".imp", "RAQ_Totalscore")]
median_F90.0 <- tapply(x_F90.0$RAQ_Totalscore, list(x_F90.0$.imp), median)


### check normal distribution as a condition for t-tests and correlations via qqplots
# note: qqplots for PG see above

with(subsample_CG, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_CG, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_TS, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_TS, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F10, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F10, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F11_to_19, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F11_to_19, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F20_23, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F20_23, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F31, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F31, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F32_to_34, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F32_to_34, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F40_41, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F40_41, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F42, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F42, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F43, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F43, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F44_45_48, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F44_45_48, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F50, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F50, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F54, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F54, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F60_61, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F60_61, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F60.31, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F60.31, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_Cluster_C_PD, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_Cluster_C_PD, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F62, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F62, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F63, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F63, qqline(RAQ_Totalscore[.imp==0]))

with(subsample_F90.0, qqnorm(RAQ_Totalscore[.imp==0], ylim=c(-20, 100)))
with(subsample_F90.0, qqline(RAQ_Totalscore[.imp==0]))

# note: some ggplots don't clearly show a normal distribution -> that's not a big problem for n>20 or n>30 but for small groups --> calculating Wilcoxon-test additionally

### t-tests, wilcoxon-tests and cohen's d for differences between CG and PG and diagnostic categories (incl TS)

RAQ_values_CG <- subsample_CG[subsample_CG$.imp==0, "RAQ_Totalscore"]

## PG vs CG
subsample_PG_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$Sample == 1, ]
mids_PG_CG <- as.mids(subsample_PG_CG)
# t-test
ttest.PG_CG_raw <- with(data=mids_PG_CG, exp=lm(RAQ_Totalscore~Sample))
ttest.PG_CG <- summary(pool(ttest.PG_CG_raw))
# wilcoxon-test
Wilcox_pvalues_PG_CG <- NULL
for(i in 1:5) {
  RAQ_values_PG <- subsample_PG$RAQ_Totalscore[subsample_PG$.imp==i]
  Wilcox_pvalues_PG_CG <- c(Wilcox_pvalues_PG_CG, wilcox.test(RAQ_values_CG, RAQ_values_PG)$p.value)}
# check p-values
all(Wilcox_pvalues_PG_CG < 0.001)
# cohen's d (cd) for different group sizes
pooled_sd_PG_CG <- sqrt(((n_CG-1)*var_CG+(n_PG-1)*sd_PG^2)/(n_CG+n_PG-2)) 
cd_PG_CG <- (-mv_CG+mv_PG)/pooled_sd_PG_CG
#note: cd is already pooled since mv and sd are pooled 

## TS vs CG
subsample_TS_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$Sample == 2, ]
mids_TS_CG <- as.mids(subsample_TS_CG)
# t-test
ttest.TS_CG_raw <- with(data=mids_TS_CG, exp=lm(RAQ_Totalscore~Sample))
ttest.TS_CG <- summary(pool(ttest.TS_CG_raw))
# wilcoxon-test
Wilcox_pvalues_TS_CG <- NULL
for(i in 1:5) {
  RAQ_values_TS <- subsample_TS$RAQ_Totalscore[subsample_TS$.imp==i]
  Wilcox_pvalues_TS_CG <- c(Wilcox_pvalues_TS_CG, wilcox.test(RAQ_values_CG, RAQ_values_TS)$p.value)}
#cohen's d 
pooled_sd_TS_CG <- sqrt(((n_CG-1)*var_CG+(n_TS-1)*sd_TS^2)/(n_CG+n_TS-2)) 
cd_TS_CG <- (-mv_CG+mv_TS)/pooled_sd_TS_CG

# TS vs PG
subsample_TS_PG <- RAQ_df_complete[RAQ_df_complete$Sample == 1 | RAQ_df_complete$Sample == 2, ]
mids_TS_PG <- as.mids(subsample_TS_PG)
# t-test
ttest.TS_PG_raw <- with(data=mids_TS_PG, exp=lm(RAQ_Totalscore~factor(Sample)))
ttest.TS_PG <- summary(pool(ttest.TS_PG_raw))
# wilcoxon-test
Wilcox_pvalues_TS_PG <- NULL
for(i in 1:5) {Wilcox_pvalues_TS_PG <- c(Wilcox_pvalues_TS_PG, wilcox.test(RAQ_values_PG, RAQ_values_TS)$p.value)}
# cohen's d for same sample size (n(TS)=127 and n(PG)=156 are close enough)
var_PG <- sd_PG * sd_PG
var_TS <- sd_TS * sd_TS
cd_TS_PG_2 <- (mv_TS+mv_PG)/sqrt((var_PG^2+var_TS^2)/2)

## F10 vs CG
subsample_F10_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F10 == 1, ]
mids_F10_CG <- as.mids(subsample_F10_CG)
# t-test
ttest.F10_CG_raw <- with(data=mids_F10_CG, exp=lm(RAQ_Totalscore~F10))
ttest.F10_CG <- summary(pool(ttest.F10_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F10_CG <- NULL
for(i in 1:5) {
  RAQ_values_F10 <- subsample_F10$RAQ_Totalscore[subsample_F10$.imp==i]
  Wilcox_pvalues_F10_CG <- c(Wilcox_pvalues_F10_CG, wilcox.test(RAQ_values_CG, RAQ_values_F10)$p.value)}
# cohen's d 
pooled_sd_F10_CG <- sqrt(((n_CG-1)*var_CG+(n_F10-1)*sd_F10^2)/(n_CG+n_F10-2)) 
cd_F10_CG <- (-mv_CG+mv_F10)/pooled_sd_F10_CG

## F11-19 vs CG
subsample_F11_to_19_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F11_to_19 == 1, ]
mids_F11_to_19_CG <- as.mids(subsample_F11_to_19_CG)
# t-test
ttest.F11_to_19_CG_raw <- with(data=mids_F11_to_19_CG, exp=lm(RAQ_Totalscore~F11_to_19))
ttest.F11_to_19_CG <- summary(pool(ttest.F11_to_19_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F11_to_19_CG <- NULL
for(i in 1:5) {
  RAQ_values_F11_to_19 <- subsample_F11_to_19$RAQ_Totalscore[subsample_F11_to_19$.imp==i]
  Wilcox_pvalues_F11_to_19_CG <- c(Wilcox_pvalues_F11_to_19_CG, wilcox.test(RAQ_values_CG, RAQ_values_F11_to_19)$p.value)}
# cohen's d
pooled_sd_F11_to_19_CG <- sqrt(((n_CG-1)*var_CG+(n_F11_to_19-1)*sd_F11_to_19^2)/(n_CG+n_F11_to_19-2)) 
cd_F11_to_19_CG <- (-mv_CG+mv_F11_to_19)/pooled_sd_F11_to_19_CG

## F20_23 vs CG
subsample_F20_23_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F20_23 == 1, ]
mids_F20_23_CG <- as.mids(subsample_F20_23_CG)
# t-test
ttest.F20_23_CG_raw <- with(data=mids_F20_23_CG, exp=lm(RAQ_Totalscore~F20_23))
ttest.F20_23_CG <- summary(pool(ttest.F20_23_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F20_23_CG <- NULL
for(i in 1:5) {
  RAQ_values_F20_23 <- subsample_F20_23$RAQ_Totalscore[subsample_F20_23$.imp==i]
  Wilcox_pvalues_F20_23_CG <- c(Wilcox_pvalues_F20_23_CG, wilcox.test(RAQ_values_CG, RAQ_values_F20_23)$p.value)}
# cohen's d
pooled_sd_F20_23_CG <- sqrt(((n_CG-1)*var_CG+(n_F20_23-1)*sd_F20_23^2)/(n_CG+n_F20_23-2)) 
cd_F20_23_CG <- (-mv_CG+mv_F20_23)/pooled_sd_F20_23_CG

## F31 vs CG
subsample_F31_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F31 == 1, ]
mids_F31_CG <- as.mids(subsample_F31_CG)
# t-test
ttest.F31_CG_raw <- with(data=mids_F31_CG, exp=lm(RAQ_Totalscore~F31))
ttest.F31_CG <- summary(pool(ttest.F31_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F31_CG <- NULL
for(i in 1:5) {
  RAQ_values_F31 <- subsample_F31$RAQ_Totalscore[subsample_F31$.imp==i]
  Wilcox_pvalues_F31_CG <- c(Wilcox_pvalues_F31_CG, wilcox.test(RAQ_values_CG, RAQ_values_F31)$p.value)}
# cohen's d
pooled_sd_F31_CG <- sqrt(((n_CG-1)*var_CG+(n_F31-1)*sd_F31^2)/(n_CG+n_F31-2)) 
cd_F31_CG <- (-mv_CG+mv_F31)/pooled_sd_F31_CG

## F31 vs PG (PG without those with F31)
mids_PG <- as.mids(subsample_PG)
subsample_PGwithoutF31 <- subsample_PG[subsample_PG$F31==0, ]
descriptives_PGwithoutF31 <- summary(pool(with(data = as.mids(subsample_PGwithoutF31), lm(RAQ_Totalscore~1))))
mv_PGwithoutF31 <- descriptives_PGwithoutF31$estimate
n_PGwithoutF31 <- n_PG-n_F31
sd_PGwithoutF31 <- descriptives_PGwithoutF31$std.error*sqrt(n_PGwithoutF31)
var_PGwithoutF31 <- sd_PGwithoutF31 * sd_PGwithoutF31
# t-test
ttest.F31_restPG_raw <- with(data=mids_PG, exp=lm(RAQ_Totalscore~F31))
ttest.F31_restPG <- summary(pool(ttest.F31_restPG_raw))
# wilcoxon-test
RAQ_values_PGwithoutF31 <- subsample_PGwithoutF31$RAQ_Totalscore[subsample_PGwithoutF31$.imp==i]
Wilcox_pvalues_F31_restPG <- NULL
for(i in 1:5) {
  RAQ_values_F31 <- subsample_F31$RAQ_Totalscore[subsample_F31$.imp==i]
  RAQ_values_PGwithoutF31 <- subsample_PGwithoutF31$RAQ_Totalscore[subsample_PGwithoutF31$.imp==i]
  Wilcox_pvalues_F31_restPG <- c(Wilcox_pvalues_F31_restPG, wilcox.test(RAQ_values_PGwithoutF31, RAQ_values_F31)$p.value)}
# cohen's d
pooled_sd_F31_restPG <- sqrt(((n_PGwithoutF31-1)*var_PGwithoutF31+(n_F31-1)*sd_F31^2)/(n_PGwithoutF31+n_F31-2)) 
cd_F31_restPG <- (-mv_PGwithoutF31+mv_F31)/pooled_sd_F31_restPG

## F32-24 vs CG
subsample_F32_to_34_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F32_to_34 == 1, ]
mids_F32_to_34_CG <- as.mids(subsample_F32_to_34_CG)
# t-test
ttest.F32_to_34_CG_raw <- with(data=mids_F32_to_34_CG, exp=lm(RAQ_Totalscore~F32_to_34))
ttest.F32_to_34_CG <- summary(pool(ttest.F32_to_34_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F32_to_34_CG <- NULL
for(i in 1:5) {
  RAQ_values_F32_to_34 <- subsample_F32_to_34$RAQ_Totalscore[subsample_F32_to_34$.imp==i]
  Wilcox_pvalues_F32_to_34_CG <- c(Wilcox_pvalues_F32_to_34_CG, wilcox.test(RAQ_values_CG, RAQ_values_F32_to_34)$p.value)}
# cohen's d
pooled_sd_F32_to_34_CG <- sqrt(((n_CG-1)*var_CG+(n_F32_to_34-1)*sd_F32_to_34^2)/(n_CG+n_F32_to_34-2)) 
cd_F32_to_34_CG <- (-mv_CG+mv_F32_to_34)/pooled_sd_F32_to_34_CG

## F40&41 vs CG
subsample_F40_41_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F40_41 == 1, ]
mids_F40_41_CG <- as.mids(subsample_F40_41_CG)
# t-test
ttest.F40_41_CG_raw <- with(data=mids_F40_41_CG, exp=lm(RAQ_Totalscore~F40_41))
ttest.F40_41_CG <- summary(pool(ttest.F40_41_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F40_41_CG <- NULL
for(i in 1:5) {
  RAQ_values_F40_41 <- subsample_F40_41$RAQ_Totalscore[subsample_F40_41$.imp==i]
  Wilcox_pvalues_F40_41_CG <- c(Wilcox_pvalues_F40_41_CG, wilcox.test(RAQ_values_CG, RAQ_values_F40_41)$p.value)}
# cohen's d
pooled_sd_F40_41_CG <- sqrt(((n_CG-1)*var_CG+(n_F40_41-1)*sd_F40_41^2)/(n_CG+n_F40_41-2)) 
cd_F40_41_CG <- (-mv_CG+mv_F40_41)/pooled_sd_F40_41_CG

## F42 vs CG
subsample_F42_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F42 == 1, ]
mids_F42_CG <- as.mids(subsample_F42_CG)
# t-test
ttest.F42_CG_raw <- with(data=mids_F42_CG, exp=lm(RAQ_Totalscore~F42))
ttest.F42_CG <- summary(pool(ttest.F42_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F42_CG <- NULL
for(i in 1:5) {
  RAQ_values_F42 <- subsample_F42$RAQ_Totalscore[subsample_F42$.imp==i]
  Wilcox_pvalues_F42_CG <- c(Wilcox_pvalues_F42_CG, wilcox.test(RAQ_values_CG, RAQ_values_F42)$p.value)}
# cohen's d
pooled_sd_F42_CG <- sqrt(((n_CG-1)*var_CG+(n_F42-1)*sd_F42^2)/(n_CG+n_F42-2)) 
cd_F42_CG <- (-mv_CG+mv_F42)/pooled_sd_F42_CG

## F43 vs CG
subsample_F43_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F43 == 1, ]
mids_F43_CG <- as.mids(subsample_F43_CG)
# t-test
ttest.F43_CG_raw <- with(data=mids_F43_CG, exp=lm(RAQ_Totalscore~F43))
ttest.F43_CG <- summary(pool(ttest.F43_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F43_CG <- NULL
for(i in 1:5) {
  RAQ_values_F43 <- subsample_F43$RAQ_Totalscore[subsample_F43$.imp==i]
  Wilcox_pvalues_F43_CG <- c(Wilcox_pvalues_F43_CG, wilcox.test(RAQ_values_CG, RAQ_values_F43)$p.value)}
# cohen's d
pooled_sd_F43_CG <- sqrt(((n_CG-1)*var_CG+(n_F43-1)*sd_F43^2)/(n_CG+n_F43-2)) 
cd_F43_CG <- (-mv_CG+mv_F43)/pooled_sd_F43_CG

## F44,45&48 vs CG
subsample_F44_45_48_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F44_45_48 == 1, ]
mids_F44_45_48_CG <- as.mids(subsample_F44_45_48_CG)
# t-test
ttest.F44_45_48_CG_raw <- with(data=mids_F44_45_48_CG, exp=lm(RAQ_Totalscore~F44_45_48))
ttest.F44_45_48_CG <- summary(pool(ttest.F44_45_48_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F44_45_48_CG <- NULL
for(i in 1:5) {
  RAQ_values_F44_45_48 <- subsample_F44_45_48$RAQ_Totalscore[subsample_F44_45_48$.imp==i]
  Wilcox_pvalues_F44_45_48_CG <- c(Wilcox_pvalues_F44_45_48_CG, wilcox.test(RAQ_values_CG, RAQ_values_F44_45_48)$p.value)}
# cohen's d
pooled_sd_F44_45_48_CG <- sqrt(((n_CG-1)*var_CG+(n_F44_45_48-1)*sd_F44_45_48^2)/(n_CG+n_F44_45_48-2)) 
cd_F44_45_48_CG <- (-mv_CG+mv_F44_45_48)/pooled_sd_F44_45_48_CG

## F50 vs CG
subsample_F50_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F50 == 1, ]
mids_F50_CG <- as.mids(subsample_F50_CG)
# t-test
ttest.F50_CG_raw <- with(data=mids_F50_CG, exp=lm(RAQ_Totalscore~F50))
ttest.F50_CG <- summary(pool(ttest.F50_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F50_CG <- NULL
for(i in 1:5) {
  RAQ_values_F50 <- subsample_F50$RAQ_Totalscore[subsample_F50$.imp==i]
  Wilcox_pvalues_F50_CG <- c(Wilcox_pvalues_F50_CG, wilcox.test(RAQ_values_CG, RAQ_values_F50)$p.value)}
# cohen's d
pooled_sd_F50_CG <- sqrt(((n_CG-1)*var_CG+(n_F50-1)*sd_F50^2)/(n_CG+n_F50-2)) 
cd_F50_CG <- (-mv_CG+mv_F50)/pooled_sd_F50_CG

## F54 vs CG
subsample_F54_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F54 == 1, ]
mids_F54_CG <- as.mids(subsample_F54_CG)
# t-test
ttest.F54_CG_raw <- with(data=mids_F54_CG, exp=lm(RAQ_Totalscore~F54))
ttest.F54_CG <- summary(pool(ttest.F54_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F54_CG <- NULL
for(i in 1:5) {
  RAQ_values_F54 <- subsample_F54$RAQ_Totalscore[subsample_F54$.imp==i]
  Wilcox_pvalues_F54_CG <- c(Wilcox_pvalues_F54_CG, wilcox.test(RAQ_values_CG, RAQ_values_F54)$p.value)}
# cohen's d
pooled_sd_F54_CG <- sqrt(((n_CG-1)*var_CG+(n_F54-1)*sd_F54^2)/(n_CG+n_F54-2)) 
cd_F54_CG <- (-mv_CG+mv_F54)/pooled_sd_F54_CG

## F60&F61 vs CG
subsample_F60_61_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F60_61 == 1, ]
mids_F60_61_CG <- as.mids(subsample_F60_61_CG)
# t-test
ttest.F60_61_CG_raw <- with(data=mids_F60_61_CG, exp=lm(RAQ_Totalscore~F60_61))
ttest.F60_61_CG <- summary(pool(ttest.F60_61_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F60_61_CG <- NULL
for(i in 1:5) {
  RAQ_values_F60_61 <- subsample_F60_61$RAQ_Totalscore[subsample_F60_61$.imp==i]
  Wilcox_pvalues_F60_61_CG <- c(Wilcox_pvalues_F60_61_CG, wilcox.test(RAQ_values_CG, RAQ_values_F60_61)$p.value)}
# cohen's d
pooled_sd_F60_61_CG <- sqrt(((n_CG-1)*var_CG+(n_F60_61-1)*sd_F60_61^2)/(n_CG+n_F60_61-2)) 
cd_F60_61_CG <- (-mv_CG+mv_F60_61)/pooled_sd_F60_61_CG

## F60.31 vs CG
subsample_F60.31_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F60.31 == 1, ]
mids_F60.31_CG <- as.mids(subsample_F60.31_CG)
# t-test
ttest.F60.31_CG_raw <- with(data=mids_F60.31_CG, exp=lm(RAQ_Totalscore~F60.31))
ttest.F60.31_CG <- summary(pool(ttest.F60.31_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F60.31_CG <- NULL
for(i in 1:5) {
  RAQ_values_F60.31 <- subsample_F60.31$RAQ_Totalscore[subsample_F60.31$.imp==i]
  Wilcox_pvalues_F60.31_CG <- c(Wilcox_pvalues_F60.31_CG, wilcox.test(RAQ_values_CG, RAQ_values_F60.31)$p.value)}
# cohen's d
pooled_sd_F60.31_CG <- sqrt(((n_CG-1)*var_CG+(n_F60.31-1)*sd_F60.31^2)/(n_CG+n_F60.31-2)) 
cd_F60.31_CG <- (-mv_CG+mv_F60.31)/pooled_sd_F60.31_CG

## F60.31 vs PG (PG without those with F60.31)
subsample_PGwithoutF60.31 <- subsample_PG[subsample_PG$F60.31==0, ]
descriptives_PGwithoutF60.31 <- summary(pool(with(data = as.mids(subsample_PGwithoutF60.31), lm(RAQ_Totalscore~1))))
mv_PGwithoutF60.31 <- descriptives_PGwithoutF60.31$estimate
n_PGwithoutF60.31 <- n_PG-n_F60.31
sd_PGwithoutF60.31 <- descriptives_PGwithoutF60.31$std.error*sqrt(n_PGwithoutF60.31)
var_PGwithoutF60.31 <- sd_PGwithoutF60.31 * sd_PGwithoutF60.31
# t-test
ttest.F60.31_restPG_raw <- with(data=mids_PG, exp=lm(RAQ_Totalscore~F60.31))
ttest.F60.31_restPG <- summary(pool(ttest.F60.31_restPG_raw))
# wilcoxon-test
RAQ_values_PGwithoutF60.31 <- subsample_PGwithoutF60.31$RAQ_Totalscore[subsample_PGwithoutF60.31$.imp==i]
Wilcox_pvalues_F60.31_restPG <- NULL
for(i in 1:5) {
  RAQ_values_F60.31 <- subsample_F60.31$RAQ_Totalscore[subsample_F60.31$.imp==i]
  RAQ_values_PGwithoutF60.31 <- subsample_PGwithoutF60.31$RAQ_Totalscore[subsample_PGwithoutF60.31$.imp==i]
  Wilcox_pvalues_F60.31_restPG <- c(Wilcox_pvalues_F60.31_restPG, wilcox.test(RAQ_values_PGwithoutF60.31, RAQ_values_F60.31)$p.value)}
# cohen's d
pooled_sd_F60.31_restPG <- sqrt(((n_PGwithoutF60.31-1)*var_PGwithoutF60.31+(n_F60.31-1)*sd_F60.31^2)/(n_PGwithoutF60.31+n_F60.31-2)) 
cd_F60.31_restPG <- (-mv_PGwithoutF60.31+mv_F60.31)/pooled_sd_F60.31_restPG

## cluster c PD vs CG
subsample_Cluster_C_PD_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$Cluster_C_PD == 1, ]
mids_Cluster_C_PD_CG <- as.mids(subsample_Cluster_C_PD_CG)
# t-test
ttest.Cluster_C_PD_CG_raw <- with(data=mids_Cluster_C_PD_CG, exp=lm(RAQ_Totalscore~Cluster_C_PD))
ttest.Cluster_C_PD_CG <- summary(pool(ttest.Cluster_C_PD_CG_raw))
# wilcoxon-test
Wilcox_pvalues_Cluster_C_PD_CG <- NULL
for(i in 1:5) {
  RAQ_values_Cluster_C_PD <- subsample_Cluster_C_PD$RAQ_Totalscore[subsample_Cluster_C_PD$.imp==i]
  Wilcox_pvalues_Cluster_C_PD_CG <- c(Wilcox_pvalues_Cluster_C_PD_CG, wilcox.test(RAQ_values_CG, RAQ_values_Cluster_C_PD)$p.value)}
# cohen's d
pooled_sd_Cluster_C_PD_CG <- sqrt(((n_CG-1)*var_CG+(n_Cluster_C_PD-1)*sd_Cluster_C_PD^2)/(n_CG+n_Cluster_C_PD-2)) 
cd_Cluster_C_PD_CG <- (-mv_CG+mv_Cluster_C_PD)/pooled_sd_Cluster_C_PD_CG

## F62 vs CG
subsample_F62_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F62 == 1, ]
mids_F62_CG <- as.mids(subsample_F62_CG)
# t-test
ttest.F62_CG_raw <- with(data=mids_F62_CG, exp=lm(RAQ_Totalscore~F62))
ttest.F62_CG <- summary(pool(ttest.F62_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F62_CG <- NULL
for(i in 1:5) {
  RAQ_values_F62 <- subsample_F62$RAQ_Totalscore[subsample_F62$.imp==i]
  Wilcox_pvalues_F62_CG <- c(Wilcox_pvalues_F62_CG, wilcox.test(RAQ_values_CG, RAQ_values_F62)$p.value)}
# cohen's d
pooled_sd_F62_CG <- sqrt(((n_CG-1)*var_CG+(n_F62-1)*sd_F62^2)/(n_CG+n_F62-2)) 
cd_F62_CG <- (-mv_CG+mv_F62)/pooled_sd_F62_CG

## F63 vs CG
subsample_F63_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F63 == 1, ]
mids_F63_CG <- as.mids(subsample_F63_CG)
# t-test
ttest.F63_CG_raw <- with(data=mids_F63_CG, exp=lm(RAQ_Totalscore~F63))
ttest.F63_CG <- summary(pool(ttest.F63_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F63_CG <- NULL
for(i in 1:5) {
  RAQ_values_F63 <- subsample_F63$RAQ_Totalscore[subsample_F63$.imp==i]
  Wilcox_pvalues_F63_CG <- c(Wilcox_pvalues_F63_CG, wilcox.test(RAQ_values_CG, RAQ_values_F63)$p.value)}
# cohen's d
pooled_sd_F63_CG <- sqrt(((n_CG-1)*var_CG+(n_F63-1)*sd_F63^2)/(n_CG+n_F63-2)) 
cd_F63_CG <- (-mv_CG+mv_F63)/pooled_sd_F63_CG

## F90.0 vs CG
subsample_F90.0_CG <- RAQ_df_complete[RAQ_df_complete$Sample == 0 | RAQ_df_complete$F90.0 == 1, ]
mids_F90.0_CG <- as.mids(subsample_F90.0_CG)
# t-test
ttest.F90.0_CG_raw <- with(data=mids_F90.0_CG, exp=lm(RAQ_Totalscore~F90.0))
ttest.F90.0_CG <- summary(pool(ttest.F90.0_CG_raw))
# wilcoxon-test
Wilcox_pvalues_F90.0_CG <- NULL
for(i in 1:5) {
  RAQ_values_F90.0 <- subsample_F90.0$RAQ_Totalscore[subsample_F90.0$.imp==i]
  Wilcox_pvalues_F90.0_CG <- c(Wilcox_pvalues_F90.0_CG, wilcox.test(RAQ_values_CG, RAQ_values_F90.0)$p.value)}
# cohen's d
pooled_sd_F90.0_CG <- sqrt(((n_CG-1)*var_CG+(n_F90.0-1)*sd_F90.0^2)/(n_CG+n_F90.0-2)) 
cd_F90.0_CG <- (-mv_CG+mv_F90.0)/pooled_sd_F90.0_CG

## F90.0 vs PG (PG without those with F90.0)
subsample_PGwithoutF90.0 <- subsample_PG[subsample_PG$F90.0==0, ]
descriptives_PGwithoutF90.0 <- summary(pool(with(data = as.mids(subsample_PGwithoutF90.0), lm(RAQ_Totalscore~1))))
mv_PGwithoutF90.0 <- descriptives_PGwithoutF90.0$estimate
n_PGwithoutF90.0 <- n_PG-n_F90.0
sd_PGwithoutF90.0 <- descriptives_PGwithoutF90.0$std.error*sqrt(n_PGwithoutF90.0)
var_PGwithoutF90.0 <- sd_PGwithoutF90.0 * sd_PGwithoutF90.0
# t-test
ttest.F90.0_restPG_raw <- with(data=mids_PG, exp=lm(RAQ_Totalscore~F90.0))
ttest.F90.0_restPG <- summary(pool(ttest.F90.0_restPG_raw))
# wilcoxon-test
RAQ_values_PGwithoutF90.0 <- subsample_PGwithoutF90.0$RAQ_Totalscore[subsample_PGwithoutF90.0$.imp==i]
Wilcox_pvalues_F90.0_restPG <- NULL
for(i in 1:5) {
  RAQ_values_F90.0 <- subsample_F90.0$RAQ_Totalscore[subsample_F90.0$.imp==i]
  RAQ_values_PGwithoutF90.0 <- subsample_PGwithoutF90.0$RAQ_Totalscore[subsample_PGwithoutF90.0$.imp==i]
  Wilcox_pvalues_F90.0_restPG <- c(Wilcox_pvalues_F90.0_restPG, wilcox.test(RAQ_values_PGwithoutF90.0, RAQ_values_F90.0)$p.value)}
# cohen's d
pooled_sd_F90.0_restPG <- sqrt(((n_PGwithoutF90.0-1)*var_PGwithoutF90.0+(n_F90.0-1)*sd_F90.0^2)/(n_PGwithoutF90.0+n_F90.0-2)) 
cd_F90.0_restPG <- (-mv_PGwithoutF90.0+mv_F90.0)/pooled_sd_F90.0_restPG



#-----------------------------------------------------------------------------------------------------------------------------
# data visualization via violin plots
#-----------------------------------------------------------------------------------------------------------------------------
library(haven)
library(dplyr)
library(plyr)
library(ggplot2)

## create a temporary data frame and replace the NAs by the mean of the five imputated values (ids with NAs in the RAQ: 1, 24, 29, 46, 47, 64, 92, 110, 133, 147, 149)
temp_df <- RAQ_df_complete
temp_df[temp_df$.imp==0 & temp_df$.id ==1, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==1, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==24, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==24, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==29, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==29, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==46, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==46, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==47, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==47, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==64, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==64, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==92, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==92, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==110, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==110, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==133, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==133, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==147, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==147, "RAQ_Totalscore"], na.rm = TRUE)
temp_df[temp_df$.imp==0 & temp_df$.id ==149, "RAQ_Totalscore"] <- mean(temp_df[temp_df$.id==149, "RAQ_Totalscore"], na.rm = TRUE)

# creating data frames. Each data frame includes only cases of the respective sample/diagnostic category 
CG <- temp_df[temp_df$Sample==0, ]
CG$diagnose <- c("diagnose")
CG$diagnose <- 0
PG <- temp_df[temp_df$Sample==1, ]
PG$diagnose <- c("diagnose")
PG$diagnose <- 1
TS <- temp_df[temp_df$Sample==2, ]
TS$diagnose <- c("diagnose")
TS$diagnose <- 95
F10 <- temp_df[temp_df$F10==1, ]
F10$diagnose <- c("diagnose")
F10$diagnose <- 10
F11_to_19 <- temp_df[temp_df$F11_to_19==1, ]
F11_to_19$diagnose <- c("diagnose")
F11_to_19$diagnose <- 11.19
F20_23 <- temp_df[temp_df$F20_23==1, ]
F20_23$diagnose <- c("diagnose")
F20_23$diagnose <- 20.23
F32_to_34 <- temp_df[temp_df$F32_to_34==1, ]
F32_to_34$diagnose <- c("diagnose")
F32_to_34$diagnose <- 32.34
F40_41 <- temp_df[temp_df$F40_41==1, ]
F40_41$diagnose <- c("diagnose")
F40_41$diagnose <- 40.41
F42 <- temp_df[temp_df$F42==1, ]
F42$diagnose <- c("diagnose")
F42$diagnose <- 42
F43 <- temp_df[temp_df$F43==1, ]
F43$diagnose <- c("diagnose")
F43$diagnose <- 43
F44_45_48 <- temp_df[temp_df$F44_45_48==1, ]
F44_45_48$diagnose <- c("diagnose")
F44_45_48$diagnose <- 44.4548
F50 <- temp_df[temp_df$F50==1, ]
F50$diagnose <- c("diagnose")
F50$diagnose <- 50
F60_61 <- temp_df[temp_df$F60_61==1, ]
F60_61$diagnose <- c("diagnose")
F60_61$diagnose <- 60.61
F60.31 <- temp_df[temp_df$F60.31==1, ]
F60.31$diagnose <- c("diagnose")
F60.31$diagnose <- 60.31
F90 <- temp_df[temp_df$F90.0==1, ]
F90$diagnose <- c("diagnose")
F90$diagnose <- 90.0

# merge data frames to one data frame for the violin plots
df_vp <- rbind(CG, PG, TS, F10, F11_to_19, F20_23, F32_to_34,F40_41, F42, F43, F44_45_48, F50, F60_61, F60.31, F90)
df_vp$diagnose <- as.factor(df_vp$diagnose)

# rename factors 
df_vp$diagnose <- revalue(df_vp$diagnose, c("0"="CG","1"="PG","95"="TS","10"="F10","11.19"="F11\n-F19","20.23"="F20\n&F23","32.34"="F32\n-F34","40.41"="F40\n&F41","42"="F42"))
df_vp$diagnose <- revalue(df_vp$diagnose, c("43"="F43","44.4548"="F44,\n F45&F48","50"="F50","60.61"="F60\n&F61","60.31"="F60.31","90"="F90.0"))

# arranging violin plots according to their median
df_vp$diagnostic_category_or_sample <- factor(df_vp$diagnose,  levels = levels(df_vp$diagnose)[c(1,5,4,6,7,2,8,3,10,15,9,11,13,12,14)])

# build violin plots 

vp <- ggplot(df_vp, aes(x=diagnostic_category_or_sample, y=RAQ_Totalscore)) + geom_violin(size=0.8, aes(fill=diagnostic_category_or_sample), color="black") +
  scale_fill_manual(values=c("#1649BF", "#56B4E9", "#56B4E9", "#56B4E9","#56B4E9","#1649BF","#56B4E9", "#56B4E9", "#56B4E9", "#1649BF", "#56B4E9", "#56B4E9", "#56B4E9", "#56B4E9", "#56B4E9")) +
  theme(legend.position="none") +
  labs(x="Diagnostic category or sample", y="RAQ-R Score") +
  geom_boxplot(width=0.1) 
vp
#ggsave(filename="vp.tiff", plot=vp1, path = "~/Desktop", width=180, height=155, units="mm", dpi=300)


#-----------------------------------------------------------------------------------------------------------------------------
# multiple regression I
# does the difference in the RAQ-R scores between PG and CG stay significant after including sociodempgraphic characteristics as potential confounders?
# dependent variable: RAQ-R score
# independet variables: sample, gender (nominal)
#                       level of education and age groups (ordinal scaled -> treated as dummies), 
#-----------------------------------------------------------------------------------------------------------------------------

reg_PG_CG <- subsample_PG_CG
reg_PG_CG$Sample <- factor(reg_PG_CG$Sample)
reg_PG_CG$Gender <- factor(reg_PG_CG$Gender)
reg_PG_CG$Age_groups <- factor(reg_PG_CG$Age_groups)
reg_PG_CG$Level_of_education <- factor(reg_PG_CG$Level_of_education)

reg_with_PG_CG <- with(data=as.mids(reg_PG_CG), exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
str(summary(pool(reg_with_PG_CG)))
pooled_reg_PG_CG <- summary(pool(reg_with_PG_CG))
# check for significant values
pooled_reg_PG_CG$p.value
# sample (p<0.001) and level_of_education5(university degree) (p0.02) are significant p<0.05
# check for estimates
pooled_reg_PG_CG$estimate

# scaling RAQ-R Score
reg_with_PG_CG_scaled <- with(data=as.mids(reg_PG_CG), exp=lm(scale(RAQ_Totalscore)~Sample+Gender+Age_groups+Level_of_education))
str(summary(pool(reg_with_PG_CG_scaled)))
pooled_reg_PG_CG_scaled <- summary(pool(reg_with_PG_CG_scaled))
pooled_reg_PG_CG_scaled$p.value
pooled_reg_PG_CG_scaled$estimate

## regression for each iteration in order to check diagnostic plots
# look for differences between the adjusted R-squared of the five iterations
reg_PG_CG_1 <- reg_PG_CG[reg_PG_CG$.imp == 1, ]
summary(lm(data=reg_PG_CG_1, RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
reg_with_PG_CG_1 <- with(data=reg_PG_CG_1, exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
plot(reg_with_PG_CG_1)

reg_PG_CG_2 <- reg_PG_CG[reg_PG_CG$.imp == 2, ]
summary(lm(data=reg_PG_CG_2, RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
reg_with_PG_CG_2 <- with(data=reg_PG_CG_2, exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
plot(reg_with_PG_CG_2)

reg_PG_CG_3 <- reg_PG_CG[reg_PG_CG$.imp == 3, ]
summary(lm(data=reg_PG_CG_3, RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
reg_with_PG_CG_3 <- with(data=reg_PG_CG_3, exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
plot(reg_with_PG_CG_3)

reg_PG_CG_4 <- reg_PG_CG[reg_PG_CG$.imp == 4, ]
summary(lm(data=reg_PG_CG_4, RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
reg_with_PG_CG_4 <- with(data=reg_PG_CG_4, exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
plot(reg_with_PG_CG_4)

reg_PG_CG_5 <- reg_PG_CG[reg_PG_CG$.imp == 5, ]
summary(lm(data=reg_PG_CG_5, RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
reg_with_PG_CG_5 <- with(data=reg_PG_CG_5, exp=lm(RAQ_Totalscore~Sample+Gender+Age_groups+Level_of_education))
plot(reg_with_PG_CG_5)

#-----------------------------------------------------------------------------------------------------------------------------
# differences in the RAQ-R scores within the PG regarding sociodemographic characteristics
# pearson’s r for age as an interval variable,
# independent-samples t-tests and Wilcoxon rank-sum tests for dichotomous variables (gender, country of birth Germany or not Germany)
# and ANOVA for ordinal variables (level of education, patient status).
#-----------------------------------------------------------------------------------------------------------------------------

## age (in years)
r_age <- (with(subsample_PG, cor(RAQ_Totalscore[.imp==1], Age_years[.imp==1]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==2], Age_years[.imp==2]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==3], Age_years[.imp==3]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==4], Age_years[.imp==4]))+
            with(subsample_PG, cor(RAQ_Totalscore[.imp==5], Age_years[.imp==5])))/5
p_age <- pt((r_age*sqrt(nrow(subsample_PG)/6-2)/sqrt(1-r_age)), nrow(subsample_PG)/6-2)

## gender
subsample_PG_male <- subsample_PG[subsample_PG$Gender == 1, ]
descriptives_PG_male <- summary(pool(with(data = as.mids(subsample_PG_male), lm(RAQ_Totalscore~1))))
mv_PG_male <- descriptives_PG_male$estimate
sd_PG_male <- descriptives_PG_male$std.error*sqrt(n_PG_male)
subsample_PG_female <- subsample_PG[subsample_PG$Gender == 0, ]
descriptives_PG_female <- summary(pool(with(data = as.mids(subsample_PG_female), lm(RAQ_Totalscore~1))))
mv_PG_female <- descriptives_PG_female$estimate
sd_PG_female <- descriptives_PG_female$std.error*sqrt(n_PG_female)
mids_PG <- as.mids(subsample_PG)
# t-test
ttest.gender_raw <- with(data=mids_PG, exp=lm(RAQ_Totalscore~Gender))
ttest.gender <- summary(pool(ttest.gender_raw))
# wilcoxon-test
Wilcox_pvalues_gender <- NULL
for(i in 1:5) {
  RAQ_values_PG_male <- subsample_PG_male$RAQ_Totalscore[subsample_PG_male$.imp==i]
  RAQ_values_PG_female <- subsample_PG_female$RAQ_Totalscore[subsample_PG_female$.imp==i]
  Wilcox_pvalues_gender <- c(Wilcox_pvalues_gender, wilcox.test(RAQ_values_PG_male, RAQ_values_PG_female)$p.value)}
# cohen's d 
pooled_sd_gender <- sqrt(((n_PG_male-1)*sd_PG_male^2+(n_PG_female-1)*sd_PG_female^2)/(n_PG_male+n_PG_female-2)) 
cd_gender <- (-mv_PG_male+mv_PG_female)/pooled_sd_gender

## country of birth
subsample_PG_Germany <- subsample_PG[subsample_PG$Country_of_birth_Germany == 1, ]
descriptives_PG_Germany <- summary(pool(with(data = as.mids(subsample_PG_Germany), lm(RAQ_Totalscore~1))))
mv_PG_Germany <- descriptives_PG_Germany$estimate
sd_PG_Germany <- descriptives_PG_Germany$std.error*sqrt(n_PG_Germany)
subsample_PG_not_Germany <- subsample_PG[subsample_PG$Country_of_birth_Germany == 0, ]
descriptives_PG_not_Germany <- summary(pool(with(data = as.mids(subsample_PG_not_Germany), lm(RAQ_Totalscore~1))))
mv_PG_not_Germany <- descriptives_PG_not_Germany$estimate
sd_PG_not_Germany <- descriptives_PG_not_Germany$std.error*sqrt(n_PG_not_Germany)
#t-test
ttest.birth_raw <- with(data=mids_PG, exp=lm(RAQ_Totalscore~Country_of_birth_Germany))
ttest.birth <- summary(pool(ttest.birth_raw))
#Wilcoxon-Test
Wilcox_pvalues_birth <- NULL
for(i in 1:5) {
  RAQ_values_PG_Germany <- subsample_PG_Germany$RAQ_Totalscore[subsample_PG_Germany$.imp==i]
  RAQ_values_PG_not_Germany <- subsample_PG_not_Germany$RAQ_Totalscore[subsample_PG_not_Germany$.imp==i]
  Wilcox_pvalues_birth <- c(Wilcox_pvalues_birth, wilcox.test(RAQ_values_PG_Germany, RAQ_values_PG_not_Germany)$p.value)}
#cohen's d 
pooled_sd_birth <- sqrt(((n_PG_Germany-1)*sd_PG_Germany^2+(n_PG_not_Germany-1)*sd_PG_not_Germany^2)/(n_PG_Germany+n_PG_not_Germany-2)) 
cd_birth <- (-mv_PG_Germany+mv_PG_not_Germany)/pooled_sd_birth

# level of education
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==1] ~ Level_of_education[.imp==1])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==2] ~ Level_of_education[.imp==2])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==3] ~ Level_of_education[.imp==3])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==4] ~ Level_of_education[.imp==4])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==5] ~ Level_of_education[.imp==5])))

# patient status
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==1] ~ Patient_status[.imp==1])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==2] ~ Patient_status[.imp==2])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==3] ~ Patient_status[.imp==3])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==4] ~ Patient_status[.imp==4])))
anova(with(subsample_PG, aov(RAQ_Totalscore[.imp==5] ~ Patient_status[.imp==5])))

#-----------------------------------------------------------------------------------------------------------------------------
# predictors of rage attacks
# multiple regression II
# dependent variable: RAQ-R score
# independet variables: all questionnaire scores (besides totalscores which are explained by their subscales: BSI_GSI, BIS15_Totalscore and HSRQ Totalscore), 
#                       age (discrete), gender, level of education, country of birth, Patient status and 
#                       all diagnoses with n>10 (besides subgroups of PD to avoid redundandance) (dummies)
# strategy: selecting predictors via foreward and backward elimination, majority and wald test
#-----------------------------------------------------------------------------------------------------------------------------
library(plm)

# selecting predictors/variables via a stepwise linear model to predict RAQ_Totalscore separately for each of the imputed datasets
imp_reg_PG <- as.mids(subsample_PG)
scope <- list(upper = ~ QoL+BSI_Somatization+BSI_Obsessive_Compulsion+BSI_Interpersonal_Sensitivity+BSI_Depression+BSI_Anxiety+BSI_Aggression_Hostility+BSI_Phobic_Anxiety+BSI_Paranoid_Ideation+BSI_Psychoticism+BIS15_Attentional+BIS15_Motor+BIS15_Non_Planning+I8_Urgence+I8_Risk_Taking+I8_Endurance+I8_Intention+ADHSSB_Totalscore+BAI_Totalscore+BDIII_Totalscore+HSRQ_Affectdiff_tol+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Selfesteem+HSRQ_Identity_Disturbances+HSRQ_Interpersonal_Disturbances+Gender+Age_years+Level_of_education+Country_of_birth_Germany+Patient_status+F10+F11_to_19+F32_to_34+F40_41+F43+F44_45_48+F50+F60_61+F90.0,
              lower = ~1)
expr <- expression(f1 <- lm(RAQ_Totalscore ~ 1),
                   f2 <- step(f1, scope = scope))
fit <- with(imp_reg_PG, expr)

# how many times was each variable selected? 
formulas <- lapply(fit$analyses, formula)
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

# regression with only variables retaining in the regression of all 5 imputated data sets
fit.without <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
summary(fit.without)

## wald test to determine whether variables retaining in less than 5 regressions should be in the final model
# note: if there is no significant difference between the models the variable is not needed in the final model
fit.with_I8_urgence <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+I8_Urgence))
D1(fit.with_I8_urgence, fit.without)

fit.with_BIS15_Motor <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+BIS15_Motor))
D1(fit.with_BIS15_Motor, fit.without)

fit.with_BSI_Paranoid_Ideation <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+BSI_Paranoid_Ideation))
D1(fit.with_BSI_Paranoid_Ideation, fit.without)

fit.with_BSI_Obsessive_Compulsion <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+BSI_Obsessive_Compulsion))
D1(fit.with_BSI_Obsessive_Compulsion, fit.without)

fit.with_BSI_Phobic_Anxiety <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+BSI_Phobic_Anxiety))
D1(fit.with_BSI_Phobic_Anxiety, fit.without)

fit.with_HSRQ_Affectdiff_tol <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+HSRQ_Affectdiff_tol))
D1(fit.with_HSRQ_Affectdiff_tol, fit.without)

fit.with_HSRQ_Interpersonal_Disturbances <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+HSRQ_Interpersonal_Disturbances))
D1(fit.with_HSRQ_Interpersonal_Disturbances, fit.without)

fit.with_Level_of_education <- with(imp_reg_PG, lm(RAQ_Totalscore ~ BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34+Level_of_education))
D1(fit.with_Level_of_education , fit.without)

# no significant differences were found between all other variables. So, the final model contains the five variables BSI_Aggression_Hostility, HSRQ_Frustrtol_Impulsecontrol, HSRQ_Identity_Disturbances, F90.0 and F32-F34.
final_model_RAQ <- fit.without

# pooled regression with variables left 
reg_final_model_RAQ <- with(data=imp_reg_PG, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
summary(pool(reg_final_model_RAQ))

# scaling
reg_final_model_RAQ_scaled <- with(data=imp_reg_PG, exp=lm(scale(RAQ_Totalscore)~scale(BSI_Aggression_Hostility)+scale(HSRQ_Frustrtol_Impulsecontrol)+scale(HSRQ_Identity_Disturbances)+F90.0+F32_to_34))
summary(pool(reg_final_model_RAQ_scaled))

## regression for each iteration in order to check diagnostic plots
reg_PG <- subsample_PG
reg_PG_1 <- reg_PG[reg_PG$.imp == 1, ]
summary(lm(data=reg_PG_1, RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
reg_with_PG_1 <- with(data=reg_PG_1, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
plot(reg_with_PG_1)

reg_PG_2 <- reg_PG[reg_PG$.imp == 2, ]
summary(lm(data=reg_PG_2, RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
reg_with_PG_2 <- with(data=reg_PG_2, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
plot(reg_with_PG_2)

reg_PG_3 <- reg_PG[reg_PG$.imp == 3, ]
summary(lm(data=reg_PG_3, RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
reg_with_PG_3 <- with(data=reg_PG_3, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
plot(reg_with_PG_3)

reg_PG_4 <- reg_PG[reg_PG$.imp == 4, ]
summary(lm(data=reg_PG_4, RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
reg_with_PG_4 <- with(data=reg_PG_4, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
plot(reg_with_PG_4)

reg_PG_5 <- reg_PG[reg_PG$.imp == 5, ]
summary(lm(data=reg_PG_5, RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
reg_with_PG_5 <- with(data=reg_PG_5, exp=lm(RAQ_Totalscore~BSI_Aggression_Hostility+HSRQ_Frustrtol_Impulsecontrol+HSRQ_Identity_Disturbances+F90.0+F32_to_34))
plot(reg_with_PG_5)

# calculating the mean of the five residual standard errors, R squared, adjusted R-squared and F statistic
Residual_standard_error_reg_PG <- (10.35+10.45+10.16+10.29+10.28)/5
R_squared_reg_PG <- (0.639+0.630+0.652+0.642+0.642)/5
R_squared_adjusted_reg_PG <-(0.6267+0.6175+0.6403+0.6301+0.6298)/5
F_statistic_reg_PG <- (53.04+51.05+56.18+53.81+53.73)/5