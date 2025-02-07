rm(list=ls(all=TRUE)) # Clear the memory of variables from previous run. 
cat("\f") # Clear console when working in RStudio # Control + Shift + F10 = Restart R

# -------------------------------- load-packages -----------------------------------------------------------
library(haven)
library(car)
library(psych)
library(GPArotation)
library(dplyr)
library(epiDisplay)
library(fabricatr)
library(fastDummies)
library(finalfit)
library(gmodels)
library(Hmisc)
library(lavaan)
library(moments)
library(naniar)
library(reshape)
library(semPlot)
library(stats)
library(tidyverse)
library(usmap)
library(ggplot2)
library(visdat)
library(gridExtra)
library(cowplot)

# -------------------------------- declare-globals -------------------------------------------------------

# path name
path_input = "./data/derived/2-recoded/ds_recoded.csv"

# ------------------------------- load-data ---------------------------------------------------------------

ds_teen = read.csv(path_input) |> 
  tibble::as_tibble()

dim(ds_teen)
colnames(ds_teen)


# --------------------------------- measures ---------------------------------------------------------------

# DV
# BB Victimization, 6 items, T1:T4

# IVs at Baseline
# Age
# Gender Identity (2 variables: sex assigned at birth, current gender identity)
# Race/Ethnicity (2 variables: race, ethnicity)
# Sexual orientation
# SES
# Vaux Social Support, 3-item scale
# MSPSS, 4-item subscale family support
# School climate, 8-item student support scale
# Hate crimes laws - protections index
# Enumerated antibullying laws (external)


# ------------------------------------- PREP SOCIODEMOGRAPHIC CHARACTERISTICS --------------------------------------------

# Hispanic recode
# get levels of race variable
epiDisplay::tab1(ds_teen$Hispanic_1, main = "Hispanic", cum.percent = TRUE) # NOTE Hispanic is NOT check all that apply 
ds_teen$Hispanic_1
ftable(ds_teen$Hispanic_1)

ds_teen <- ds_teen |> dplyr::mutate(
  HispanicD_1 = (if_else(Hispanic_1 == 1, 0, 1))
)

ftable(ds_teen$HispanicD_1)

# make into factor and add label levels 
ds_teen$HispanicD_cat_1 <- factor(ds_teen$HispanicD_1,
                                    levels = c(0,1),
                                    labels = c("NOT_HISPANIC", "HISPANIC"))


# make into factor and add label levels 
ds_teen$Hispanic_1 <- factor(ds_teen$Hispanic_1,
                             levels = c(1,2,3,4,5,6,7,8),
                             labels = c("NOT_HISPANIC", "MEXICAN", "PUERTO_RICAN", "CUBAN", "CENTRAL_AM", "SOUTH_AM", "CARIBBEAN", "OTHER_HISPANIC"))

# look at variable again to make sure it is a factor
ds_teen$Hispanic_1
ftable(ds_teen$Hispanic_1)

# create a vector of column names that you want to create dummy variables with use c() for multiple columns
NEW_HISPANIC <- "Hispanic_1"

# create and append new dummy variables - ds1 apends five new dummy variables based on the levels of RACE_RC to end of dataset
ds_teen <- ds_teen %>% fastDummies::dummy_cols(select_columns = NEW_HISPANIC, ignore_na = TRUE)
# the Asian and Black variables are not vaild names because the have a space and asian has a "/". 
# This corrects the issue so we can alter there name. Notice periods have replaced the space and "/" 
names(ds_teen) <- make.names(names(ds_teen), unique = TRUE)
ds_teen %>% names()

epiDisplay::tab1(ds_teen$Hispanic_1_NOT_HISPANIC, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_MEXICAN, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_PUERTO_RICAN, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_CUBAN, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_CENTRAL_AM, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_SOUTH_AM, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_CARIBBEAN, cum.percent = TRUE, missing = FALSE)
epiDisplay::tab1(ds_teen$Hispanic_1_OTHER_HISPANIC, cum.percent = TRUE, missing = FALSE)

# rename Hispanic dummy variables

ds_teen <- ds_teen |> dplyr::rename(
  Not_Hispanic_1   = Hispanic_1_NOT_HISPANIC,
  Mexican_1        = Hispanic_1_MEXICAN,
  Puerto_Rican_1   = Hispanic_1_PUERTO_RICAN,
  Cuban_1          = Hispanic_1_CUBAN,
  Central_AM_1     = Hispanic_1_CENTRAL_AM,
  South_AM_1       = Hispanic_1_SOUTH_AM,
  Cariabean_1      = Hispanic_1_CARIBBEAN,
  Other_Hispanic_1 = Hispanic_1_OTHER_HISPANIC
  
)



# ------------------------------------- check all that apply variables -------------------------------------

# race
epiDisplay::tab1(ds_teen$White_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Black_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Native_Ind_Alas_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Asian_Indian_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Chinese_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Filipino_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Japanese_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Korean_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Vietnamese_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_Asian_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Native_Hawaiian_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Guamanian_Chamo_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Samoan_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_Pacific_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_race_1, cum.percent = TRUE)


# select variables
ds_race <- ds_teen |> 
  dplyr::select(ID, White_1, Black_1, Native_Ind_Alas_1, Asian_Indian_1, Chinese_1, Filipino_1, Japanese_1, Korean_1,
                Vietnamese_1, Other_Asian_1, Native_Hawaiian_1, Guamanian_Chamo_1, Samoan_1, Other_Pacific_1, Other_race_1)

# remove "_prs" from column names
names(ds_race) <- gsub("_1", "", names(ds_race), fixed = TRUE)

ds_race1 <- ds_race |>
  pivot_longer(!ID, names_to = "race", values_to = "count") |> # make data long by the categories
  mutate(race1 = ifelse(count == 1, race, NA)) |>  # place actual name of the catefory in column based on count column in new column called race1
  subset(select = -count) |>  # remove count column beause we dont need it anymore
  pivot_wider(names_from = race, values_from = race1) |> # turn data back wide by the categories
  unite(race_new, White:Other_race, sep='_', na.rm = TRUE) # collapse the columns, separate words with "_", and remove NAs

epiDisplay::tab1(ds_race1$race_new,    main = "Race", cum.percent = TRUE)  

# join race data with original data using ResponseId         
ds_teen <- left_join(ds_teen, ds_race1, by = "ID")
names(ds_teen)

epiDisplay::tab1(ds_teen$race_new,    main = "Race", cum.percent = TRUE)  

# create data frame to count the number of people in each category 
ds_race_grp <- ds_race1 |> 
  group_by(race_new) |>
  summarise(race_count = length(race_new))

# Gender Identity

epiDisplay::tab1(ds_teen$Male_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Female_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Non_Binary_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Trans_Male_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Trans_Female_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Gender_Queer_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_Gender_1, cum.percent = TRUE)

# select variables
ds_gender <- ds_teen |> 
  dplyr::select(ID, Male_1, Female_1, Non_Binary_1, Trans_Male_1, Trans_Female_1, Gender_Queer_1, Other_Gender_1)

# remove "_prs" from column names
names(ds_gender) <- gsub("_1", "", names(ds_gender), fixed = TRUE)

ds_gender1 <- ds_gender |>
  pivot_longer(!ID, names_to = "gender", values_to = "count") |> # make data long by the categories
  mutate(gender1 = ifelse(count == 1, gender, NA)) |>  # place actual name of the category in column based on count column in new column called gender1
  subset(select = -count) |>  # remove count column beause we dont need it anymore
  pivot_wider(names_from = gender, values_from = gender1) |> # turn data back wide by the categories
  unite(gender_new, Male:Other_Gender, sep='_', na.rm = TRUE) # collapse the columns, separate words with "_", and remove NAs

epiDisplay::tab1(ds_gender1$gender_new,    main = "Gender", cum.percent = TRUE) 

# create data frame to count the number of people in each category 
ds_gender_grp <- ds_gender1 |> 
  group_by(gender_new) |>
  summarise(gender_count = length(gender_new))

# join gender data with original data using ResponseId         
ds_teen <- left_join(ds_teen, ds_gender1, by = "ID")
names(ds_teen)

# Sexual Orientation 

epiDisplay::tab1(ds_teen$Bisexual_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Demisexual_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Gay_Les_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Pansexual_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Queer_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Straight_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Questioning_1, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_SexOr_1, cum.percent = TRUE)

# select variables
ds_sex_or <- ds_teen |> 
  dplyr::select(ID, Bisexual_1, Demisexual_1, Gay_Les_1, Pansexual_1, Queer_1, Straight_1, Questioning_1, Other_SexOr_1)

# remove "_prs" from column names
names(ds_sex_or) <- gsub("_1", "", names(ds_sex_or), fixed = TRUE)

ds_sex_or1 <- ds_sex_or |>
  pivot_longer(!ID, names_to = "sex_or", values_to = "count") |> # make data long by the categories
  mutate(sex_or1 = ifelse(count == 1, sex_or, NA)) |>  # place actual name of the category in the column based on count column in new column called sex_or1
  subset(select = -count) |>  # remove count column beause we dont need it anymore
  pivot_wider(names_from = sex_or, values_from = sex_or1) |> # turn data back wide by the categories
  unite(sex_or_new, Bisexual:Other_SexOr, sep='_', na.rm = TRUE) # collapse the columns, separate words with "_", and remove NAs


epiDisplay::tab1(ds_sex_or1$sex_or_new, main = "Sexual Orientation", cum.percent = TRUE) 

# create data frame to count the number of people in each category 
ds_sex_or_grp <- ds_sex_or1 |> 
  group_by(sex_or_new) |>
  summarise(sex_or_count = length(sex_or_new))

# join sex orient data with original data using ResponseId         
ds_teen <- left_join(ds_teen, ds_sex_or1, by = "ID")
names(ds_teen)



# ---- combining-demographic-categories -----------------------------------------------------------------
# combining race
ds_teen <- ds_teen %>%
  dplyr::mutate(
    race_cat_1 = case_when(
      race_new             == "Black" ~ 1,
      race_new             == "White" ~ 2,
      race_new             == "Chinese" ~ 3,
      race_new             == "Chinese_Japanese" ~ 3,
      race_new             == "Chinese_Other_Asian" ~ 3,
      race_new             == "Filipino" ~ 3,
      race_new             == "Japanese" ~ 3,
      race_new             == "Korean" ~ 3,
      race_new             == "Other_Asian" ~ 3,
      race_new             == "Vietnamese" ~ 3,
      race_new             == "White_Black" ~ 4,
      race_new             == "Other_race" ~ 5,
      race_new             == "Native_Ind_Alas" ~ 6,
      race_new             == "Asian_Indian" ~ 5,
      race_new             == "White_Chinese" ~ 4,
      race_new             == "White_Filipino" ~ 4,
      race_new             == "White_Filipino_Guamanian_Chamo" ~ 4,
      race_new             == "White_Japanese" ~ 4,
      race_new             == "White_Korean" ~ 4,
      race_new             == "White_Native_Ind_Alas" ~ 4,
      race_new             == "White_Native_Ind_Alas_Other_Pacific" ~ 4,
      race_new             == "White_Other_Asian" ~ 4,
      race_new             == "White_Other_Asian_Other_Pacific_Other_race" ~ 4,
      race_new             == "White_Other_race" ~ 4,
      race_new             == "White_Samoan" ~ 4,
      race_new             == "Black_Asian_Indian_Native_Hawaiian" ~ 4,
      race_new             == "Black_Filipino" ~ 4,
      race_new             == "Black_Japanese_Native_Hawaiian" ~ 4,
      race_new             == "Black_Native_Ind_Alas" ~ 4,
      race_new             == "Black_Other_Asian" ~ 4,
      race_new             == "Black_Other_Pacific" ~ 4,
      race_new             == "Black_Other_race" ~ 4,
      race_new             == "White_Black_Asian_Indian_Chinese_Japanese" ~ 4,
      race_new             == "White_Black_Japanese" ~ 4,
      race_new             == "White_Black_Native_Ind_Alas_Other_Asian_Other_race" ~ 4,
      race_new             == "White_Black_Native_Ind_Alas_Other_race" ~ 4,
      race_new             == "White_Black_Other_race" ~ 4,
      race_new             == "White_Black_Native_Ind_Alas" ~ 4
    ))

ds_teen$race_cat_1 <- factor(ds_teen$race_cat_1,
                             levels = c(1,2,3,4,5,6),
                             labels = c("Black", "White", "Asian", "Multiracial", "Other_Race",
                                        "American_Indian_Alaska_Native"))


epiDisplay::tab1(ds_teen$race_cat_1, cum.percent = TRUE)

### cross-tabulation of race and ethnicity
xtab_re <- table(ds_teen$HispanicD_cat_1, ds_teen$race_cat_1, exclude = NULL)
addmargins(xtab_re)


### new measure of race/ethnicity based on cross-tabulation
ds_teen$race_ethn <- NA
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "White"] <- 1  # White alone, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "Black"] <- 2  # Black alone, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "Asian"] <- 3  # Another racial identity, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "Multiracial"] <- 3  # Another racial identity, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "Other_Race"] <- 3  # Another racial identity, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "NOT_HISPANIC" & ds_teen$race_cat_1 == "American_Indian_Alaska_Native"] <- 3  # Another racial identity, non-Hispanic
ds_teen$race_ethn[ds_teen$HispanicD_cat_1 == "HISPANIC"] <- 4  # Hispanic



table(ds_teen$race_ethn, exclude = NULL) # check #s


ds_teen$race_ethn <- factor(ds_teen$race_ethn,
                               levels = c(1,2,3,4),
                               labels = c("White_NH", "Black_NH", "Another_NH", "Hispanic"))                   

epiDisplay::tab1(ds_teen$race_ethn, cum.percent = TRUE)


#combining gender
ds_teen <- ds_teen %>%
  dplyr::mutate(
    gender_cat_1 = case_when(
      gender_new             == "Female" ~ 1,
      gender_new             == "Male" ~ 2,
      gender_new             == "Male_Trans_Female" ~ 3,
      gender_new             == "Male_Trans_Male" ~ 3,
      gender_new             == "Trans_Female" ~ 3,
      gender_new             == "Trans_Male" ~ 3,
      gender_new             == "Female_Non_Binary" ~ 3,
      gender_new             == "Male_Non_Binary" ~ 3,
      gender_new             == "Non_Binary" ~ 3,
      gender_new             == "Non_Binary_Gender_Queer" ~ 3,
      gender_new             == "Other_Gender" ~ 3,
      gender_new             == "Gender_Queer" ~ 3,
      gender_new             == "Male_Female_Non_Binary_Gender_Queer" ~ 3
      
    ))

ds_teen$gender_cat_1 <- factor(ds_teen$gender_cat_1,
                               levels = c(1,2,3),
                               labels = c("Female", "Male", "Another"))                   

epiDisplay::tab1(ds_teen$gender_cat_1, cum.percent = TRUE)


### UPDATE to cross-classification of sex assigned at birth and gender identity, 2-step measure
# crosstab of ASAB (1=Male, 2=Female) and gender ID
xtab_gid <- table(ds_teen$Sex_1, ds_teen$gender_new)
addmargins(xtab_gid)

# crosstab of ASAB (1=Male, 2=Female) and gender_cat_1
xtab_gid_cat <- table(ds_teen$Sex_1, ds_teen$gender_cat_1)
addmargins(xtab_gid_cat)

### new measure of gender ID based on cross-classification
ds_teen$gender_2step <- NA
ds_teen$gender_2step[ds_teen$Sex_1 == 1 & ds_teen$gender_cat_1 == "Male"] <- 1
ds_teen$gender_2step[ds_teen$Sex_1 == 2 & ds_teen$gender_cat_1 == "Female"] <- 2
ds_teen$gender_2step[ds_teen$Sex_1 == 1 & ds_teen$gender_cat_1 == "Another"] <- 3
ds_teen$gender_2step[ds_teen$Sex_1 == 2 & ds_teen$gender_cat_1 == "Another"] <- 3
ds_teen$gender_2step[ds_teen$Sex_1 == 1 & ds_teen$gender_cat_1 == "Female"] <- 3  # 1 respondent reported Male ASAB and Female Gender ID

table(ds_teen$gender_2step, exclude = NULL) # check #s


ds_teen$gender_2step <- factor(ds_teen$gender_2step,
                               levels = c(1,2,3),
                               labels = c("Male", "Female", "Gender Diverse"))                   

epiDisplay::tab1(ds_teen$gender_2step, cum.percent = TRUE)




# combining sexual orientation 
ds_teen <- ds_teen %>%
  dplyr::mutate(
    sex_or_cat_1 = case_when(
      sex_or_new             == "Straight" ~ 1,
      sex_or_new             == "Bisexual" ~ 2,
      sex_or_new             == "Gay_Les" ~ 3,
      sex_or_new             == "Pansexual" ~ 4,
      sex_or_new             == "Questioning" ~ 5,
      sex_or_new             == "Queer" ~ 6,
      sex_or_new             == "Demisexual" ~ 6,
      sex_or_new             == "Other_SexOr" ~ 6,
      sex_or_new             == "Queer_Straight" ~ 6,
      sex_or_new             == "Bisexual_Demisexual_Pansexual" ~ 6,
      sex_or_new             == "Bisexual_Gay_Les" ~ 6,
      sex_or_new             == "Bisexual_Gay_Les_Queer_Other_SexOr" ~ 6,
      sex_or_new             == "Bisexual_Pansexual_Queer" ~ 6,
      sex_or_new             == "Bisexual_Queer" ~ 6,
      sex_or_new             == "Bisexual_Straight" ~ 6,
      sex_or_new             == "Demisexual_Gay_Les" ~ 6,
      sex_or_new             == "Demisexual_Pansexual" ~ 6
    ))

ds_teen$sex_or_cat_1 <- factor(ds_teen$sex_or_cat_1,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Straight", "Bisexual", "Gay_Les",
                                          "Pansexual", "Questioning", "Another"))                                



# sexual orientation recode
ds_teen <- ds_teen %>%
  dplyr::mutate(
    sex_or_cat_R_1 = case_when(
      sex_or_new             == "Straight" ~ 1,
      sex_or_new             == "Bisexual" ~ 2,
      sex_or_new             == "Gay_Les" ~ 2,
      sex_or_new             == "Pansexual" ~ 2,
      sex_or_new             == "Questioning" ~ 3,
      sex_or_new             == "Queer" ~ 2,
      sex_or_new             == "Demisexual" ~ 2,
      sex_or_new             == "Other_SexOr" ~ 2,
      sex_or_new             == "Queer_Straight" ~ 2,
      sex_or_new             == "Bisexual_Demisexual_Pansexual" ~ 2,
      sex_or_new             == "Bisexual_Gay_Les" ~ 2,
      sex_or_new             == "Bisexual_Gay_Les_Queer_Other_SexOr" ~ 2,
      sex_or_new             == "Bisexual_Pansexual_Queer" ~ 2,
      sex_or_new             == "Bisexual_Queer" ~ 2,
      sex_or_new             == "Bisexual_Straight" ~ 2,
      sex_or_new             == "Demisexual_Gay_Les" ~ 2,
      sex_or_new             == "Demisexual_Pansexual" ~ 2
    ))

# examine the participants who solely reported Other sexual orientation
so_write_in <- ds_teen[ds_teen$sex_or_new == "Other_SexOr",]
table(so_write_in$Other_SexOr_Open_1, exclude = NULL)

### go back and recode 1 participant who wrote in heterosexual 
ds_teen$sex_or_cat_R_1[ds_teen$Other_SexOr_Open_1 == "heterosexual"] <- 1 # straight/heterosexual

ds_teen$sex_or_cat_R_1 <- factor(ds_teen$sex_or_cat_R_1,
                                 levels = c(1,2,3),
                                 labels = c("Straight_heterosexual", "Sexual_diverse", "Questioning")) 

epiDisplay::tab1(ds_teen$sex_or_cat_R_1, cum.percent = TRUE)


ds_teen$Sex_1 <- factor(ds_teen$Sex_1,
                        levels = c(1,2),
                        labels = c("Male", "Female"))     




# descriptive statistics on state
length(unique(ds_teen$State_1))
table(ds_teen$State_1, exclude = NULL)
# NO AK or VT in data

# US region
epiDisplay::tab1(ds_teen$Region_4levels_1, cum.percent = TRUE)

ds_teen$census_region_1 <- factor(ds_teen$Region_4levels_1,
                                levels = c(1,2,3,4),
                                labels = c("northeast", "midwest", "south", "west"))    

epiDisplay::tab1(ds_teen$census_region_1, cum.percent = TRUE)


# examine family income, 4-levels
epiDisplay::tab1(ds_teen$HouseholdIncome_4levels_1, cum.percent = TRUE)

ds_teen$hs_income_1 <- factor(ds_teen$HouseholdIncome_4levels_1,
                              levels = c(1,2,3,4),
                              labels = c("< $30,000", "$30,000 to < $60,000", "$60,000 to < $100,000", "$100,000 or more"))    

epiDisplay::tab1(ds_teen$hs_income_1, cum.percent = TRUE)


# examine parental education
tab1(ds_teen$PAREDU_1)
# scored less than high school (0), high school or equivalent (.25), some college (.50), college graduate (.75), and postcollege or professional degree (1.00).
ds_teen$paredu_num <- NA
ds_teen$paredu_num[ds_teen$PAREDU_1 == 1] <- 0
ds_teen$paredu_num[ds_teen$PAREDU_1 == 2] <- .25
ds_teen$paredu_num[ds_teen$PAREDU_1 == 3] <- .50
ds_teen$paredu_num[ds_teen$PAREDU_1 == 4] <- .75
ds_teen$paredu_num[ds_teen$PAREDU_1 == 5] <- 1
class(ds_teen$paredu_num)
tab1(ds_teen$paredu_num)

# examine household income, 9 categories
tab1(ds_teen$HouseholdIncome_9levels_1)
# top category scored 1 (150k or more), the bottom category scored 0 (less than 5k), all others proportional [0, .125, .25, .375, .50, .625, .75, .875, 1]
ds_teen$income_num <- NA
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 1] <- 0
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 2] <- .125
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 3] <- .25
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 4] <- .375
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 5] <- .50
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 6] <- .625
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 7] <- .75
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 8] <- .875
ds_teen$income_num[ds_teen$HouseholdIncome_9levels_1 == 9] <- 1
class(ds_teen$income_num)
tab1(ds_teen$income_num)

# sum the values
ds_teen$ses <- ds_teen$paredu_num + ds_teen$income_num
tab1(ds_teen$ses)
mean(ds_teen$ses)

# standardize via z-transformation
ds_teen$std_ses <- scale(ds_teen$ses)

mean(ds_teen$std_ses)
sd(ds_teen$std_ses)
tab1(ds_teen$std_ses)

# bin into terciles (low, mid, high SES)
ds_teen$std_ses_tercile <- split_quantile(x = ds_teen$std_ses, type = 3)
tab1(ds_teen$std_ses_tercile)
class(ds_teen$std_ses_tercile)

ds_teen$std_ses_tercile <- factor(ds_teen$std_ses_tercile,
                              levels = c(1,2,3),
                              labels = c("low", "mid", "high"))    

tab1(ds_teen$std_ses_tercile, cum.percent = TRUE)


# ------------------------------------- RENAME DATAFRAME  --------------------------------------------

ds_measure <- ds_teen


# ------------------------------------- PREP TEEN MEASURES ----------------------------------------------
# DEPENDENT VARIABLES 
ds_measure <- ds_measure %>% 
  dplyr::mutate(
    # DEPENDENT VARIABLES (BB VICTIMIZATION, 6 items, T1:T4)
    VRACE_1   = if_else(HarassRace_1 >= 1, 1, 0),
    VNATO_1   = if_else(HarassNatOrigin_1 >= 1, 1, 0),
    VRELI_1   = if_else(HarassReligion_1 >= 1, 1, 0),
    VSEXO_1   = if_else(HarassSexOr_1 >= 1, 1, 0),
    VIMMI_1   = if_else(HarassImmigr_1 >= 1, 1, 0),
    VGEND_1   = if_else(HarassGenderID_1 >= 1, 1, 0),
    VSUM_1    = (VRACE_1 + VNATO_1 + VRELI_1 + VSEXO_1 + VIMMI_1 + VGEND_1),
    BBVIC_1   = rowMeans(across(c(HarassRace_1, HarassNatOrigin_1, HarassReligion_1, HarassSexOr_1, HarassImmigr_1, HarassGenderID_1))),
   
    VRACE_2   = if_else(HarassRace_2 >= 1, 1, 0),
    VNATO_2   = if_else(HarassNatOrigin_2 >= 1, 1, 0),
    VRELI_2   = if_else(HarassReligion_2 >= 1, 1, 0),
    VSEXO_2   = if_else(HarassSexOr_2 >= 1, 1, 0),
    VIMMI_2   = if_else(HarassImmigr_2 >= 1, 1, 0),
    VGEND_2   = if_else(HarassGenderID_2 >= 1, 1, 0),
    VSUM_2    = (VRACE_2 + VNATO_2 + VRELI_2 + VSEXO_2 + VIMMI_2 + VGEND_2),
    BBVIC_2   = rowMeans(across(c(HarassRace_2, HarassNatOrigin_2, HarassReligion_2, HarassSexOr_2, HarassImmigr_2, HarassGenderID_2))),
    
    VRACE_3   = if_else(HarassRace_3 >= 1, 1, 0),
    VNATO_3   = if_else(HarassNatOrigin_3 >= 1, 1, 0),
    VRELI_3   = if_else(HarassReligion_3 >= 1, 1, 0),
    VSEXO_3   = if_else(HarassSexOr_3 >= 1, 1, 0),
    VIMMI_3   = if_else(HarassImmigr_3 >= 1, 1, 0),
    VGEND_3   = if_else(HarassGenderID_3 >= 1, 1, 0),
    VSUM_3    = (VRACE_3 + VNATO_3 + VRELI_3 + VSEXO_3 + VIMMI_3 + VGEND_3),
    BBVIC_3   = rowMeans(across(c(HarassRace_3, HarassNatOrigin_3, HarassReligion_3, HarassSexOr_3, HarassImmigr_3, HarassGenderID_3))),
    
    VRACE_4   = if_else(HarassRace_4 >= 1, 1, 0),
    VNATO_4   = if_else(HarassNatOrigin_4 >= 1, 1, 0),
    VRELI_4   = if_else(HarassReligion_4 >= 1, 1, 0),
    VSEXO_4   = if_else(HarassSexOr_4 >= 1, 1, 0),
    VIMMI_4   = if_else(HarassImmigr_4 >= 1, 1, 0),
    VGEND_4   = if_else(HarassGenderID_4 >= 1, 1, 0),
    VSUM_4    = (VRACE_4 + VNATO_4 + VRELI_4 + VSEXO_4 + VIMMI_4 + VGEND_4),
    BBVIC_4   = rowMeans(across(c(HarassRace_4, HarassNatOrigin_4, HarassReligion_4, HarassSexOr_4, HarassImmigr_4, HarassGenderID_4))),
    
    # INDEPENDENT VARIABLES
    # PEER SOCIAL SUPPORT
    PSSUP_1 = rowMeans(across(c(Vaux1_1, Vaux2_1, Vaux3_1))), 
    # FAMILY SOCIAL SUPPORT
    FSSUP_1 = rowMeans(across(c(MSPSS1_1, MSPSS2_1, MSPSS3_1, MSPSS4_1))),
    # SCHOOL SOCIAL SUPPORT
    SSSUP_1 = rowMeans(across(c(SSS1_1, SSS2_1, SSS3_1, SSS4_1, SSS5_1, SSS6_1, SSS7_1, SSS8_1))),
    
    
    # DUMMY VARIABLES FOR MODELS
    female           = if_else(gender_2step == "Female", 1, 0),
    another_gid      = if_else(gender_2step == "Gender Diverse", 1, 0),
    black_nh         = if_else(race_ethn == "Black_NH", 1, 0),
    another_nh       = if_else(race_ethn == "Another_NH", 1, 0),
    hispanic         = if_else(race_ethn == "Hispanic", 1, 0),
    sexual_diverse   = if_else(sex_or_cat_R_1 == "Sexual_diverse", 1, 0),
    questioning      = if_else(sex_or_cat_R_1 == "Questioning", 1, 0),
    low_ses         = if_else(std_ses_tercile == "low", 1, 0),
    mid_ses         = if_else(std_ses_tercile == "mid", 1, 0),

    
    # GRAND MEAN CENTER AGE FOR MODELS
    age_c            = Age_1 - mean(Age_1)
    

  )


### Create State Protections Index and link to individual-level data via: "State_1" variable
### data from MAP report, data effective as of 6/2021
#1 race/ethnicity/national origin/religion (46 + D.C.)
ds_state <- ds_measure$State_1
ds_state[ds_state == "AL" | ds_state == "AZ" | ds_state == "CA" | ds_state == "CO" | 
              ds_state == "CT" | ds_state == "DE" | ds_state == "DC" | ds_state == "FL" | ds_state == "GA" |
              ds_state == "HI" | ds_state == "ID" | ds_state == "IL" | ds_state == "IA" | ds_state == "KS" |
              ds_state == "KY" | ds_state == "LA" | ds_state == "ME" | ds_state == "MD" | ds_state == "MA" |
              ds_state == "MI" | ds_state == "MN" | ds_state == "MS" | ds_state == "MO" | ds_state == "MT" |
              ds_state == "NE" | ds_state == "NV" | ds_state == "NH" | ds_state == "NJ" | ds_state == "NM" |
              ds_state == "NY" | ds_state == "NC" | ds_state == "ND" | ds_state == "OH" | ds_state == "OK" |
              ds_state == "OR" | ds_state == "PA" | ds_state == "RI" | ds_state == "SD" | ds_state == "TN" |
              ds_state == "TX" | ds_state == "UT" | ds_state == "VA" | ds_state == "WA" | ds_state == "WV" |
              ds_state == "WI"] <- 1
ds_state[ds_state == "AR" | ds_state == "IN" | ds_state == "SC" | ds_state == "WY"] <- 0
ds_measure["race_ethn_natlorigin_relig_index"] <- as.numeric(ds_state)
# 0 = absence of race/ethnicity/natl origin/religion protections, 1 = presence of race/ethnicity/natl origin/religion protections
epiDisplay::tab1(ds_measure$race_ethn_natlorigin_relig_index)



# 2 disability (34 + D.C.)
ds_state_d <- ds_measure$State_1
ds_state_d[ds_state_d == "AL" | ds_state_d == "AZ" | ds_state_d == "CA" | ds_state_d == "CO" | ds_state_d == "CT" |
             ds_state_d == "DE" | ds_state_d == "DC" | ds_state_d == "FL" | ds_state_d == "GA" |ds_state_d == "HI" |
             ds_state_d == "IL" | ds_state_d == "IA" | ds_state_d == "LA" | ds_state_d == "ME" | ds_state_d == "MD" |
             ds_state_d == "MA" | ds_state_d == "MN" | ds_state_d == "MO" | ds_state_d == "NE" | ds_state_d == "NV" | 
             ds_state_d == "NH" | ds_state_d == "NJ" | ds_state_d == "NM" |ds_state_d == "NY" | ds_state_d == "OK" |
             ds_state_d == "OR" | ds_state_d == "RI" | ds_state_d == "TN" | ds_state_d == "TX" | ds_state_d == "UT" |
             ds_state_d == "VA" | ds_state_d == "WA" | ds_state_d == "WI"] <- 1
ds_state_d[ds_state_d == "AR" | ds_state_d == "ID" | ds_state_d == "IN" | ds_state_d == "KS" | ds_state_d == "KY" |
             ds_state_d == "MI" | ds_state_d == "MS" | ds_state_d == "MT" | ds_state_d == "NC" | ds_state_d == "ND" |
             ds_state_d == "OH" | ds_state_d == "PA" | ds_state_d == "SC" | ds_state_d == "SD" | ds_state_d == "WV" |
             ds_state_d == "WY"] <- 0
ds_measure["disability_index"] <- as.numeric(ds_state_d)
# 0 = absence of disability protection, 1 = presence of disability protection
epiDisplay::tab1(ds_measure$disability_index)



# 3 sex/gender (33 + D.C.)
ds_state_s <- ds_measure$State_1
ds_state_s[ds_state_s == "AZ" | ds_state_s == "CA" | ds_state_s == "CT" | ds_state_s == "DE" | ds_state_s == "DC" |
             ds_state_s == "GA" |ds_state_s == "HI" | ds_state_s == "IL" | ds_state_s == "IA" | ds_state_s == "LA" | 
             ds_state_s == "ME" | ds_state_s == "MD" | ds_state_s == "MA" | ds_state_s == "MI" | ds_state_s == "MN" | 
             ds_state_s == "MS" | ds_state_s == "MO" | ds_state_s == "NE" | ds_state_s == "NV" | ds_state_s == "NH" | 
             ds_state_s == "NJ" | ds_state_s == "NM" |ds_state_s == "NY" | ds_state_s == "ND" | ds_state_s == "OR" |
             ds_state_s == "RI" | ds_state_s == "TN" | ds_state_s == "TX" | ds_state_s == "UT" | ds_state_s == "VA" | 
             ds_state_s == "WA" | ds_state_s == "WV"] <- 1
ds_state_s[ds_state_s == "AL" | ds_state_s == "AR" | ds_state_s == "CO" | ds_state_s == "FL" | ds_state_s == "ID" |
             ds_state_s == "IN" | ds_state_s == "KS" | ds_state_s == "KY" | ds_state_s == "MT" | ds_state_s == "NC" | 
             ds_state_s == "OH" | ds_state_s == "OK" | ds_state_s == "PA" | ds_state_s == "SC" | ds_state_s == "SD" | 
             ds_state_s == "WI" | ds_state_s == "WY"] <- 0
ds_measure["sex_index"] <- as.numeric(ds_state_s)
# 0 = absence of sex/gender protection, 1 = presence of sex/gender protection
epiDisplay::tab1(ds_measure$sex_index)


# 4 sexual orientation (33 + D.C.)
ds_state_so <- ds_measure$State_1
ds_state_so[ds_state_so == "AZ" | ds_state_so == "CA" | ds_state_so == "CO" | ds_state_so == "CT" | ds_state_so == "DE" |
             ds_state_so == "DC" | ds_state_so == "FL" | ds_state_so == "GA" |ds_state_so == "HI" | ds_state_so == "IL" | 
             ds_state_so == "IA" | ds_state_so == "KS" | ds_state_so == "KY" | ds_state_so == "LA" | ds_state_so == "ME" | 
             ds_state_so == "MD" | ds_state_so == "MA" | ds_state_so == "MN" | ds_state_so == "MO" | ds_state_so == "NE" | 
             ds_state_so == "NV" | ds_state_so == "NH" | ds_state_so == "NJ" | ds_state_so == "NM" |ds_state_so == "NY" |
             ds_state_so == "OR" | ds_state_so == "RI" | ds_state_so == "TN" | ds_state_so == "TX" | ds_state_so == "UT" | 
             ds_state_so == "VA" | ds_state_so == "WA" | ds_state_so == "WI"] <- 1
ds_state_so[ds_state_so == "AL" | ds_state_so == "AR" | ds_state_so == "ID" | ds_state_so == "IN" | ds_state_so == "MI" |
             ds_state_so == "MS" | ds_state_so == "MT" | ds_state_so == "NC" | ds_state_so == "ND" | ds_state_so == "OH" | 
             ds_state_so == "OK" | ds_state_so == "PA" | ds_state_so == "SC" | ds_state_so == "SD" | ds_state_so == "WV" | 
             ds_state_so == "WY"] <- 0
ds_measure["sexual_orientation_index"] <- as.numeric(ds_state_so)
# 0 = absence of sexual orientation protection, 1 = presence of sexual orientation protection
epiDisplay::tab1(ds_measure$sexual_orientation_index)


# 5 gender identity (23 + D.C.)
ds_state_g <- ds_measure$State_1
ds_state_g[ds_state_g == "CA" | ds_state_g == "CO" | ds_state_g == "CT" | ds_state_g == "DE" | ds_state_g == "DC" | 
              ds_state_g == "GA" | ds_state_g == "HI" | ds_state_g == "IL" | ds_state_g == "ME" | ds_state_g == "MD" | 
              ds_state_g == "MA" | ds_state_g == "MN" | ds_state_g == "MO" | ds_state_g == "NV" | ds_state_g == "NH" | 
              ds_state_g == "NJ" | ds_state_g == "NM" | ds_state_g == "NY" | ds_state_g == "OR" | ds_state_g == "TN" | 
              ds_state_g == "UT" | ds_state_g == "VA" | ds_state_g == "WA"] <- 1
ds_state_g[ds_state_g == "AL" | ds_state_g == "AZ" | ds_state_g == "AR" | ds_state_g == "FL" | 
              ds_state_g == "ID" | ds_state_g == "IN" | ds_state_g == "IA" | ds_state_g == "KS" | ds_state_g == "KY" |
              ds_state_g == "LA" | ds_state_g == "MI" | ds_state_g == "MS" | ds_state_g == "MT" | ds_state_g == "NE" | 
              ds_state_g == "NC" | ds_state_g == "ND" | ds_state_g == "OH" | ds_state_g == "OK" | ds_state_g == "PA" | 
              ds_state_g == "RI" | ds_state_g == "SC" | ds_state_g == "SD" | ds_state_g == "TX" | ds_state_g == "WV" | 
              ds_state_g == "WI" | ds_state_g == "WY"] <- 0
ds_measure["gender_identity_index"] <- as.numeric(ds_state_g)
# 0 = absence of gender identity protection, 1 = presence of gender identity protection
epiDisplay::tab1(ds_measure$gender_identity_index)


# 6 age (12 + D.C.)
ds_state_a <- ds_measure$State_1
ds_state_a[ds_state_a == "DC" | ds_state_a == "FL" | ds_state_a == "IA" | ds_state_a == "LA" | ds_state_a == "ME" |
             ds_state_a == "MN" | ds_state_a == "NE" | ds_state_a == "NH" | ds_state_a == "NM" | ds_state_a == "NY" |
             ds_state_a == "TX" | ds_state_a == "UT"] <- 1
ds_state_a[ds_state_a == "AL" | ds_state_a == "AZ" | ds_state_a == "AR" | ds_state_a == "CA" | ds_state_a == "CO" |
             ds_state_a == "CT" | ds_state_a == "DE" | ds_state_a == "GA" | ds_state_a == "HI" | ds_state_a == "ID" | 
             ds_state_a == "IL" | ds_state_a == "IN" | ds_state_a == "KS" | ds_state_a == "KY" | ds_state_a == "MD" | 
             ds_state_a == "MA" | ds_state_a == "MI" | ds_state_a == "MS" | ds_state_a == "MO" | ds_state_a == "MT" | 
             ds_state_a == "NV" | ds_state_a == "NJ" | ds_state_a == "NC" | ds_state_a == "ND" | ds_state_a == "OH" | 
             ds_state_a == "OK" | ds_state_a == "OR" | ds_state_a == "PA" | ds_state_a == "RI" | ds_state_a == "SC" | 
             ds_state_a == "SD" | ds_state_a == "TN" | ds_state_a == "VA" | ds_state_a == "WA" | ds_state_a == "WV" | 
             ds_state_a == "WI" | ds_state_a == "WY"] <- 0
ds_measure["age_index"] <- as.numeric(ds_state_a)
# 0 = absence of age protection, 1 = presence of age protection
epiDisplay::tab1(ds_measure$age_index)




# construct composite equity index (range: 0 to 6 for states; reminder no VT/AK in data)
ds_measure$protections <- ds_measure$race_ethn_natlorigin_relig_index + ds_measure$disability_index + ds_measure$sex_index +
                          ds_measure$sexual_orientation_index + ds_measure$gender_identity_index + ds_measure$age_index
  
ds_measure$protections <- as.numeric(ds_measure$protections)
epiDisplay::tab1(ds_measure$protections)
class(ds_measure$protections)


### code presence/absence of enumerated groups in anti-bullying laws by state
ds_state_e <- ds_measure$State_1
ds_state_e[ds_state_e == "AL" | ds_state_e == "AR" | ds_state_e == "CA" | ds_state_e == "CO" | ds_state_e == "CT" |
             ds_state_e == "DC" | ds_state_e == "HI" | ds_state_e == "IL" | ds_state_e == "IA" | ds_state_e == "ME" |
             ds_state_e == "MD" | ds_state_e == "MA" | ds_state_e == "MN" | ds_state_e == "NV" | ds_state_e == "NH" | 
             ds_state_e == "NJ" | ds_state_e == "NM" |ds_state_e == "NY" | ds_state_e == "NC" | ds_state_e == "OR" | 
             ds_state_e == "RI" | ds_state_e == "VA" | ds_state_e == "WA"] <- 1
ds_state_e[ds_state_e == "AZ" | ds_state_e == "DE" | ds_state_e == "FL" | ds_state_e == "GA" | ds_state_e == "ID" |
             ds_state_e == "IN" | ds_state_e == "KS" | ds_state_e == "KY" | ds_state_e == "LA" | ds_state_e == "MI" |
             ds_state_e == "MS" | ds_state_e == "MO" | ds_state_e == "MT" | ds_state_e == "NE" | ds_state_e == "ND" |
             ds_state_e == "OH" | ds_state_e == "OK" | ds_state_e == "PA" | ds_state_e == "SC" | ds_state_e == "SD" |
             ds_state_e == "TN" | ds_state_e == "TX" | ds_state_e == "UT" | ds_state_e == "WV" |
             ds_state_e == "WI" | ds_state_e == "WY"] <- 0
ds_measure["protected_groups_antibully"] <- as.numeric(ds_state_e)
# 0 = absence of protected groups, 1 = presence of protected groups
epiDisplay::tab1(ds_measure$protected_groups_antibully)


ds <- ds_measure |> 
  dplyr::select(
    P_ID, # unique participant ID
    WEIGHT_TEEN1_1, # WEIGHTING VARIABLE T1
    Age_1, age_c,
    Sex_1,
    gender_2step,
    race_ethn,
    sex_or_cat_R_1,
    census_region_1,
    hs_income_1,
    State_1,
    VRACE_1, VNATO_1, VRELI_1, VSEXO_1, VIMMI_1, VGEND_1, VSUM_1, BBVIC_1,
    VRACE_2, VNATO_2, VRELI_2, VSEXO_2, VIMMI_2, VGEND_2, VSUM_2, BBVIC_2,
    VRACE_3, VNATO_3, VRELI_3, VSEXO_3, VIMMI_3, VGEND_3, VSUM_3, BBVIC_3,
    VRACE_4, VNATO_4, VRELI_4, VSEXO_4, VIMMI_4, VGEND_4, VSUM_4, BBVIC_4,
    HarassRace_1, HarassNatOrigin_1, HarassReligion_1, HarassSexOr_1, HarassImmigr_1, HarassGenderID_1, # t1 individual items
    HarassRace_2, HarassNatOrigin_2, HarassReligion_2, HarassSexOr_2, HarassImmigr_2, HarassGenderID_2, # t2
    HarassRace_3, HarassNatOrigin_3, HarassReligion_3, HarassSexOr_3, HarassImmigr_3, HarassGenderID_3, # t3
    HarassRace_4, HarassNatOrigin_4, HarassReligion_4, HarassSexOr_4, HarassImmigr_4, HarassGenderID_4, # t4
    MSPSS1_1, MSPSS2_1, MSPSS3_1, MSPSS4_1, # Family Support - Item Level
    SSS1_1, SSS2_1, SSS3_1, SSS4_1, SSS5_1, SSS6_1, SSS7_1, SSS8_1, # School Support - Item Level
    PSSUP_1, FSSUP_1, SSSUP_1, # Mean peer support, family support, school social support 
    Vaux1_1, Vaux2_1, Vaux3_1, # Peer Support - Item Level
    protections, # protections index
    protected_groups_antibully, # binary indicator of protected groups in state antibully laws
    female, # baseline dummy variables 
    another_gid,
    black_nh,
    another_nh,
    hispanic,
    sexual_diverse,
    questioning,
    std_ses,
    std_ses_tercile,
    low_ses,
    mid_ses
    
    
  )

# ------------------------------------- RELIABILITY ---------------------------------------------------------------

# Vaux Social Support Record -  Peer Support, T1
vaux3_t1_rel <- ds %>% dplyr::select(Vaux1_1, Vaux2_1, Vaux3_1)

# MSPSS 4-item family support subscale, T1
mspss_sub_t1_rel <- ds %>% dplyr::select(MSPSS1_1, MSPSS2_1, MSPSS3_1, MSPSS4_1)

# Student Support Scale 8-item, T1
sss8_t1_rel <- ds %>% dplyr::select(SSS1_1, SSS2_1, SSS3_1, SSS4_1, SSS5_1, SSS6_1, SSS7_1, SSS8_1)

# BB Vic, 6 items, Time 1
bbvic_t1_rel   = ds %>% dplyr::select(HarassRace_1, HarassNatOrigin_1, HarassReligion_1, HarassSexOr_1, HarassImmigr_1, HarassGenderID_1)

# BB Vic, 6 items, Time 2
bbvic_t2_rel   = ds %>% dplyr::select(HarassRace_2, HarassNatOrigin_2, HarassReligion_2, HarassSexOr_2, HarassImmigr_2, HarassGenderID_2)

# BB Vic, 6 items, Time 3
bbvic_t3_rel   = ds %>% dplyr::select(HarassRace_3, HarassNatOrigin_3, HarassReligion_3, HarassSexOr_3, HarassImmigr_3, HarassGenderID_3)

# BB Vic, 6 items, Time 4
bbvic_t4_rel   = ds %>% dplyr::select(HarassRace_4, HarassNatOrigin_4, HarassReligion_4, HarassSexOr_4, HarassImmigr_4, HarassGenderID_4)


psych::alpha(vaux3_t1_rel)
psych::alpha(mspss_sub_t1_rel)
psych::alpha(sss8_t1_rel)
psych::alpha(bbvic_t1_rel)
psych::alpha(bbvic_t2_rel)
psych::alpha(bbvic_t3_rel)
psych::alpha(bbvic_t4_rel)

# ------------------------------------- EXAMINE MISSINGNESS ---------------------------------------------------------------

# subset df to key variables to examine missingness
ds_key_var <- ds |> 
  dplyr::select(
    VSUM_1, VSUM_2, VSUM_3, VSUM_4, # bb vic items, T1:T4
    PSSUP_1, # average peer support
    FSSUP_1, # average family support
    SSSUP_1, # average school social support
    protections, # protections index
    protected_groups_antibully, # enumerations anti-bullying laws
    Age_1,
    gender_2step,
    race_ethn,
    sex_or_cat_R_1,
    std_ses
    
  )

# any missing
any_na(ds_key_var)
# count across df
n_miss(ds_key_var)
prop_miss(ds_key_var)
# names of variables w/ missing values
ds_key_var %>% is.na() %>% colSums()

# n/% missing values per variable 
missing <- miss_var_summary(ds_key_var)
print(missing,n=14)

# n/% missing values per participant 
miss_case_summary(ds_key_var)
miss_case_table(ds_key_var)

# examine ggplot
gg_miss_var(ds_key_var)
vis_miss(ds_key_var) + theme(axis.text.x = element_text(angle=80))



# ------------------------------------- EXAMINE SAMPLE CHARACTERISTICS   ------------------------------------
# age, M(SD)
round(mean(ds$Age_1), digits = 1)
round(sd(ds$Age_1), digits =1)

# gender identity, 2-step
tab1(ds$gender_2step, sort.group = "decreasing", cum.percent = TRUE)

# race/ethnicity
tab1(ds$race_ethn, sort.group = "decreasing", cum.percent = TRUE)

# sexual orientation, recoded
tab1(ds$sex_or_cat_R_1, sort.group = "decreasing", cum.percent = TRUE)

# ses, binned into terciles
tab1(ds$std_ses_tercile, sort.group = "decreasing", cum.percent = TRUE)

# us census region
tab1(ds$census_region_1, sort.group = "decreasing", cum.percent = TRUE)

# average peer support
mean(ds$PSSUP_1, na.rm = TRUE)
sd(ds$PSSUP_1, na.rm = TRUE)

# average family support
mean(ds$FSSUP_1, na.rm = TRUE)
sd(ds$FSSUP_1, na.rm = TRUE)

# average school climate
mean(ds$SSSUP_1, na.rm = TRUE)
sd(ds$SSSUP_1, na.rm = TRUE)

# protections index
tab1(ds$protections, cum.percent = TRUE)
mean(ds$protections)
sd(ds$protections)

# enumerated classes in anti-bully laws
tab1(ds$protected_groups_antibully, cum.percent = TRUE)


# BB Vic Sum, Wave 1, average
round(mean(ds$VSUM_1, na.rm = TRUE), digits = 2)
round(sd(ds$VSUM_1, na.rm = TRUE), digits = 2)

# BB Vic, Wave 2, average
round(mean(ds$VSUM_2, na.rm = TRUE), digits = 2)
round(sd(ds$VSUM_2, na.rm = TRUE), digits = 2)

# BB Vic, Wave 3, average
round(mean(ds$VSUM_3, na.rm = TRUE), digits = 2)
round(sd(ds$VSUM_3, na.rm = TRUE), digits = 2)

# BB Vic, Wave 4, average
round(mean(ds$VSUM_4, na.rm = TRUE), digits = 2)
round(sd(ds$VSUM_4, na.rm = TRUE), digits = 2)

##### BB Victimization over time, T1:T4 #####
# Plot to examine sum scores over time 
# subset to variables of interest, bb vic
ds_bbvic <- ds[, c("P_ID", "VSUM_1", "VSUM_2",
                   "VSUM_3", "VSUM_4")]

dim(ds_bbvic)
describe(ds_bbvic)


# convert wide to long
ds_bbvic_long <- Hmisc::reShape(x = ds_bbvic, id = "P_ID", reps = 4, times = c(1, 2, 3, 4), timevar = "wave",
                                base = c("VSUM_"))

# remove underscore from column names
names(ds_bbvic_long) <- gsub("_", "", names(ds_bbvic_long), fixed = TRUE)


# remove rows with NA for bb vic score
ds_bbvic_long_complete <- ds_bbvic_long[which(is.na(ds_bbvic_long$VSUM) == FALSE), ]



# plot with mean line
ggplot(ds_bbvic_long_complete, aes(wave, VSUM, group = PID)) + 
  geom_line(alpha = 0.05) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) +
  theme_bw() + 
  #setting x-axis with breaks and labels
  scale_x_continuous(limits=c(1,4),
                     breaks = c(1,2,3,4), 
                     name = "Wave") +    
  #setting y-axis with limits breaks and labels
  scale_y_continuous(limits=c(0,6), 
                     breaks = c(0,1,2,3,4,5,6), 
                     name = "BB Vic Sum Score")


# Plot to examine mean scores over time 
# subset to variables of interest, bb vic 1:4
ds_bbvic_mean <- ds[, c("P_ID", "BBVIC_1", "BBVIC_2",
                        "BBVIC_3", "BBVIC_4")]

dim(ds_bbvic_mean)
describe(ds_bbvic_mean)


# convert wide to long
ds_bbvic_mean_long <- Hmisc::reShape(x = ds_bbvic_mean, id = "P_ID", reps = 4, times = c(1, 2, 3, 4), timevar = "wave",
                                     base = c("BBVIC_"))

# remove underscore from column names
names(ds_bbvic_mean_long) <- gsub("_", "", names(ds_bbvic_mean_long), fixed = TRUE)


# remove rows with NA for bb vic score
ds_bbvic_mean_long_complete <- ds_bbvic_mean_long[which(is.na(ds_bbvic_mean_long$BBVIC) == FALSE), ]



# plot with mean line
ggplot(ds_bbvic_mean_long_complete, aes(wave, BBVIC, group = PID)) + 
  geom_line(alpha = 0.05) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.5,
    color = "red"
  ) +
  theme_bw() + 
  #setting x-axis with breaks and labels
  scale_x_continuous(limits=c(1,4),
                     breaks = c(1,2,3,4), 
                     name = "Wave") +    
  #setting y-axis with limits breaks and labels
  scale_y_continuous(limits=c(0,6), 
                     breaks = c(0,1,2,3), 
                     name = "BB Vic Mean Score")



#### MEASUREMENT INVARIANCE ####
#Configural measurement invariance - freely estimated factors at each time point

configural <-' 

#coding in factors at each wave, in this case using unit loading identification
bbvic1 =~ HarassRace_1 + HarassNatOrigin_1 + HarassReligion_1 + HarassSexOr_1 + HarassImmigr_1 + HarassGenderID_1  
bbvic2 =~ HarassRace_2 + HarassNatOrigin_2 + HarassReligion_2 + HarassSexOr_2 + HarassImmigr_2 + HarassGenderID_2
bbvic3 =~ HarassRace_3 + HarassNatOrigin_3 + HarassReligion_3 + HarassSexOr_3 + HarassImmigr_3 + HarassGenderID_3
bbvic4 =~ HarassRace_4 + HarassNatOrigin_4 + HarassReligion_4 + HarassSexOr_4 + HarassImmigr_4 + HarassGenderID_4


#covariance among residuals at each wave
HarassRace_1 ~~ HarassRace_2 + HarassRace_3 + HarassRace_4
HarassRace_2 ~~ HarassRace_3 + HarassRace_4
HarassRace_3 ~~ HarassRace_4

HarassNatOrigin_1 ~~ HarassNatOrigin_2 + HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_2 ~~ HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_3 ~~ HarassNatOrigin_4

HarassReligion_1 ~~ HarassReligion_2 + HarassReligion_3 + HarassReligion_4
HarassReligion_2 ~~ HarassReligion_3 + HarassReligion_4
HarassReligion_3 ~~ HarassReligion_4

HarassSexOr_1 ~~ HarassSexOr_2 + HarassSexOr_3 + HarassSexOr_4
HarassSexOr_2 ~~ HarassSexOr_3 + HarassSexOr_4
HarassSexOr_3 ~~ HarassSexOr_4

HarassImmigr_1 ~~ HarassImmigr_2 + HarassImmigr_3 + HarassImmigr_4
HarassImmigr_2 ~~ HarassImmigr_3 + HarassImmigr_4
HarassImmigr_3 ~~ HarassImmigr_4

HarassGenderID_1 ~~ HarassGenderID_2 + HarassGenderID_3 + HarassGenderID_4
HarassGenderID_2 ~~ HarassGenderID_3 + HarassGenderID_4
HarassGenderID_3 ~~ HarassGenderID_4


#estimate observed intercepts at each wave
HarassRace_1 ~1
HarassNatOrigin_1 ~1
HarassReligion_1 ~1
HarassSexOr_1 ~1
HarassImmigr_1 ~1
HarassGenderID_1 ~1

HarassRace_2 ~1
HarassNatOrigin_2 ~1
HarassReligion_2 ~1
HarassSexOr_2 ~1
HarassImmigr_2 ~1
HarassGenderID_2 ~1

HarassRace_3 ~1
HarassNatOrigin_3 ~1
HarassReligion_3 ~1
HarassSexOr_3 ~1
HarassImmigr_3 ~1
HarassGenderID_3 ~1

HarassRace_4 ~1
HarassNatOrigin_4 ~1
HarassReligion_4 ~1
HarassSexOr_4 ~1
HarassImmigr_4 ~1
HarassGenderID_4 ~1

#fix latent means to 0 to identify models
bbvic1 ~ 0*1
bbvic2 ~ 0*1
bbvic3 ~ 0*1
bbvic4 ~ 0*1

'

configural.fit<-sem(configural, data=ds, meanstructure = TRUE, missing = "ML")
summary(configural.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(configural.fit) #Modification indices
residuals(configural.fit, type="standardized")


#Weak/metric Measurement Invariance - fixed factor loadings

weak <-' 

#same model as before, only difference is factor loadings are fixed across waves
#first loading still assumed to be 1.0, but use code (l2* and l3*) to constrain
#remaining loadings to equality
#rest of code is the same

bbvic1 =~ HarassRace_1 + l2*HarassNatOrigin_1 + l3*HarassReligion_1 + l4*HarassSexOr_1 + l5*HarassImmigr_1 + l6*HarassGenderID_1  
bbvic2 =~ HarassRace_2 + l2*HarassNatOrigin_2 + l3*HarassReligion_2 + l4*HarassSexOr_2 + l5*HarassImmigr_2 + l6*HarassGenderID_2
bbvic3 =~ HarassRace_3 + l2*HarassNatOrigin_3 + l3*HarassReligion_3 + l4*HarassSexOr_3 + l5*HarassImmigr_3 + l6*HarassGenderID_3
bbvic4 =~ HarassRace_4 + l2*HarassNatOrigin_4 + l3*HarassReligion_4 + l4*HarassSexOr_4 + l5*HarassImmigr_4 + l6*HarassGenderID_4


#covariance among residuals at each wave
HarassRace_1 ~~ HarassRace_2 + HarassRace_3 + HarassRace_4
HarassRace_2 ~~ HarassRace_3 + HarassRace_4
HarassRace_3 ~~ HarassRace_4

HarassNatOrigin_1 ~~ HarassNatOrigin_2 + HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_2 ~~ HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_3 ~~ HarassNatOrigin_4

HarassReligion_1 ~~ HarassReligion_2 + HarassReligion_3 + HarassReligion_4
HarassReligion_2 ~~ HarassReligion_3 + HarassReligion_4
HarassReligion_3 ~~ HarassReligion_4

HarassSexOr_1 ~~ HarassSexOr_2 + HarassSexOr_3 + HarassSexOr_4
HarassSexOr_2 ~~ HarassSexOr_3 + HarassSexOr_4
HarassSexOr_3 ~~ HarassSexOr_4

HarassImmigr_1 ~~ HarassImmigr_2 + HarassImmigr_3 + HarassImmigr_4
HarassImmigr_2 ~~ HarassImmigr_3 + HarassImmigr_4
HarassImmigr_3 ~~ HarassImmigr_4

HarassGenderID_1 ~~ HarassGenderID_2 + HarassGenderID_3 + HarassGenderID_4
HarassGenderID_2 ~~ HarassGenderID_3 + HarassGenderID_4
HarassGenderID_3 ~~ HarassGenderID_4


#estimate observed intercepts at each wave
HarassRace_1 ~1
HarassNatOrigin_1 ~1
HarassReligion_1 ~1
HarassSexOr_1 ~1
HarassImmigr_1 ~1
HarassGenderID_1 ~1

HarassRace_2 ~1
HarassNatOrigin_2 ~1
HarassReligion_2 ~1
HarassSexOr_2 ~1
HarassImmigr_2 ~1
HarassGenderID_2 ~1

HarassRace_3 ~1
HarassNatOrigin_3 ~1
HarassReligion_3 ~1
HarassSexOr_3 ~1
HarassImmigr_3 ~1
HarassGenderID_3 ~1

HarassRace_4 ~1
HarassNatOrigin_4 ~1
HarassReligion_4 ~1
HarassSexOr_4 ~1
HarassImmigr_4 ~1
HarassGenderID_4 ~1

#fix latent means to 0 to identify models
bbvic1 ~ 0*1
bbvic2 ~ 0*1
bbvic3 ~ 0*1
bbvic4 ~ 0*1
'

weak.fit<-sem(weak, data=ds, meanstructure=TRUE, missing = "ML")
summary(weak.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(weak.fit) #Modification indices
lavTestScore(weak.fit) #Modification indices for constrained parameters
residuals(weak.fit, type="standardized")
#code for comparing nested models using LRT and BIC/AIC
anova(configural.fit, weak.fit)


#Strong/Scalar Measurement Invariance - fixed factor loadings
#now add constraints on intercepts
strong <-' 
bbvic1 =~ HarassRace_1 + l2*HarassNatOrigin_1 + l3*HarassReligion_1 + l4*HarassSexOr_1 + l5*HarassImmigr_1 + l6*HarassGenderID_1  
bbvic2 =~ HarassRace_2 + l2*HarassNatOrigin_2 + l3*HarassReligion_2 + l4*HarassSexOr_2 + l5*HarassImmigr_2 + l6*HarassGenderID_2
bbvic3 =~ HarassRace_3 + l2*HarassNatOrigin_3 + l3*HarassReligion_3 + l4*HarassSexOr_3 + l5*HarassImmigr_3 + l6*HarassGenderID_3
bbvic4 =~ HarassRace_4 + l2*HarassNatOrigin_4 + l3*HarassReligion_4 + l4*HarassSexOr_4 + l5*HarassImmigr_4 + l6*HarassGenderID_4


#covariance among residuals at each wave
HarassRace_1 ~~ HarassRace_2 + HarassRace_3 + HarassRace_4
HarassRace_2 ~~ HarassRace_3 + HarassRace_4
HarassRace_3 ~~ HarassRace_4

HarassNatOrigin_1 ~~ HarassNatOrigin_2 + HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_2 ~~ HarassNatOrigin_3 + HarassNatOrigin_4
HarassNatOrigin_3 ~~ HarassNatOrigin_4

HarassReligion_1 ~~ HarassReligion_2 + HarassReligion_3 + HarassReligion_4
HarassReligion_2 ~~ HarassReligion_3 + HarassReligion_4
HarassReligion_3 ~~ HarassReligion_4

HarassSexOr_1 ~~ HarassSexOr_2 + HarassSexOr_3 + HarassSexOr_4
HarassSexOr_2 ~~ HarassSexOr_3 + HarassSexOr_4
HarassSexOr_3 ~~ HarassSexOr_4

HarassImmigr_1 ~~ HarassImmigr_2 + HarassImmigr_3 + HarassImmigr_4
HarassImmigr_2 ~~ HarassImmigr_3 + HarassImmigr_4
HarassImmigr_3 ~~ HarassImmigr_4

HarassGenderID_1 ~~ HarassGenderID_2 + HarassGenderID_3 + HarassGenderID_4
HarassGenderID_2 ~~ HarassGenderID_3 + HarassGenderID_4
HarassGenderID_3 ~~ HarassGenderID_4


# add constraints on intercepts 
HarassRace_1 ~ t1*1
HarassNatOrigin_1 ~ t2*1
HarassReligion_1 ~ t3*1
HarassSexOr_1 ~ t4*1
HarassImmigr_1 ~ t5*1
HarassGenderID_1 ~ t6*1

HarassRace_2 ~ t1*1
HarassNatOrigin_2 ~ t2*1
HarassReligion_2 ~ t3*1
HarassSexOr_2 ~ t4*1
HarassImmigr_2 ~ t5*1
HarassGenderID_2 ~ t6*1

HarassRace_3 ~ t1*1
HarassNatOrigin_3 ~ t2*1
HarassReligion_3 ~ t3*1
HarassSexOr_3 ~ t4*1
HarassImmigr_3 ~ t5*1
HarassGenderID_3 ~ t6*1

HarassRace_4 ~ t1*1
HarassNatOrigin_4 ~ t2*1
HarassReligion_4 ~ t3*1
HarassSexOr_4 ~ t4*1
HarassImmigr_4 ~ t5*1
HarassGenderID_4 ~ t6*1


#when fixing intercepts to equality, remember to free one latent mean
#if you do not do this, you are also testing that means are equivalent
bbvic1 ~ 0*1
bbvic2 ~ 1
bbvic3 ~ 1
bbvic4 ~ 1

bbvic2 ~~ bbvic1
bbvic3 ~~ bbvic2
bbvic4 ~~ bbvic3
'

strong.fit<-sem(strong, data=ds, meanstructure=TRUE, missing = "ML")
summary(strong.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(strong.fit) #Modification indices
lavTestScore(strong.fit) #Modification indices for constrained parameters
residuals(strong.fit, type="standardized")

anova(weak.fit, strong.fit)

##### MODELING #####

# bbvic sum score, range: 0 - 6 
### Model 1: Estimate LCGM, linear model, unweighted
m1 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4'

fit1 <- growth(m1, data = ds, estimator = 'MLR', missing = 'FIML') # robust fit

summary(fit1, fit.measures = TRUE, standardized = TRUE)


### Model 2: Estimate LCGM, linear model, weighted
m2 <-    'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
          s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4'

fit2 <- growth(m2, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1") # sampling weights, default is MLR, robust

summary (fit2, fit.measures = TRUE, standardized = TRUE)

# derive one-tailed p-values from two-tailed p-values for * variance only * given it is appropriately 1-tailed test
# variance for slope
.038/2


### Model 3 with quadratic slope, weighted
m3 <-    'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
          s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4
          s2 =~ 0*VSUM_1 + 1*VSUM_2 + 4*VSUM_3 + 9*VSUM_4'

fit3 <- growth(m3, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1", check.gradient = FALSE) # sampling weights, default is MLR, robust

summary(fit3, fit.measures = TRUE, standardized = TRUE)


anova(fit3, fit2) # compare models
### Linear Model is better fit - use to build out model ###


### Model 4: Estimate LCGM, linear, weighted, individual-level predictors 
m4 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4
       

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses 
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses'


fit4 <- growth(m4, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary (fit4, fit.measures = TRUE, standardized = TRUE)

# variance for slope
.061/2


### Model 5: Estimate LCGM, linear, weighted, peer-level predictors
m5 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + PSSUP_1
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + PSSUP_1'


fit5 <- growth(m5, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary (fit5, fit.measures = TRUE, standardized = TRUE)

# variance for slope 
.064/2


### Model 6: Estimate LCGM, linear, weighted, family-level predictors
m6 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + FSSUP_1
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + FSSUP_1'


fit6 <- growth(m6, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary(fit6, fit.measures = TRUE, standardized = TRUE)

# variance for slope
.061/2


### Model 7: Estimate LCGM, linear, weighted, school-level predictors
m7 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + SSSUP_1
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + SSSUP_1'


fit7 <- growth(m7, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary(fit7, fit.measures = TRUE, standardized = TRUE)

# variance for slope
.068/2



### Model 8: Estimate LCGM, linear, weighted, community-level predictors
m8 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + protections + protected_groups_antibully
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + protections + protected_groups_antibully'


fit8 <- growth(m8, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary(fit8, fit.measures = TRUE, standardized = TRUE)

# variance for slope
.068/2


### Model 9: FULL MODEL
m9 <- 'i =~ 1*VSUM_1 + 1*VSUM_2 + 1*VSUM_3 + 1*VSUM_4 
       s =~ 0*VSUM_1 + 1*VSUM_2 + 2*VSUM_3 + 3*VSUM_4

       # regressions
       i ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + PSSUP_1 + FSSUP_1 + SSSUP_1 + protections + protected_groups_antibully
       s ~ age_c + female + black_nh + another_nh + hispanic + sexual_diverse + questioning + std_ses + PSSUP_1 + FSSUP_1 + SSSUP_1 + protections + protected_groups_antibully'

fit9 <- growth(m9, data = ds, estimator = 'MLR', missing = 'FIML', sampling.weights = "WEIGHT_TEEN1_1")

summary(fit9, fit.measures = TRUE, standardized = TRUE)

# variance for slope
.066/2


### DATA VIZ ###

# Figure 1. Enumerated Anti-bullying laws by state
enum_state <- aggregate(ds$protected_groups_antibully, by = list(State = ds$State_1), FUN = max)
enum_state <- rbind(enum_state, c("AK", 0))  # no protected groups
enum_state <- rbind(enum_state, c("VT", 1))  # yes protected groups, last reviewed 8/1/2021 on stopbullying.gov site
enum_state
colnames(enum_state) <- c("State", "x")
enum_state
class(enum_state$x)
# convert
enum_state_e <- enum_state$x
enum_state_e[enum_state$x == 0] <- "No Protections"
enum_state_e[enum_state$x == 1] <- "Protections"
enum_state["protected"] <- as.factor(enum_state_e)



# US map
us_states <- us_map(
  regions = "states")

us_states_score <- us_states %>%
  left_join(enum_state, by = c("abbr" = "State"))
us_states_score
summary(us_states_score)



p1 <- plot_usmap(data = us_states_score, values = "protected", labels=FALSE)
p1

p1a <- p1 + scale_fill_brewer(name = "Enumeration", palette = "Paired") +
  ggtitle('Figure 1a. Enumerated Anti-bullying Laws by State') +
  theme(plot.title = element_text(size=12, hjust = -0.33, family = "serif", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "serif"),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
#       panel.background=element_rect(fill = "white"),
        panel.border=element_blank(),
        panel.grid=element_blank())
#       plot.background=element_rect(fill = "white"))

p1a

# Figure 2. Protections Index by state
protections_state <- aggregate(ds$protections, by = list(State = ds$State_1), FUN = max)
protections_state <- rbind(protections_state, c("AK", 3))
protections_state <- rbind(protections_state, c("VT", 6))
protections_state
colnames(protections_state) <- c("State", "x")
protections_state
protections_state$x <- as.factor(protections_state$x)



us_states_score_p <- us_states %>%
  left_join(protections_state, by = c("abbr" = "State"))
us_states_score_p
summary(us_states_score_p)



p2 <- plot_usmap(data = us_states_score_p, values = "x", labels=FALSE)
p2


p2a <- p2 + scale_fill_brewer(name = "Protections Index", palette = "PuBuGn") +
  ggtitle('Figure 1b. Hate Crime Laws Protections Index by State') +
  theme(plot.title = element_text(size=12, hjust = -0.4, family = "serif", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "serif"),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
#       panel.background=element_rect(fill = "white"),
        panel.border=element_blank(),
        panel.grid=element_blank())
#       plot.background=element_rect(fill = "white"))

p2a




### panel plot
### combine plots #1-2 into one plot 
pdf("original_figures/Figure_combined.pdf")
plots = plot_grid(
  plotlist = list(p1a, p2a),
  align = "hv",
  axis = "tb",
  nrow = 2)
plot_grid(plotlist = list(plots))
dev.off()




### COLLABORATOR FEEDBACK ON MANUSCRIPT###
# SUBSET DATA TO JUST LOOK AT SMY
ds_smy = ds %>%
  filter(sex_or_cat_R_1 == "Sexual_diverse")

dim(ds_smy) # N = 104

### examine means on overall (aggregate) BBV items

# BB Vic Sum, Wave 1, average
round(mean(ds_smy$VSUM_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSUM_1, na.rm = TRUE), digits = 2)

# BB Vic, Wave 2, average
round(mean(ds_smy$VSUM_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSUM_2, na.rm = TRUE), digits = 2)

# BB Vic, Wave 3, average
round(mean(ds_smy$VSUM_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSUM_3, na.rm = TRUE), digits = 2)

# BB Vic, Wave 4, average
round(mean(ds_smy$VSUM_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSUM_4, na.rm = TRUE), digits = 2)


# SUBSET DATA TO JUST LOOK AT STRAIGHT/HETEROSEXUAL YOUTH
ds_ht = ds %>%
  filter(sex_or_cat_R_1 == "Straight_heterosexual")

dim(ds_ht) # N = 484

### examine means on overall (aggregate) BBV items

# BB Vic Sum, Wave 1, average
round(mean(ds_ht$VSUM_1, na.rm = TRUE), digits = 2)
round(sd(ds_ht$VSUM_1, na.rm = TRUE), digits = 2)

# BB Vic, Wave 2, average
round(mean(ds_ht$VSUM_2, na.rm = TRUE), digits = 2)
round(sd(ds_ht$VSUM_2, na.rm = TRUE), digits = 2)

# BB Vic, Wave 3, average
round(mean(ds_ht$VSUM_3, na.rm = TRUE), digits = 2)
round(sd(ds_ht$VSUM_3, na.rm = TRUE), digits = 2)

# BB Vic, Wave 4, average
round(mean(ds_ht$VSUM_4, na.rm = TRUE), digits = 2)
round(sd(ds_ht$VSUM_4, na.rm = TRUE), digits = 2)





### now look at by type
### Time 1: VRACE_1, VNATO_1, VRELI_1, VSEXO_1, VIMMI_1, VGEND_1
### for SMY
# Race-based
#Wave 1, average
round(mean(ds_smy$VRACE_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRACE_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VRACE_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRACE_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VRACE_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRACE_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VRACE_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRACE_4, na.rm = TRUE), digits = 2)


# Natl-origin-based
# Wave 1, average
round(mean(ds_smy$VNATO_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VNATO_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VNATO_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VNATO_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VNATO_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VNATO_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VNATO_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VNATO_4, na.rm = TRUE), digits = 2)


# Religious-based
# Wave 1, average
round(mean(ds_smy$VRELI_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRELI_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VRELI_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRELI_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VRELI_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRELI_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VRELI_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VRELI_4, na.rm = TRUE), digits = 2)


# Sexual orientation-based
# Wave 1, average
round(mean(ds_smy$VSEXO_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSEXO_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VSEXO_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSEXO_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VSEXO_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSEXO_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VSEXO_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VSEXO_4, na.rm = TRUE), digits = 2)


# Immigration-based
# Wave 1, average
round(mean(ds_smy$VIMMI_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VIMMI_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VIMMI_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VIMMI_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VIMMI_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VIMMI_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VIMMI_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VIMMI_4, na.rm = TRUE), digits = 2)


# Gender-based
# Wave 1, average
round(mean(ds_smy$VGEND_1, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VGEND_1, na.rm = TRUE), digits = 2)

# Wave 2, average
round(mean(ds_smy$VGEND_2, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VGEND_2, na.rm = TRUE), digits = 2)

# Wave 3, average
round(mean(ds_smy$VGEND_3, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VGEND_3, na.rm = TRUE), digits = 2)

# Wave 4, average
round(mean(ds_smy$VGEND_4, na.rm = TRUE), digits = 2)
round(sd(ds_smy$VGEND_4, na.rm = TRUE), digits = 2)



### Reviewer feedback: provide n/% of youth by sexual orientation/gender identity, examine Time4 as comparison
# Sexual Orientation, Time 4 

epiDisplay::tab1(ds_teen$Bisexual_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Demisexual_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Gay_Les_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Pansexual_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Queer_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Straight_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Questioning_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_SexOr_4, cum.percent = TRUE)

# select variables
ds_sex_or_4 <- ds_teen |> 
  dplyr::select(ID, Bisexual_4, Demisexual_4, Gay_Les_4, Pansexual_4, Queer_4, Straight_4, Questioning_4, Other_SexOr_4)

# remove "_prs" from column names
names(ds_sex_or_4) <- gsub("_4", "", names(ds_sex_or_4), fixed = TRUE)

ds_sex_or_w4 <- ds_sex_or_4 |>
  pivot_longer(!ID, names_to = "sex_or", values_to = "count") |> # make data long by the categories
  mutate(sex_or4 = ifelse(count == 1, sex_or, NA)) |>  # place actual name of the category in the column based on count column in new column called sex_or1
  subset(select = -count) |>  # remove count column beause we dont need it anymore
  pivot_wider(names_from = sex_or, values_from = sex_or4) |> # turn data back wide by the categories
  unite(sex_or_new_4, Bisexual:Other_SexOr, sep='_', na.rm = TRUE) # collapse the columns, separate words with "_", and remove NAs


epiDisplay::tab1(ds_sex_or_w4$sex_or_new_4, main = "Sexual Orientation", cum.percent = TRUE) 

# create data frame to count the number of people in each category 
ds_sex_or_grp_4 <- ds_sex_or_w4 |> 
  group_by(sex_or_new_4) |>
  summarise(sex_or_count = length(sex_or_new_4))

ds_sex_or_grp_4 # examine

# join sex orient data at wave 4 with original data using ResponseId         
ds_teen <- left_join(ds_teen, ds_sex_or_w4, by = "ID")
names(ds_teen)

table(ds_teen$sex_or_new_4, exclude = NULL) # examine

# combining sexual orientation for wave 4
ds_teen <- ds_teen %>%
  dplyr::mutate(
    sex_or_cat_4 = case_when(
      sex_or_new_4             == "Straight" ~ 1,
      sex_or_new_4             == "Bisexual" ~ 2,
      sex_or_new_4             == "Gay_Les" ~ 3,
      sex_or_new_4             == "Pansexual" ~ 4,
      sex_or_new_4             == "Questioning" ~ 5,
      sex_or_new_4             == "Queer" ~ 6,
      sex_or_new_4             == "Demisexual" ~ 6,
      sex_or_new_4             == "Other_SexOr" ~ 6,
      sex_or_new_4             == "Queer_Straight" ~ 6,
      sex_or_new_4             == "Bisexual_Demisexual_Pansexual" ~ 6,
      sex_or_new_4             == "Bisexual_Gay_Les" ~ 6,
      sex_or_new_4             == "Bisexual_Gay_Les_Queer_Other_SexOr" ~ 6,
      sex_or_new_4             == "Bisexual_Pansexual_Queer" ~ 6,
      sex_or_new_4             == "Bisexual_Queer" ~ 6,
      sex_or_new_4             == "Bisexual_Straight" ~ 6,
      sex_or_new_4             == "Demisexual_Gay_Les" ~ 6,
      sex_or_new_4             == "Demisexual_Pansexual" ~ 6,
      sex_or_new_4             == "Bisexual_Demisexual_Queer" ~ 6,
      sex_or_new_4             == "Bisexual_Gay_Les_Pansexual_Straight" ~ 6,
      sex_or_new_4             == "Bisexual_Gay_Les_Queer_Straight" ~ 6,
      sex_or_new_4             == "Bisexual_Pansexual" ~ 6,
      sex_or_new_4             == "Demisexual_Queer" ~ 6,
      sex_or_new_4             == "Pansexual_Queer" ~ 6
    ))

ds_teen$sex_or_cat_4 <- factor(ds_teen$sex_or_cat_4,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Straight", "Bisexual", "Gay_Les",
                                          "Pansexual", "Questioning", "Another"))                                

tab1(ds_teen$sex_or_cat_4, exclude = NULL) # examine

# sexual orientation recode
ds_teen <- ds_teen %>%
  dplyr::mutate(
    sex_or_cat_R_4 = case_when(
      sex_or_new_4             == "Straight" ~ 1,
      sex_or_new_4             == "Bisexual" ~ 2,
      sex_or_new_4             == "Gay_Les" ~ 2,
      sex_or_new_4             == "Pansexual" ~ 2,
      sex_or_new_4             == "Questioning" ~ 3,
      sex_or_new_4             == "Queer" ~ 2,
      sex_or_new_4             == "Demisexual" ~ 2,
      sex_or_new_4             == "Other_SexOr" ~ 2,
      sex_or_new_4             == "Queer_Straight" ~ 2,
      sex_or_new_4             == "Bisexual_Demisexual_Pansexual" ~ 2,
      sex_or_new_4             == "Bisexual_Gay_Les" ~ 2,
      sex_or_new_4             == "Bisexual_Gay_Les_Queer_Other_SexOr" ~ 2,
      sex_or_new_4             == "Bisexual_Pansexual_Queer" ~ 2,
      sex_or_new_4             == "Bisexual_Queer" ~ 2,
      sex_or_new_4             == "Bisexual_Straight" ~ 2,
      sex_or_new_4             == "Demisexual_Gay_Les" ~ 2,
      sex_or_new_4             == "Demisexual_Pansexual" ~ 2,
      sex_or_new_4             == "Bisexual_Demisexual_Queer" ~ 2,
      sex_or_new_4             == "Bisexual_Gay_Les_Pansexual_Straight" ~ 2,
      sex_or_new_4             == "Bisexual_Gay_Les_Queer_Straight" ~ 2,
      sex_or_new_4             == "Bisexual_Pansexual" ~ 2,
      sex_or_new_4             == "Demisexual_Queer" ~ 2,
      sex_or_new_4             == "Pansexual_Queer" ~ 2
    ))

# examine wave 4 participants who solely reported Other sexual orientation
so_write_in_4 <- ds_teen[ds_teen$sex_or_new_4 == "Other_SexOr",]
table(so_write_in_4$Other_SexOr_Open_4, exclude = NULL)

ds_teen$sex_or_cat_R_4 <- factor(ds_teen$sex_or_cat_R_4,
                                 levels = c(1,2,3),
                                 labels = c("Straight", "Sexual_diverse", "Questioning")) 

epiDisplay::tab1(ds_teen$sex_or_cat_R_4, cum.percent = TRUE)



# Gender Identity, Time 4

epiDisplay::tab1(ds_teen$Male_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Female_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Non_Binary_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Trans_Male_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Trans_Female_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Gender_Queer_4, cum.percent = TRUE)
epiDisplay::tab1(ds_teen$Other_Gender_4, cum.percent = TRUE)

# select variables
ds_gender_4 <- ds_teen |> 
  dplyr::select(ID, Male_4, Female_4, Non_Binary_4, Trans_Male_4, Trans_Female_4, Gender_Queer_4, Other_Gender_4)

# remove "_prs" from column names
names(ds_gender_4) <- gsub("_4", "", names(ds_gender_4), fixed = TRUE)

ds_gender_w4 <- ds_gender_4 |>
  pivot_longer(!ID, names_to = "gender", values_to = "count") |> # make data long by the categories
  mutate(gender4 = ifelse(count == 1, gender, NA)) |>  # place actual name of the category in column based on count column in new column called gender1
  subset(select = -count) |>  # remove count column beause we dont need it anymore
  pivot_wider(names_from = gender, values_from = gender4) |> # turn data back wide by the categories
  unite(gender_new_4, Male:Other_Gender, sep='_', na.rm = TRUE) # collapse the columns, separate words with "_", and remove NAs

epiDisplay::tab1(ds_gender_w4$gender_new_4,    main = "Gender", cum.percent = TRUE) 

# create data frame to count the number of people in each category 
ds_gender_grp_4 <- ds_gender_w4 |> 
  group_by(gender_new_4) |>
  summarise(gender_count = length(gender_new_4))

ds_gender_grp_4 # examine


# join gender data with original data using ResponseId         
ds_teen <- left_join(ds_teen, ds_gender_w4, by = "ID")
names(ds_teen)

tab1(ds_teen$gender_new_4) # examine

# combining gender
ds_teen <- ds_teen %>%
  dplyr::mutate(
    gender_cat_4 = case_when(
      gender_new_4             == "Female" ~ 1,
      gender_new_4             == "Male" ~ 2,
      gender_new_4             == "Male_Trans_Female" ~ 3,
      gender_new_4             == "Male_Trans_Male" ~ 3,
      gender_new_4             == "Trans_Female" ~ 3,
      gender_new_4             == "Trans_Male" ~ 3,
      gender_new_4             == "Female_Non_Binary" ~ 3,
      gender_new_4             == "Male_Non_Binary" ~ 3,
      gender_new_4             == "Non_Binary" ~ 3,
      gender_new_4             == "Non_Binary_Gender_Queer" ~ 3,
      gender_new_4             == "Other_Gender" ~ 3,
      gender_new_4             == "Gender_Queer" ~ 3,
      gender_new_4             == "Male_Female_Non_Binary_Gender_Queer" ~ 3,
      gender_new_4             == "Male_Female" ~ 3,
      gender_new_4             == "Male_Other_Gender" ~ 3
      
    ))

ds_teen$gender_cat_4 <- factor(ds_teen$gender_cat_4,
                               levels = c(1,2,3),
                               labels = c("Female", "Male", "Another Gender"))                   

epiDisplay::tab1(ds_teen$gender_cat_4, cum.percent = TRUE)


# cross-classification of sex assigned at birth and gender identity, 2-step measure
# crosstab of ASAB (1=Male, 2=Female) and gender ID
xtab_gid <- table(ds_teen$Sex_4, ds_teen$gender_new_4)
addmargins(xtab_gid)

# crosstab of ASAB (1=Male, 2=Female) and gender_cat_4
xtab_gid_cat <- table(ds_teen$Sex_4, ds_teen$gender_cat_4)
addmargins(xtab_gid_cat)

### new measure of gender ID based on cross-classification
ds_teen$gender_2step_4 <- NA
ds_teen$gender_2step_4[ds_teen$Sex_4 == 1 & ds_teen$gender_cat_4 == "Male"] <- 1
ds_teen$gender_2step_4[ds_teen$Sex_4 == 2 & ds_teen$gender_cat_4 == "Female"] <- 2
ds_teen$gender_2step_4[ds_teen$Sex_4 == 1 & ds_teen$gender_cat_4 == "Another Gender"] <- 3
ds_teen$gender_2step_4[ds_teen$Sex_4 == 2 & ds_teen$gender_cat_4 == "Another Gender"] <- 3
ds_teen$gender_2step_4[ds_teen$Sex_4 == 1 & ds_teen$gender_cat_4 == "Female"] <- 3  # 9 respondents reported Male ASAB and Female Gender ID
ds_teen$gender_2step_4[ds_teen$Sex_4 == 2 & ds_teen$gender_cat_4 == "Male"] <- 3  # 14 respondents reported Female ASAB and Male Gender ID

table(ds_teen$gender_2step_4, exclude = NULL) # check #s


ds_teen$gender_2step_4 <- factor(ds_teen$gender_2step_4,
                               levels = c(1,2,3),
                               labels = c("Male", "Female", "Gender Diverse"))                   

epiDisplay::tab1(ds_teen$gender_2step_4, cum.percent = TRUE)










































































