# Data pre-processing

#### Load the packages #######

library(probably) # Calibration analysis
library(haven) # Open SAS files
library(tidyverse) # Data Manipulation
library(lubridate) # Dates Manipulation
library(janitor) # Fast statistics calculations (cross-tabs)
library(tidymodels) # Modelling
library(finetune) # Fine tuning of models
library(xgboost) # Extreme Gradient Boosting Trees
library(vip) # Variable Importance Tools
library(riskRegression) # Some evaluation and calibration tools
library(dcurves) # Decision curves
library(patchwork) # Merging Plots
library(DALEX) # Model Explainability
library(DALEXtra) # Additional Exlainability
library(shapviz) # Shapley Values
library(rms) # For Classic Logistic Regression
library(DALEX) # For explainability
library(patchwork) # For figure merging
library(yardstick) #Metrics
library(dcurves) # Decision Curve Analysis
library(missRanger) # Imputation
library(ingredients) # PDP plots
library(DALEX) # Explainability of the model
library(DataExplorer) # Data exploration
library(SHAPforxgboost) # Shapleys for XGBoost
library(hstats) # Interaction Statistics
library(tidylog) # Logs of pre-processing
library(arrow) # Writing parquet files

#########################


################################
####### Data extraction ########
################################

# Access the directory for the data

# Overall population

nogle23 <- read_sas("nogle2023.sas7bdat")

# 11-year follow-up data for children

dnbc_11_children <- read_sas("y11_samlet_barn.sas7bdat")

# 11-year follow-up data for the adults

dnbc_11_adults <- read_sas("y11_samlet_voksen.sas7bdat")

# Read the 18-year follow-up data (That's where we get the outcome)

dnbc_18 <- read_sas("y18_samlet.sas7bdat")

# First interview of mather

adults_i1 <- read_sas("i1_samlet.sas7bdat")

# Children's stress scale

stress_11 <- read_sas("sic_y11.sas7bdat")

# Parity

parents <- read_dta("mat_char_birth.dta")

# Various eating disorders & behaviours

ED_11 <- read_sas("ed_11y_fu.sas7bdat") 

ED_18 <- read_sas("ed_18y_fu.sas7bdat") 


# BMI data at 7 & 11 years follow-up

BMI_7 <- read_sas("bmi_7y_fu.sas7bdat") #BMI at 7-year follow up

BMI_11 <- read_sas("BMI_11y_fu.sas7bdat") # BMI at 11-year follow up


# Parental Education (11-y follow up)

# Parental Education

parental_education <- read_sas("hh_ha_edu.sas7bdat")

# Paternal Education

paternal_education <- read_sas("pat_ha_edu.sas7bdat")

# Maternal Education

maternal_education <- read_sas("mat_ha_edu.sas7bdat")

# Income at 11years of age

income_11 <- read_sas("hh_inc_quart_age11.sas7bdat")

# Feeding disorders

feeding_disorders <- read_sas("indexchild_feedingillness.sas7bdat")

# Pre-Conceptional Folic Acid

folic <- read_sas("dnbc_folin.sas7bdat")

# Grab urbanicity at age 11

urbanicity <- read_sas("urb_year.sas7bdat")


########### Childhood adversities ###########

parental_somatic_illness <- read_dta("A_1_Parental_somatic_illness.dta")

sibling_somatic_illness <- read_dta("A_2_sibling_somatic_illness.dta")

parental_psychiatric_illness <- read_dta("A_3_parental_psychiatric_illness.dta")

sibling_psychiatric_illness <- read_dta("A_4_sibling_psychiatric_illness.dta")

foster_care <- read_dta("A_5_foster_care.dta")

death_of_parent <- read_dta("A_6_death_of_parent.dta")

death_of_sibling <- read_dta("A_7_death_of_sibling.dta")

parental_separation <- read_dta("A_8_parental_separation.dta")

parental_long_term_unemployment <- read_dta("A_9_parental_long-term_unemployment.dta")

poverty <- read_dta("A_10_poverty.dta")

alcohol_abuse <- read_dta("A_11_parental_alcohol_abuse.dta")

drug_abuse <- read_dta("A_12_parental_drug_abuse.dta")


#################################

# Eating disorders in the registers

ed_lpr <- read_sas("eating_disorders.sas7bdat")


# Maternal drinking

mat_drink <- read_sas("mat_drink_i1.sas7bdat")

# Maternal smoking

mat_smoke <- read_sas("mat_smok_i1.sas7bdat")

# Parental psychiatric disorders

diagn_parental_psychiatric <- read_sas("parent_psych_noed.sas7bdat")

# Parental eating disorders

diagn_parental_ed <- read_sas("parent_ed.sas7bdat")

# Child's psychiatric disorders

diagn_children_psychiatric <- read_sas("children_psych_noed.sas7bdat")

# Autoimmune Diseases Diagnosed

diagn_autoimmune <- read_sas("autoimmune_diseases.sas7bdat")

# Autoinflammatory Conditions

diagn_autoinfl <- read_sas("autoinflammatory_conditions.sas7bdat")

# Implementing the SDQ scales also at 7 and 11-y follow up

sdq11_child <-  read_sas("sdqchild_y11.sas7bdat")

sdq11_parent <- read_sas("sdqparents_y11.sas7bdat")

sdq7_parent <- read_sas("sdq_y7.sas7bdat")

############################################################################

# Initial restriction of the population

# We should only keep the children that have 
# agreed to participate and those who answered the 11-year 
# follow up along with their mothers


nogle23 <- nogle23 |> 
  filter(y11_besvar_v == 1, y11_besvar_b == 1,
         outcom_f == 1 | 
           outcom_f == 18 | outcom_f == 22 | (outcom_f == 21 & udfald_barn != 0))

# After restricting to children (alive) and parents answered in 11-y follow up:

# These are people for whom we have consent to analyze the data and have answered the items

dnbc_11_children <- dnbc_11_children |> 
  filter(lbgravff %in% nogle23$lbgravff)

# Now match the DNBC-11 adults with the children above

dnbc_11_adults <- dnbc_11_adults |> 
  filter(lbgravff %in% dnbc_11_children$lbgravff)

# Put the cpr of these children in the original dataframe

cprs <- nogle23 |> select(lbgravff,bcpr)    

# Merge the CPR numbers to our existing DNBC database

dnbc_11_children <- dnbc_11_children |> 
  left_join(cprs,by = "lbgravff") |> 
  relocate(bcpr,.after = lbgravff)


#############################################

# Re-coding of some predictors

################  Age variable ##########################

dnbc_11_children |> 
  tabyl(E001) |> 
  mutate(percent = round(percent,3))  

# Let's change age to be either 11,12 or something else

dnbc_11_children <- dnbc_11_children |> 
  mutate(Age = case_when(E001 == 99  ~ NA,
                         E001 == 2 ~ "11 years old", 
                         E001 == 3 ~ "12 years old",
                         .default = "Else")) |> 
  mutate_at(vars(Age),as.factor) |> 
  relocate(Age,.before = E001) |> 
  select(-E001)

################## Sex ########################

dnbc_11_children |> 
  tabyl(E_SEX)

# Let's convert it also into a factor

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sex = if_else(E_SEX == 1, "Male", "Female")) |> 
  mutate_at(vars(Sex), as.factor) |> 
  relocate(Sex, .before = E_SEX) |> 
  select(-E_SEX)


############### Family structure ####################

# With whom are you living with

dnbc_11_children |> 
  tabyl(E003) |> 
  mutate(percent = round(percent,2))

# Reconstruct into a factor

dnbc_11_children <- dnbc_11_children |> 
  mutate(Family_Living = case_when(E003 == 99 ~ NA,
                                   E003 == 1 ~ "Both of my parents", 
                                   .default = "Other")) |> 
  mutate_at(vars(Family_Living),as.factor) |> 
  relocate(Family_Living,.before = E003) |> 
  select(-E003)


############### Age at Parental Divorce ##################

dnbc_11_children |> 
  tabyl(E005) |> 
  mutate(percent = round(percent,3))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Divorce_Age = case_when(E005 == 100 ~ "Not Divorced",
                                 E005 == 99 ~ NA,
                                 E005 %in% 1:5 ~ "Younger than 5 years old",
                                 E005 %in% 6:10 ~ "Between 6 to 10 years old",
                                 E005 %in% 11:14 ~ "Older than 10 years old")) |> 
  mutate_at(vars(Divorce_Age),as.factor) |> 
  relocate(Divorce_Age, .before = E005) |> 
  select(-E005)


############### Biological Siblings ######################

dnbc_11_children |> 
  tabyl(E007)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Bio_Sibling = case_when(E007 == 99 ~ NA,
                                 E007 == 1 ~ "Have a biological sibling", 
                                 .default = "No")) |> 
  mutate_at(vars(Bio_Sibling), as.factor) |> 
  relocate(Bio_Sibling, .before = E007) |> 
  select(-E007)


############## Room-Share ################

dnbc_11_children |> 
  tabyl(E010) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Room_Share = case_when(E010 == 99 ~ NA,
                                E010 == 1 ~"I do not share room",
                                .default = "I share")) |> 
  mutate_at(vars(Room_Share), as.factor) |> 
  relocate(Room_Share, .before = E010) |> 
  select(-E010)


#################### Social relations and well-being ##################

# How many of your close friends are boys

dnbc_11_children |> 
  tabyl(E011_1) |> 
  mutate(percent = round(percent,3))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Close_Male_Friends = case_when(E011_1 == 1 ~ "None", 
                                        E011_1 == 2 ~ "One",
                                        E011_1 == 3 ~ "Two",
                                        E011_1 == 4 ~ "Three or more",
                                        E011_1 == 99 ~ NA)) |> 
  mutate_at(vars(Close_Male_Friends), as.factor) |> 
  relocate(Close_Male_Friends, .before = E011_1) |> 
  select(-E011_1)


# How many of your close friends are females

dnbc_11_children |> 
  tabyl(E011_2) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Close_Female_Friends = case_when(E011_2 == 1 ~ "None", 
                                          E011_2 == 2 ~ "One",
                                          E011_2 == 3 ~ "Two",
                                          E011_2 == 4 ~ "Three or more",
                                          E011_2 == 99 ~ NA)) |> 
  mutate_at(vars(Close_Female_Friends), as.factor) |> 
  relocate(Close_Female_Friends, .before = E011_2) |> 
  select(-E011_2)



# How easy or difficult to make new friends

dnbc_11_children |> 
  tabyl(E012) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Friendship_Creation = case_when(
    E012 == 1 | E012 == 2 ~ "Easy or Very Easy",
    E012 == 99 ~ NA,
    .default = "Difficult or Very Difficult")) |> 
  mutate_at(vars(Friendship_Creation),as.factor) |> 
  relocate(Friendship_Creation, .before = E012) |> 
  select(-E012)


##################### Bullying #######################

dnbc_11_children |> 
  tabyl(E014_1)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Bullying_Receive = case_when(
    E014_1 == 1 | E014_1 == 2 ~ "Bullied",
    E014_1 == 99 ~ NA,
    .default = "Not Bullied")) |> 
  mutate_at(vars(Bullying_Receive),as.factor) |> 
  relocate(Bullying_Receive, .before = E014_1) |> 
  select(-E014_1)


dnbc_11_children |> 
  tabyl(Bullying_Receive)


###################### Bullying Others #######################

dnbc_11_children |> 
  tabyl(E014_2)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Bullying_Given = case_when(
    E014_2 == 1 | E014_2 == 2 ~ "Have Bullied",
    E014_2 == 99 ~ NA,
    .default = "Have not Bullied")) |> 
  mutate_at(vars(Bullying_Given),as.factor) |> 
  relocate(Bullying_Given, .before = E014_2) |> 
  select(-E014_2)


########################### Schooling ########################

# Rating of own school achievements

dnbc_11_children |> 
  tabyl(E015_1) |> 
  mutate(percent = round(percent,2))



dnbc_11_children <- dnbc_11_children |> 
  mutate(School_Performance = case_when(
    E015_1 == 99 ~ NA,
    E015_1 == 1  ~ "Excellent",
    E015_1 == 2 ~ "Good",
    E015_1 == 3 | E015_1 == 4 ~ "Not Good")) |>
  mutate_at(vars(School_Performance),as.factor) |> 
  relocate(School_Performance, .before = E015_1) |> 
  select(-E015_1)


# Sports performance

dnbc_11_children |> 
  tabyl(E015_4) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Sports_Performance = case_when(
    E015_4 == 99 ~ NA,
    E015_4 == 1  ~ "Excellent",
    E015_4 == 2 ~ "Good",
    E015_4 == 3 | E015_4 == 4 | E015_4 == 99 ~ "Not Good")) |>
  mutate_at(vars(Sports_Performance),as.factor) |> 
  relocate(Sports_Performance, .before = E015_4) |> 
  select(-E015_4)


####################### General Well-Being #########################

# Stomach Pains


dnbc_11_children |> 
  tabyl(E022_5) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Stomach_Pains = case_when(
    E022_5 == 1 | E022_5 == 2  ~ "Very Often or Often",
    E022_5 == 3 ~ "Sometimes",
    E022_5 == 4 ~ "Never",
    E022_5 == 99 ~ NA)) |>
  mutate_at(vars(Stomach_Pains),as.factor) |> 
  relocate(Stomach_Pains, .before = E022_5) |> 
  select(-E022_5)


# Loneliness

dnbc_11_children |> 
  tabyl(E022_6) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Loneliness_Feeling = case_when(
    E022_6 == 1 | E022_6 == 2  ~ "Very Often or Often",
    E022_6 == 3 ~ "Sometimes",
    E022_6 == 4 ~ "Never",
    E022_6 == 99 ~ NA)) |>
  mutate_at(vars(Loneliness_Feeling),as.factor) |> 
  relocate(Loneliness_Feeling, .before = E022_6) |> 
  select(-E022_6)


# Do you express your sadness to the rest

dnbc_11_children |> 
  tabyl(E022_17) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Showing_Sadness = case_when(
    E022_17 == 1 | E022_17 == 2  ~ "Very Often or Often",
    E022_17 == 3 ~ "Sometimes",
    E022_17 == 4 ~ "Never",
    E022_17 == 99 ~ NA)) |>
  mutate_at(vars(Showing_Sadness),as.factor) |> 
  relocate(Showing_Sadness, .before = E022_17) |> 
  select(-E022_17)


#  Is there an adult to talk to in hard times

dnbc_11_children |> 
  tabyl(E022_19) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Adult_To_Talk_To = case_when(
    E022_19 == 1 | E022_19 == 2  ~ "Very Often or Often",
    E022_19 == 3 ~ "Sometimes",
    E022_19 == 4 ~ "Never",
    E022_19 == 99 ~ NA)) |>
  mutate_at(vars(Adult_To_Talk_To),as.factor) |> 
  relocate(Adult_To_Talk_To, .before = E022_19) |> 
  select(-E022_19)




# Negative events in children's life: they start with E023_1 :E023_15

# We need this to be factors

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(E023_1:E023_15),as.factor) |> 
  mutate_at(vars(starts_with("E023_")), ~case_match(., "1" ~ "Yes", "2" ~ "No")) |>   
  mutate_at(vars(E023_1:E023_15),as.factor)


################# Sleeping related questions #####################

# Do you feel that you don't get enough sleep


dnbc_11_children |> 
  tabyl(E028_1) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Lack_Of_Sleep = case_when(
    E028_1 == 1 ~ "Almost Always or Always",
    E028_1 == 2 ~ "Sometimes",
    E028_1 == 3 ~ "Rarely/Never",
    E028_1 == 99 ~ NA)) |>
  mutate_at(vars(Lack_Of_Sleep),as.factor) |> 
  relocate(Lack_Of_Sleep, .before = E028_1) |> 
  select(-E028_1)


# Do you find it difficult to wake up in the morning?

dnbc_11_children |> 
  tabyl(E028_8) |> 
  mutate(percent = round(percent,2))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Difficult_To_WakeUp = case_when(
    E028_8 == 99 ~ NA,
    E028_8 == 1  ~ "Almost Always or Always",
    E028_8 == 2 ~ "Sometimes",
    E028_8 == 3 ~ "Rarely/Never")) |>
  mutate_at(vars(Difficult_To_WakeUp),as.factor) |> 
  relocate(Difficult_To_WakeUp, .before = E028_8) |> 
  select(-E028_8)


# Create a variable to measure amount of sleep in children

sleep_df <- dnbc_11_children |> 
  select(lbgravff,E024,E025,E026,E027) |> 
  mutate(E024 = case_when(E024 == 1 ~ ("19:00"),
                          E024 == 2 ~ ("19:30"),
                          E024 == 3 ~ ("20:00"),
                          E024 == 4 ~ ("20:30"),
                          E024 == 5 ~ ("21:00"),
                          E024 == 6 ~ ("21:30"),
                          E024 == 7 ~ ("22:00"),
                          E024 == 8 ~ ("22:30"),
                          E024 == 9 ~ ("23:00"),
                          E024 == 10 ~ ("23:30"),
                          E024 == 11 ~ ("24:00"),
                          E024 == 12 ~ ("24:30"),
                          E024 == 13 ~ ("01:00"),
                          E024 == 14 ~ ("02:00"),
                          E024 == 15 ~ ("03:00"),
                          E024 == 99 ~ NA)) |> 
  mutate(E024 = hm(E024)) |> 
  mutate(E025 = case_when(E025 == 1 ~ ("19:00"),
                          E025 == 2 ~ ("19:30"),
                          E025 == 3 ~ ("20:00"),
                          E025 == 4 ~ ("20:30"),
                          E025 == 5 ~ ("21:00"),
                          E025 == 6 ~ ("21:30"),
                          E025 == 7 ~ ("22:00"),
                          E025 == 8 ~ ("22:30"),
                          E025 == 9 ~ ("23:00"),
                          E025 == 10 ~ ("23:30"),
                          E025 == 11 ~ ("24:00"),
                          E025 == 12 ~ ("24:30"),
                          E025 == 13 ~ ("01:00"),
                          E025 == 14 ~ ("02:00"),
                          E025 == 15 ~ ("03:00"),
                          E025 == 99 ~ NA)) |> 
  mutate(E025 = hm(E025)) |> 
  mutate(E026 = case_when(E026 == 1 ~ ("5:00"),
                          E026 == 2 ~ ("5:30"),
                          E026 == 3 ~ ("6:00"),
                          E026 == 4 ~ ("6:30"),
                          E026 == 5 ~ ("7:00"),
                          E026 == 6 ~ ("7:30"),
                          E026 == 7 ~ ("8:00"),
                          E026 == 8 ~ ("8:30"),
                          E026 == 9 ~ ("9:00"),
                          E026 == 10 ~ ("9:30"),
                          E026 == 11 ~ ("10:00"),
                          E026 == 12 ~ ("11:00"),
                          E026 == 13 ~ ("12:00"),
                          E026 == 14 ~ ("13:00"),
                          E026 == 99 ~ NA)) |> 
  mutate(E026 = hm(E026)) |> 
  mutate(E027 = case_when(E027 == 1 ~ ("5:00"),
                          E027 == 2 ~ ("5:30"),
                          E027 == 3 ~ ("6:00"),
                          E027 == 4 ~ ("6:30"),
                          E027 == 5 ~ ("7:00"),
                          E027 == 6 ~ ("7:30"),
                          E027 == 7 ~ ("8:00"),
                          E027 == 8 ~ ("8:30"),
                          E027 == 9 ~ ("9:00"),
                          E027 == 10 ~ ("9:30"),
                          E027 == 11 ~ ("10:00"),
                          E027 == 12 ~ ("11:00"),
                          E027 == 13 ~ ("12:00"),
                          E027 == 14 ~ ("13:00"),
                          E027 == 99 ~ NA)) |> 
  mutate(E027 = hm(E027)) |>
  rename(weekday_bed = "E024",
         weekend_bed = "E025",
         weekday_wake = "E026",
         weekend_wake = "E027") |> 
  mutate(sleep_weekday = as.numeric(weekday_wake - weekday_bed)/3600,
         sleep_weekend = as.numeric(weekend_wake - weekend_bed)/3600) |> 
  mutate(sleep_weekday = if_else(sleep_weekday < 0, sleep_weekday + 24, sleep_weekday)) |>  
  mutate(sleep_weekend = if_else(sleep_weekend < 0, sleep_weekend + 24, sleep_weekend)) |> 
  mutate(avg_sleep = (5*sleep_weekday + 2*sleep_weekend)/7)

sleep_df <- sleep_df |> 
  rename(Sleeping_Hours = "avg_sleep")

# Let's put the Sleeping hours into dnbc_11_children

dnbc_11_children <- dnbc_11_children |> 
  left_join(sleep_df |> select(lbgravff,Sleeping_Hours),by = "lbgravff")

##################### Dietary Habits ####################

# How ofter do you have breakfast

dnbc_11_children |> 
  tabyl(E030_1) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Breakfast = case_when(E030_1 == 1 ~ "Every Day",
                               E030_1 == 2 ~ "6 Days a week",
                               E030_1 == 99 ~ NA,
                               .default = "Less than 6 days a week")) |> 
  mutate_at(vars(Breakfast), as.factor) |> 
  relocate(Breakfast, .before = E030_1) |> 
  select(-E030_1)


# How often do you have lunch


dnbc_11_children |> 
  tabyl(E030_2) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Lunch = case_when(E030_2 == 1 ~ "Every Day",
                           E030_2 == 2 ~ "6 Days a week",
                           E030_2 == 99 ~ NA,
                           .default = "Less than 6 days a week")) |> 
  mutate_at(vars(Lunch), as.factor) |>
  relocate(Lunch, .before = E030_2) |> 
  select(-E030_2)


# Dinner 

dnbc_11_children |> 
  tabyl(E030_3) |> 
  mutate(percent = round(percent,3))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Dinner = case_when(E030_3 == 1 ~ "Every Day",
                            E030_3 == 2 ~ "6 Days a week",
                            E030_3 == 99 ~ NA,
                            .default = "Less than 6 days a week")) |> 
  mutate_at(vars(Dinner), as.factor) |>
  relocate(Dinner, .before = E030_3) |> 
  select(-E030_3)


# Snack Frequency

dnbc_11_children |> 
  tabyl(E031) |> 
  mutate(percent = round(percent,3))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Snack = case_when(E031 == 1 | E031 == 2 | E031 ==3 ~ "3 to 5 times a day",
                           E031 == 4 | E031 == 5 ~ "1 to 2 times a day",
                           E031 == 6 ~ "Not every day",
                           E031 == 99 ~ NA)) |> 
  mutate_at(vars(Snack), as.factor) |>
  relocate(Snack, .before = E031) |> 
  select(-E031)



###### Fat Feeling #########

dnbc_11_children |> 
  tabyl(E033_1)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Feeling_Fat = case_when(E033_1 == 1 ~ "Every Day",
                                 E033_1 == 2 | 
                                 E033_1 == 3 ~ "Often or Sometimes",
                                 E033_1 == 99 ~ NA,
                                 .default = "Rarely or Never")) |> 
  mutate_at(vars(Feeling_Fat), as.factor) |>
  relocate(Feeling_Fat, .before = E033_1) |> 
  select(-E033_1)


###### How often you felt you wannted to be thinner ###########

dnbc_11_children |> 
  tabyl(E033_2)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Want_For_Thin = case_when(E033_2 == 1 ~ "Every Day",
                                   E033_2 == 2 | 
                                   E033_2 == 3 ~ "Often or Sometimes",
                                   E033_2 == 99 ~ NA,
                                   .default = "Rarely or Never")) |> 
  mutate_at(vars(Want_For_Thin), as.factor) |>
  relocate(Want_For_Thin, .before = E033_2) |> 
  select(-E033_2)


# Have you often gone to an eating binge

dnbc_11_children |> 
  tabyl(E037) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Binge_Eating = case_when(E037 == 1 ~ "Never",
                                  E037 == 2 ~ "1-3 times per month",
                                  E037 == 99 ~ NA,
                                  .default = "Once or more in a week")) |> 
  mutate_at(vars(Binge_Eating), as.factor) |>
  relocate(Binge_Eating, .before = E037) |> 
  select(-E037)

# Did you eat until your stomach hurt ?

dnbc_11_children |> 
  tabyl(E038_2)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Stomach_Binge = case_when(E038_2 == 1 ~ "Yes",
                                   E038_2 == 99 ~ NA,
                                   E038_2 == 2 ~ "No",
                                   .default = "Not Applicable")) |> 
  mutate_at(vars(Stomach_Binge), as.factor) |>
  relocate(Stomach_Binge, .before = E038_2) |> 
  select(-E038_2)



############### Tried smoking #################


dnbc_11_children |> 
  tabyl(E071) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Smoking = case_when(E071 == 1 ~ "Smoked", 
                             E071 == 2 ~ "Not Smoked",
                             E071 == 99 ~ NA)) |> 
  mutate_at(vars(Smoking),as.factor) |> 
  select(-E071)


############# Health-related questions  ###############


# Self-Rated Health

dnbc_11_children |> 
  tabyl(E083)


dnbc_11_children <- dnbc_11_children |> 
  mutate(Self_Rated_Health = case_when(E083 == 1 ~ "Excellent",
                                       E083 == 2 ~ "Good",
                                       E083 == 3 | E083 == 4  ~ "Not so Good or Poor",
                                       E083 == 99 ~ NA)) |> 
  mutate_at(vars(Self_Rated_Health), as.factor) |>
  relocate(Self_Rated_Health, .before = E083) |> 
  select(-E083)



# Physical Condition

dnbc_11_children |> 
  tabyl(E084)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Self_Physical_Condition = case_when(E084 == 1 ~ "Excellent",
                                             E084 == 2 ~ "Good",
                                             E084 == 3 | E084 == 4 | E084 == 5  ~ "Not so Good or Poor or Very Poor",
                                             E084 == 99 ~ NA)) |> 
  mutate_at(vars(Self_Physical_Condition), as.factor) |>
  relocate(Self_Physical_Condition, .before = E084) |> 
  select(-E084)


################### Questions of Athleticism and Activity ##################

# School Break Movement

dnbc_11_children |> 
  tabyl(E059)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Movement_School = if_else(E059 == 99, NA, E059)) |> 
  mutate_at(vars(Movement_School),as.factor) |> 
  select(-E059)

levels(dnbc_11_children$Movement_School) <- c("Very Active", "Active", "Normal", "Not Active")


#Leisure Movement

dnbc_11_children |> 
  tabyl(E060)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Movement_Leisure = if_else(E060 == 99, NA, E060)) |> 
  mutate_at(vars(Movement_Leisure),as.factor) |> 
  select(-E060)

levels(dnbc_11_children$Movement_Leisure) <- c("Very Active", "Active", "Normal", "Not Active")


# Gymnastics

dnbc_11_children <- dnbc_11_children |> 
  mutate(Gymnastics_Dancing = case_when(
    E062_7 == 99 | E062_8 == 99 ~ NA,
    E062_7 == 1 | E062_8 == 1 ~ "Does Gymnastics or Dancing", 
    .default = "Does not do Gymnastics or Dancing")) |> 
  mutate(Frequency_Gymnastics_Dancing = case_when(
    E062_7A == 3 | E062_7A == 4 | 
      E062_7A == 5 | E062_8A == 3 |
      E062_8A == 4 | E062_8A == 5 ~ "Practicing at least 3 times per week", 
    E062_7A == 99 | E062_8A == 99 ~ NA,
    .default = "Not Practicing or Practicing Rarely")) |> 
  mutate_at(vars(Gymnastics_Dancing,Frequency_Gymnastics_Dancing),as.factor)


# Body Dissatisfaction Score

# Body Disatisfaction

dnbc_11_children <-dnbc_11_children |> 
  mutate(E125_1 = if_else(E125_1 == 99 | E125_1 == 100, NA, E125_1)) |> 
  mutate(E125_2 = if_else(E125_2 == 99 | E125_2 == 100, NA, E125_2)) |> 
  mutate(Body_Score = E125_2 - E125_1)

# The way you want to look - The way you look

dnbc_11_children <-dnbc_11_children |> 
  mutate(E126_1 = if_else(E126_1 == 99 | E126_1 == 100, NA, E126_1)) |> 
  mutate(E126_2 = if_else(E126_2 == 99 | E126_2 == 100, NA, E126_2)) |> 
  mutate(Body_Score_Males = E126_2 - E126_1)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Body_Score = if_else(is.na(Body_Score) & !is.na(Body_Score_Males), Body_Score_Males, Body_Score ))


dnbc_11_children <- dnbc_11_children |> 
  select(-Body_Score_Males)


################### Self Harm #####################


dnbc_11_children <- dnbc_11_children |> 
  mutate(Self_Harm = case_when(E116_3 == 1 ~ "Yes", 
                               E116_3 == 99 ~ NA,
                               E116_3 == 2 ~ "No")) |> 
  mutate_at(vars(Self_Harm),as.factor) |> 
  select(-E116_3)


####################### Depression Proxy ########################

dnbc_11_children <- dnbc_11_children |> 
  mutate(Depression_Feelings  = case_when(E112 == 1 ~ "Yes", 
                                          E112 == 99 ~ NA,
                                          E112 == 2 ~ "No")) |> 
  mutate(Loss_Of_Interest = case_when(E114 == 1 ~ "Yes", 
                                      E114 == 99 ~ NA,
                                      E114 == 2 ~ "No")) |> 
  mutate_at(vars(Depression_Feelings, Loss_Of_Interest),as.factor) |> 
  select(-E112,-E114)


################### Perfectionist Related Questions ##################

## We will create a composite variable of Obsessive Compulsive Disorder Items

# E086_1 until E086_5

dnbc_11_children <- dnbc_11_children |> 
  mutate(OCD_Symptoms = case_when(
    E086_1 == 1 | E086_2 == 1 | E086_3 == 1 | 
      E086_4 == 1 | E086_5 == 1 ~ "Frequent OCD Symptoms",
    E086_1 == 2 | E086_2 == 2 | E086_3 == 2 | 
      E086_4 == 2 | E086_5 == 2 ~ "Occasional OCD Symptoms",
    E086_1 == 3 | E086_2 == 3 | E086_3 == 3 |  
      E086_4 == 3 | E086_5 == 3 ~ "Abscence of OCD Symptoms")) |> 
  mutate_at(vars(OCD_Symptoms),as.factor) |> 
  relocate(OCD_Symptoms, .before = E086_1)


############ Musculosceletal Pain ################

dnbc_11_children |> 
  tabyl(E089) |> 
  mutate(percent = round(percent,2))


dnbc_11_children <- dnbc_11_children |> 
  mutate(Neck_Pain = case_when(E089 == 1 | E089 == 2 ~ 
                                 "Often or occasionally",
                               E089 == 3 ~ "Rarely",
                               E089 == 4 ~ "Never",
                               E089 == 99 ~ NA)) |> 
  mutate_at(vars(Neck_Pain),as.factor) |> 
  relocate(Neck_Pain, .before = E089) |> 
  select(-E089)



dnbc_11_children <- dnbc_11_children |> 
  mutate(Middle_Back_Pain = case_when(E090 == 1 | E090 == 2 ~ 
                                        "Often or occasionally",
                                      E090 == 3 ~ "Rarely",
                                      E090 == 4 ~ "Never",
                                      E090 == 99 ~ NA)) |> 
  mutate_at(vars(Middle_Back_Pain),as.factor) |> 
  relocate(Middle_Back_Pain, .before = E090) |> 
  select(-E090)



dnbc_11_children <- dnbc_11_children |> 
  mutate(Low_Back_Pain = case_when(E091 == 1 | E091 == 2 ~ 
                                     "Often or occasionally",
                                   E091 == 3 ~ "Rarely",
                                   E091 == 4 ~ "Never",
                                   E091 == 99 ~ NA)) |> 
  mutate_at(vars(Low_Back_Pain),as.factor) |> 
  relocate(Low_Back_Pain, .before = E091) |> 
  select(-E091)


########### Gender Identity ###########


dnbc_11_children |> 
  tabyl(E119)


dnbc_11_children <- dnbc_11_children |> 
  mutate(Gender_Discomfort = case_when(E119 == 1  ~ "No",
                                       E119 == 99 ~ NA,
                                       .default = "Yes or Somewhat Yes")) |> 
  mutate_at(vars(Gender_Discomfort),as.factor) |> 
  relocate(Gender_Discomfort, .before = E119) |> 
  select(-E119)



####### Smartphone Usage ##########

# Where do you keep your phone when sleeping

dnbc_11_children |> 
  tabyl(E124)


dnbc_11_children <- dnbc_11_children |> 
  mutate(Smartphone_Placement = case_when(E124 == 1 | 
                                          E124 == 2 ~ "Close to me",
                                          E124 == 3 ~ "Further away from me",
                                          E124 == 99 ~ NA)) |> 
  mutate_at(vars(Smartphone_Placement),as.factor) |> 
  relocate(Smartphone_Placement,.before = E124) |> 
  select(-E124)


# Adding the child's stress scale

dnbc_11_children <- dnbc_11_children |> 
  left_join(stress_11 |> select(lbgravff,GMS),by = "lbgravff")


# Adding the parity

parents <- parents |> 
  rename(bcpr = PNR)

parents <- parents |> 
  select(bcpr, Parity = parity)

dnbc_11_children <- dnbc_11_children |> 
  left_join(parents, by = "bcpr")


# Adding the child's BMI variables
dnbc_11_children <- dnbc_11_children |> 
  left_join(BMI_7 |> select(lbgravff,bmi7),by = "lbgravff")

dnbc_11_children <- dnbc_11_children |> 
  left_join(BMI_11 |> select(lbgravff,bmi11),by = "lbgravff")


# Adding parental education

paternal_education$ha_edu_group_agg_age11 <- str_replace(paternal_education$ha_edu_group_agg_age11, "^[0-9]+\\s","")

paternal_education$paternal_ed <- paternal_education$ha_edu_group_agg_age11

paternal_education$paternal_ed[paternal_education$paternal_ed == ""] <- NA

paternal_education$paternal_ed <- as.factor(paternal_education$paternal_ed)

dnbc_11_children <- dnbc_11_children |> 
  left_join(paternal_education |> 
              select(bcpr = PNR, Paternal_Education = paternal_ed), 
            by = "bcpr")


# Adding Maternal Education

maternal_education$ha_edu_group_agg_age11 <- str_replace(maternal_education$ha_edu_group_agg_age11, "^[0-9]+\\s","")

maternal_education$maternal_ed <- maternal_education$ha_edu_group_agg_age11

maternal_education$maternal_ed[maternal_education$maternal_ed == ""] <- NA

maternal_education$maternal_ed <- as.factor(maternal_education$maternal_ed)


dnbc_11_children <- dnbc_11_children |> 
  left_join(maternal_education |> 
              select(bcpr = PNR, Maternal_Education = maternal_ed), 
            by = "bcpr")

# Adding the income

income_11 <- income_11 |> mutate(Income_Quartile = case_when(
  hh_inc_quart == "1 Lowest quartile" ~ "Lowest Quartile",
  hh_inc_quart == "2 Second lowest quartile" ~ "Second Lowest Quartile",
  hh_inc_quart == "3 Second highest quartile" ~ "Second Highest Quartile",
  hh_inc_quart == "4 Highest quartile" ~ "Highest Quartile",
  .default = NA)) |> 
  mutate_at(vars(Income_Quartile),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(income_11 |> 
              select(bcpr = PNR, Income_Quartile),
            by = "bcpr")

# Adding Maternal Education

parents_ed <- adults_i1 |> 
  mutate(Maternal_ED = if_else(A123 == 1 | A125 == 1, 
                               "Maternal Eating Disorder", 
                               "No Maternal Eating Disorder")) |> 
  mutate_at(vars(Maternal_ED),as.factor)


dnbc_11_children <- dnbc_11_children |> 
  left_join(parents_ed |> select(lbgravnr,Maternal_ED),by = "lbgravnr")

dnbc_11_children$Maternal_ED <- as.factor(dnbc_11_children$Maternal_ED) 


# Adding self-reported maternal psychiatric disorder

adults_i1 <- adults_i1 |> 
  mutate(Maternal_PD = 
           case_when(A075 == 2  ~ 
                       "No Psychiatric Disorder",
                     A075 == 1 | A075 == 3 | A075 == 4 | 
                       A075 == 9 ~ "Yes/Don't know/Potentially", 
                     A075 == 10 ~ "Irrelevant")) |> 
  mutate_at(vars(Maternal_PD),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(adults_i1 |> select(lbgravnr, Maternal_PD), by = "lbgravnr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Maternal_PD = if_else(
    Maternal_PD == "No Psychiatric Disorder" | 
    Maternal_ED == "Maternal Eating Disorder", 
    "No Psychiatric Disorder", Maternal_PD))

# Adding feeding disorders

# We remove i3,i4,i5_date 

feeding_disorders <- feeding_disorders |> 
  select(-i3_date,-i4_date,-i5_date)

# Make the data long

feeding_disorders <- feeding_disorders |> 
  pivot_longer(i1_date:i2_date,
               names_to = "Feeding_Events", 
               values_to = "Counts") |> 
  select(-Feeding_Events)

feeding_disorders <- feeding_disorders |> 
  select(bcpr = PNR, Counts)

feeding_disorders <- feeding_disorders |> 
  group_by(bcpr) |> 
  mutate(Event = if_else(!is.na(Counts),1,0))

feeding_disorders <- feeding_disorders |> 
  ungroup()

# Now we need to merge the questionaire date completion

feeding_df <- dnbc_11_children |> 
  left_join(feeding_disorders,by = "bcpr")

feeding_df$Event <- if_else(is.na(feeding_df$Event),0,feeding_df$Event)

feeding_df$Counts <- as_date(feeding_df$Counts)

feeding_df$E_QDATE <- as_date(feeding_df$E_QDATE)

# I will only keep the sum of feeding disorders up to 6years of age
# I will create a placebo really old date just to filter properly
# I do not really need this as we have restricted to max i2 date

feeding_df <- feeding_df |> 
  replace_na(list(Counts = as_date("1900-01-01"))) |> 
  filter(Counts <= E_QDATE) |> 
  ungroup()


# Let's calculate the number of feeding events
cumul_feeding <- feeding_df|> 
  group_by(bcpr) |> 
  summarise(Cumulative_Feeding = sum(Event)) |> 
  replace_na(list(Cumulative_Feeding = 0)) |> 
  ungroup()

# Now let's place in the dnbc dataset

dnbc_11_children <- dnbc_11_children |> 
  left_join(cumul_feeding,by = "bcpr")


# Adding other maternal characteristics

adults_i1 <- adults_i1 |> 
  mutate(Pregnancy_Plan = case_when(A018 == 4 | A018 == 5 | A018 == 9 ~ NA,
                                    A018 == 10 ~ "Irrelevant",
                                    A018 == 1 | 
                                      A018 == 2 ~ "Planned or Partly Planned",
                                    A018 == 3 ~ "Not Planned"
  )) |> 
  mutate(Alcohol_Pregnancy = case_when(A145A == 0 ~ "No Drinks",
                                       A145A == 1 ~ "Once",
                                       A145A == 2 ~ "Twice",
                                       A145A >= 3 ~ "Three or more times",
                                       is.na(A145A) ~ "NA")) |> 
  mutate(Smoking_Pregnancy = case_when(A127 == 1 ~ "Yes",
                                       A127 == 2 ~ "No",
                                       A127 == 3 | A127 == 4 | A127 == 9 ~ NA,
                                       A127 == 10 ~ "Irrelevant")) |> 
  mutate(Pre_Preg_Weight = A039A) |> 
  mutate(Pre_Preg_Height = A040A) |> 
  mutate(Pre_Preg_Height = Pre_Preg_Height/100) |> 
  mutate(BMI_Pre_Pregnancy = Pre_Preg_Weight / (Pre_Preg_Height^2)) |> 
  mutate_at(vars(Pregnancy_Plan, Alcohol_Pregnancy, Smoking_Pregnancy),as.factor)


i1_new <- adults_i1 |> 
  select(lbgravnr,Pregnancy_Plan,Alcohol_Pregnancy,Smoking_Pregnancy,
         BMI_Pre_Pregnancy)

# Trim the BMI_Pre_Pregnancy Variable

lower_pre <- mean(i1_new$BMI_Pre_Pregnancy,na.rm = T) - 4*sd(i1_new$BMI_Pre_Pregnancy,na.rm = T)

upper_pre <- mean(i1_new$BMI_Pre_Pregnancy,na.rm = T) + 4*sd(i1_new$BMI_Pre_Pregnancy,na.rm = T)

i1_new <- i1_new |> 
  mutate(BMI_Pre_Pregnancy = if_else((BMI_Pre_Pregnancy > upper_pre) | (BMI_Pre_Pregnancy < lower_pre), NA,BMI_Pre_Pregnancy))

# Adding them to the dataset

dnbc_11_children <- dnbc_11_children |> 
  left_join(i1_new,by = "lbgravnr")

# Adding the folic acid information

folic <- folic |> 
  select(lbgravnr,sup_fol1) |> 
  mutate(Folic_Acid = case_when(sup_fol1 == 0 | sup_fol1 == 2 ~ "No",
                                sup_fol1 == 1 | sup_fol1 == 10 ~ "Yes")) |> 
  select(lbgravnr,Folic_Acid)

folic$Folic_Acid <- as.factor(folic$Folic_Acid)

dnbc_11_children <- dnbc_11_children |> 
  left_join(folic,by = "lbgravnr")  


# Adding urbanicity

urbanicity <- urbanicity |> filter(age == 11) |> select(-age)

urbanicity <- urbanicity |> 
  select(bcpr = PNR, urb) |> 
  mutate_at(vars(urb),factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(urbanicity, by = "bcpr")


# Adding the autoimmune Diseases

# Psoriasis

dnbc_11_adults$Psoriasis <- dnbc_11_adults$F052

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(Psoriasis = if_else(Psoriasis == 99, NA, Psoriasis)) |> 
  mutate_at(vars(Psoriasis),as.factor)

levels(dnbc_11_adults$Psoriasis) <- c("Yes", "No")


# Diabetes


dnbc_11_adults$Diabetes <- dnbc_11_adults$F121

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(Diabetes = if_else(Diabetes == 99, NA, Diabetes)) |> 
  mutate_at(vars(Diabetes),as.factor)

levels(dnbc_11_adults$Diabetes) <- c("Yes", "No")


# Coeliac


dnbc_11_adults$Coeliac <- dnbc_11_adults$F123

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(Coeliac = if_else(Coeliac == 99, NA, Coeliac)) |> 
  mutate_at(vars(Coeliac),as.factor)

levels(dnbc_11_adults$Coeliac) <- c("Yes", "No")


# Asthma


dnbc_11_adults$Asthma <- dnbc_11_adults$F085

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(Asthma = if_else(Asthma == 99, NA, Asthma)) |> 
  mutate_at(vars(Asthma),as.factor)

levels(dnbc_11_adults$Asthma) <- c("Yes", "No")


# Now we put all the autoimmune diseases all back together

dnbc_11_children <- dnbc_11_children |> 
  left_join(dnbc_11_adults |> 
            select(lbgravff,Psoriasis,Diabetes,Coeliac,Asthma),
            by = "lbgravff")


# Adding paternal bmi at 11-y follow up

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(F183 = if_else(F183 == 0 | F183 == 99 | F183 == 100, NA, F183)) |> 
  mutate(F185 = if_else(F185 == 0 | F185 == 99 | F185 == 100, NA, F185))

paternal_bmi <- dnbc_11_adults |> select(lbgravff,F183,F185)

paternal_bmi$F183 <- paternal_bmi$F183/100

paternal_bmi$BMI_Father <- paternal_bmi$F185 / (paternal_bmi$F183^2)

paternal_bmi <- paternal_bmi |> 
  select(lbgravff,BMI_Father)

# Trim the Paternal BMI

lower_paternal <- mean(paternal_bmi$BMI_Father,na.rm = T) - 4*sd(paternal_bmi$BMI_Father,na.rm = T)

upper_paternal <- mean(paternal_bmi$BMI_Father,na.rm = T) + 4*sd(paternal_bmi$BMI_Father,na.rm = T)

# Create NAs for values that exceeds 4sds above or below the mean

paternal_bmi <- paternal_bmi |> 
  mutate(BMI_Father = if_else((BMI_Father > upper_paternal) | (BMI_Father < lower_paternal), NA,BMI_Father))

# Put it back to our dataset

dnbc_11_children <- dnbc_11_children |> 
  left_join(paternal_bmi,by = "lbgravff")


# Adding maternal BMI at 11-y

dnbc_11_adults <- dnbc_11_adults |> 
  mutate(F189 = if_else(F189 == 0 | F189 == 99 | F189 == 100, NA, F189)) |> 
  mutate(F191 = if_else(F191 == 0 | F191 == 99 | F191 == 100, NA, F191))

maternal_bmi <- dnbc_11_adults |> select(lbgravff,F189,F191)

maternal_bmi$F189 <- maternal_bmi$F189/100

maternal_bmi$BMI_Mother_11 <- maternal_bmi$F191 / (maternal_bmi$F189^2)

maternal_bmi <- maternal_bmi |> 
  select(lbgravff,BMI_Mother_11)

# Trim the Maternal BMI

lower_maternal<- mean(maternal_bmi$BMI_Mother_11,na.rm = T) - 4*sd(maternal_bmi$BMI_Mother_11,na.rm = T)

upper_maternal <- mean(maternal_bmi$BMI_Mother_11,na.rm = T) + 4*sd(maternal_bmi$BMI_Mother_11,na.rm = T)

# Create NAs for values that exceeds 4sds above or below the mean

maternal_bmi <- maternal_bmi |> 
  mutate(BMI_Mother_11 = if_else((BMI_Mother_11 > upper_maternal) | 
                                   (BMI_Mother_11 < lower_maternal), 
                                    NA,BMI_Mother_11))

# Put it back to our dataset

dnbc_11_children <- dnbc_11_children |> 
  left_join(maternal_bmi,by = "lbgravff")


# Adding maternal and paternal age of birth

dnbc_11_children <- dnbc_11_children |> 
  left_join(nogle23 |> transmute(lbgravff,
                                 Mother_Age = malder, 
                                 Father_Age = falder),
                                 by = c("lbgravff")) 

############################################

# Adding all the adversities

### Parental somatic illness

length(unique(parental_somatic_illness$PNR)) 

# Let's merge that information with our dataset

parental_somatic_illness <- parental_somatic_illness |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

parental_somatic_illness <- parental_somatic_illness |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Parental somatic illness, mother","Mother","Father")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

parental_somatic_illness <- 
  parental_somatic_illness |> 
  select(bcpr,
         Parental_Somatic = event, 
         Parental_Somatic_Date = Event_Date,
         Parental_Somatic_Detail = Event_Detail)


dnbc_11_children <- dnbc_11_children |> 
  left_join(parental_somatic_illness,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Somatic = if_else(is.na(Parental_Somatic),"No Parental Somatic Illness", Parental_Somatic))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Somatic_Detail = if_else(is.na(Parental_Somatic_Detail),"Not Applicable", Parental_Somatic_Detail))


dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Parental_Somatic, Parental_Somatic_Detail),as.factor)

# Perhaps let's keep the Parental_Somatic variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Somatic = if_else(
    Parental_Somatic == "Parental somatic illness", 1, 0))


# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Parental_Somatic_Date,.before = lbgravnr)

# Let's calculate the number of somatic illnesses

cumul_somatic <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Parental_Somatic_Illnesses = sum(Parental_Somatic[E_QDATE > Parental_Somatic_Date])) |> 
  ungroup() |> 
  replace_na(list(Cumulative_Parental_Somatic_Illnesses = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Parental_Somatic_Date,-Parental_Somatic,-Parental_Somatic_Detail) |> 
  left_join(cumul_somatic,by = "bcpr")

# Create a factor for Parental Somatic Illnesses

summary(dnbc_11_children$Cumulative_Parental_Somatic_Illnesses)

dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Parental_Somatic_Illnesses = case_when(Cumulative_Parental_Somatic_Illnesses == 0 ~ "Zero",
                                                           Cumulative_Parental_Somatic_Illnesses %in% 1:3 ~"One to Three",
                                                           Cumulative_Parental_Somatic_Illnesses >= 4 ~"Four or more")) |>
  mutate_at(vars(Cumulative_Parental_Somatic_Illnesses),as.factor)


### Sibling somatic illness

length(unique(sibling_somatic_illness$PNR)) 

# Let's merge that information with our dataset

sibling_somatic_illness <- sibling_somatic_illness |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

sibling_somatic_illness <- sibling_somatic_illness |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail, -event_date) |> 
  mutate_at(vars(event),as.factor)

sibling_somatic_illness <- 
  sibling_somatic_illness |> 
  select(bcpr,
         Sibling_Somatic = event, 
         Sibling_Somatic_Date = Event_Date)


dnbc_11_children <- dnbc_11_children |> 
  left_join(sibling_somatic_illness,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children|> 
  mutate(Sibling_Somatic = if_else(is.na(Sibling_Somatic),"No Sibling Somatic Illness", Sibling_Somatic))

dnbc_11_children <- dnbc_11_children|> 
  mutate_at(vars(Sibling_Somatic),as.factor)

# Perhaps let's keep the Sibling_Somatic variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Somatic = if_else(
    Sibling_Somatic == "Sibling somatic illness", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Sibling_Somatic_Date,.before = lbgravnr)

# Let's calculate the number of somatic illnesses

cumul_somatic_sibling <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Sibling_Somatic_Illnesses = sum(Sibling_Somatic[E_QDATE > Sibling_Somatic_Date])) |> 
  ungroup() |> 
  replace_na(list(Cumulative_Sibling_Somatic_Illnesses = 0))

# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Sibling_Somatic_Date,-Sibling_Somatic) |> 
  left_join(cumul_somatic_sibling,by = "bcpr")


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Sibling_Somatic_Illnesses = case_when(Cumulative_Sibling_Somatic_Illnesses == 0 ~ "Zero",
                                                          Cumulative_Sibling_Somatic_Illnesses %in% 1:3 ~ "One to Three",
                                                          Cumulative_Sibling_Somatic_Illnesses >= 4 ~ "Four or more")) |>
  mutate_at(vars(Cumulative_Sibling_Somatic_Illnesses),as.factor)


### Parental Psychiatric Illnesses

# How many observations we have

length(unique(parental_psychiatric_illness$PNR)) 

# Let's merge that information with our dataset

parental_psychiatric_illness <- parental_psychiatric_illness |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

parental_psychiatric_illness <- parental_psychiatric_illness |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Parental psychiatric illness, mother","Mother","Father")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

parental_psychiatric_illness <- 
  parental_psychiatric_illness |> 
  select(bcpr,
         Parental_Psychiatric = event, 
         Parental_Psychiatric_Date = Event_Date,
         Parental_Psychiatric_Detail = Event_Detail)


dnbc_11_children <- dnbc_11_children |> 
  left_join(parental_psychiatric_illness,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Psychiatric = if_else(is.na(Parental_Psychiatric),"No Parental Psychiatric Illness", Parental_Psychiatric))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Psychiatric_Detail = if_else(is.na(Parental_Psychiatric_Detail),"Not Applicable", Parental_Psychiatric_Detail))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Parental_Psychiatric, Parental_Psychiatric_Detail),as.factor)

# Perhaps let's keep the Parental_Psychiatric variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Psychiatric = if_else(
    Parental_Psychiatric == "Parental psychiatric illness", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Parental_Psychiatric_Date,.before = lbgravnr)

# Let's calculate the number of psychiatric illnesses

cumul_psychiatric <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Parental_Psychiatric_Illnesses = sum(Parental_Psychiatric[E_QDATE > Parental_Psychiatric_Date])) |> 
  replace_na(list(Cumulative_Parental_Psychiatric_Illnesses = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Parental_Psychiatric_Date,-Parental_Psychiatric,-Parental_Psychiatric_Detail) |> 
  left_join(cumul_psychiatric,by = "bcpr")

summary(dnbc_11_children$Cumulative_Parental_Psychiatric_Illnesses)


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Parental_Psychiatric_Illnesses = case_when(Cumulative_Parental_Psychiatric_Illnesses == 0 ~ "Zero",
                                                               Cumulative_Parental_Psychiatric_Illnesses %in% 1:3 ~ "One to Three",
                                                               Cumulative_Parental_Psychiatric_Illnesses >= 4 ~ "Four or more")) |>
  mutate_at(vars(Cumulative_Parental_Psychiatric_Illnesses),as.factor)


### Sibling Psychiatric Illnesses

################### Let's look at sibling psychiatric illness #################

length(unique(sibling_psychiatric_illness$PNR)) 

# Let's merge that information with our dataset

sibling_psychiatric_illness <- sibling_psychiatric_illness |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

sibling_psychiatric_illness <- sibling_psychiatric_illness |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail, -event_date) |> 
  mutate_at(vars(event),as.factor)

sibling_psychiatric_illness <- 
  sibling_psychiatric_illness |> 
  select(bcpr,
         Sibling_Psychiatric = event, 
         Sibling_Psychiatric_Date = Event_Date)


dnbc_11_children <- dnbc_11_children |> 
  left_join(sibling_psychiatric_illness,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Psychiatric = if_else(is.na(Sibling_Psychiatric),"No Sibling Psychiatric Illness", Sibling_Psychiatric))


dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Sibling_Psychiatric),as.factor)

# Perhaps let's keep the Sibling_Psychiatric variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Psychiatric = if_else(
    Sibling_Psychiatric == "Sibling psychiatric illness", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Sibling_Psychiatric_Date,.before = lbgravnr)

# Let's calculate the number of psychiatric illnesses

cumul_psychiatric_sibling <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Sibling_Psychiatric_Illnesses = sum(Sibling_Psychiatric[E_QDATE > Sibling_Psychiatric_Date])) |> 
  ungroup() |> 
  replace_na(list(Cumulative_Sibling_Psychiatric_Illnesses = 0))

# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Sibling_Psychiatric_Date,-Sibling_Psychiatric) |> 
  left_join(cumul_psychiatric_sibling,by = "bcpr")


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Sibling_Psychiatric_Illnesses = case_when(Cumulative_Sibling_Psychiatric_Illnesses == 0 ~ "Zero", 
                                                              Cumulative_Sibling_Psychiatric_Illnesses %in% 1:3 ~ "One to Three",
                                                              Cumulative_Sibling_Psychiatric_Illnesses >= 4 ~ "Four or more")) |>
  mutate_at(vars(Cumulative_Sibling_Psychiatric_Illnesses),as.factor)


### Foster Care Events

################### Let's look at foster care #################

length(unique(foster_care$PNR)) 

# Let's merge that information with our dataset

foster_care <- foster_care |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

foster_care <- foster_care |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail, -event_date) |> 
  mutate_at(vars(event),as.factor)

foster_care <- 
  foster_care |> 
  select(bcpr,
         Foster_Care_Event = event, 
         Foster_Care_Date = Event_Date)

dnbc_11_children <- dnbc_11_children |> 
  left_join(foster_care,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Foster_Care_Event = if_else(is.na(Foster_Care_Event),"No Foster Care", Foster_Care_Event))


dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Foster_Care_Event),as.factor)

# Perhaps let's keep the Foster_Care_Event variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Foster_Care_Event = if_else(
    Foster_Care_Event == "Foster care", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Foster_Care_Date,.before = lbgravnr)

# Let's calculate the number of foster care events

cumul_foster_care <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Foster_Care_Events = sum(Foster_Care_Event[E_QDATE > Foster_Care_Date])) |> 
  ungroup() |> 
  replace_na(list(Cumulative_Foster_Care_Events = 0))

# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Foster_Care_Date,-Foster_Care_Event) |> 
  left_join(cumul_foster_care,by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Foster_Care_Events = if_else(Cumulative_Foster_Care_Events == 0 ,"No Foster Care", "Foster Care Event")) |> 
  mutate_at(vars(Cumulative_Foster_Care_Events),as.factor)


### Parental Death

# How many observations we have

length(unique(death_of_parent$PNR)) 

# Let's merge that information with our dataset

death_of_parent <- death_of_parent |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

death_of_parent <- death_of_parent |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Death of a father","Father","Mother")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

death_of_parent <- 
  death_of_parent |> 
  select(bcpr,
         Parental_Death = event, 
         Parental_Death_Date = Event_Date,
         Parental_Death_Detail = Event_Detail)

dnbc_11_children <- dnbc_11_children |> 
  left_join(death_of_parent,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Death = if_else(is.na(Parental_Death),"No Parental Death", Parental_Death))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Death_Detail = if_else(is.na(Parental_Death_Detail),"Not Applicable", Parental_Death_Detail))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Parental_Death, Parental_Death_Detail),as.factor)

# Perhaps let's keep the Parental Death as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Death = if_else(
    Parental_Death == "Death of a parent", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Parental_Death_Date,.before = lbgravnr)

# Let's calculate the number of parental deaths

cumul_parental_deaths <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Parental_Deaths = sum(Parental_Death[E_QDATE > Parental_Death_Date])) |> 
  replace_na(list(Cumulative_Parental_Deaths = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Parental_Death_Date,-Parental_Death) |> 
  left_join(cumul_parental_deaths,by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Death = if_else(Cumulative_Parental_Deaths == 0 , "No Parental Death", "Parental Death Event")) |> 
  mutate_at(vars(Parental_Death),as.factor) |> 
  select(-Cumulative_Parental_Deaths)

### Sibling Death

# How many observations we have

length(unique(death_of_sibling$PNR)) 

# Let's merge that information with our dataset

death_of_sibling <- death_of_sibling |> 
  mutate(bcpr = PNR) |> 
  select(-PNR)

death_of_sibling <- death_of_sibling |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail) |> 
  mutate_at(vars(event),as.factor)

death_of_sibling <- 
  death_of_sibling |> 
  select(bcpr,
         Sibling_Death = event, 
         Sibling_Death_Date = Event_Date)

dnbc_11_children <- dnbc_11_children |> 
  left_join(death_of_sibling,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Death = if_else(is.na(Sibling_Death),"No Sibling Death", Sibling_Death))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Sibling_Death),as.factor)

# Perhaps let's keep the Sibling Death as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Death = if_else(
    Sibling_Death == "Death of a sibling", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Sibling_Death_Date,.before = lbgravnr)

# Let's calculate the number of sibling deaths

cumul_sibling_deaths <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Sibling_Deaths = sum(Sibling_Death[E_QDATE > Sibling_Death_Date])) |> 
  replace_na(list(Cumulative_Sibling_Deaths = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Sibling_Death_Date,-Sibling_Death) |> 
  left_join(cumul_sibling_deaths,by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Sibling_Death = if_else(Cumulative_Sibling_Deaths == 0 , "No Sibling Death", "Sibling Death Event")) |> 
  mutate_at(vars(Sibling_Death),as.factor) |> 
  select(-Cumulative_Sibling_Deaths)

### Parental Separation

# Let's merge that information with our dataset

parental_separation <- parental_separation |> 
  mutate(bcpr = PNR) |> 
  select(-PNR) 

parental_separation <- parental_separation |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail) |> 
  mutate_at(vars(event),as.factor)

parental_separation <- 
  parental_separation |> 
  select(bcpr,
         Parental_Separation = event, 
         Parental_Separation_Date = Event_Date)

dnbc_11_children <- dnbc_11_children |> 
  left_join(parental_separation,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Separation = if_else(is.na(Parental_Separation),"No Parental Separation", Parental_Separation))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Parental_Separation),as.factor)

# Perhaps let's keep the Parental Separation as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Separation = if_else(
    Parental_Separation == "separation", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Parental_Separation_Date,.before = lbgravnr)

# Let's calculate the number of parental separations

cumul_parental_seperations <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Parental_Separations = sum(Parental_Separation[E_QDATE > Parental_Separation_Date])) |> 
  replace_na(list(Cumulative_Parental_Separations = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Parental_Separation_Date,-Parental_Separation) |> 
  left_join(cumul_parental_seperations,by = "bcpr")


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Parental_Separations = case_when(Cumulative_Parental_Separations == 0 ~ "Zero",
                                                     Cumulative_Parental_Separations %in% 1:3 ~ "One to Three",
                                                     Cumulative_Parental_Separations >= 4 ~ "Four or more")) |>
  mutate_at(vars(Cumulative_Parental_Separations),as.factor)

### Parental Long-Term Unemployment

# Let's merge that information with our dataset

parental_long_term_unemployment <- parental_long_term_unemployment |> 
  mutate(bcpr = PNR) |> 
  select(-PNR) 

parental_long_term_unemployment <- parental_long_term_unemployment |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Parental long-term unemployment, mother","Mother","Father")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

parental_long_term_unemployment <- 
  parental_long_term_unemployment |> 
  select(bcpr,
         Parental_Unemployment = event, 
         Parental_Unemployment_Date = Event_Date,
         Parental_Unemployment_Detail = Event_Detail)


dnbc_11_children <- dnbc_11_children |> 
  left_join(parental_long_term_unemployment,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Unemployment = if_else(is.na(Parental_Unemployment),"No Parental Unemployment", Parental_Unemployment))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Unemployment_Detail = if_else(is.na(Parental_Unemployment_Detail),"Not Applicable", Parental_Unemployment_Detail))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Parental_Unemployment, Parental_Unemployment_Detail),as.factor)

# Perhaps let's keep the Parental_Unemployment variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Parental_Unemployment = if_else(
    Parental_Unemployment == "Parental long-term unemployment", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Parental_Unemployment_Date,.before = lbgravnr)

# Let's calculate the number of unemployment events

cumul_unemployment <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Parental_Unemployment = sum(Parental_Unemployment[E_QDATE > Parental_Unemployment_Date])) |> 
  replace_na(list(Cumulative_Parental_Unemployment = 0))

# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Parental_Unemployment_Date,-Parental_Unemployment,-Parental_Unemployment_Detail) |> 
  left_join(cumul_unemployment,by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Parental_Unemployment = case_when(Cumulative_Parental_Unemployment == 0 ~ "Zero", 
                                                      Cumulative_Parental_Unemployment == 1 ~ "One Parental Unemployment Event",
                                                      Cumulative_Parental_Unemployment >= 2 ~ "More than two")) |>
  mutate_at(vars(Cumulative_Parental_Unemployment),as.factor)

### Poverty

# Let's merge that information with our dataset

poverty <- poverty |> 
  mutate(bcpr = PNR) |> 
  select(-PNR) 

poverty <- poverty |> 
  mutate(Event_Date = event_date) |> 
  select(-event_detail) |> 
  mutate_at(vars(event),as.factor)

poverty <- 
  poverty |> 
  select(bcpr,
         Poverty = event, 
         Poverty_Date = Event_Date)

dnbc_11_children <- dnbc_11_children |> 
  left_join(poverty,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Poverty = if_else(is.na(Poverty),"No Poverty", Poverty))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Poverty),as.factor)

# Perhaps let's keep the Poverty as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Poverty = if_else(
    Poverty == "Poverty", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Poverty_Date,.before = lbgravnr)

# Let's calculate the number of poverty periods
cumul_poverty <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Poverty = sum(Poverty[E_QDATE > Poverty_Date])) |> 
  replace_na(list(Cumulative_Poverty = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Poverty_Date,-Poverty) |> 
  left_join(cumul_poverty,by = "bcpr")


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Poverty = case_when(Cumulative_Poverty == 0 ~ "Zero", 
                                        Cumulative_Poverty == 1 ~ "One poverty period",
                                        Cumulative_Poverty >= 2 ~ "Two or more poverty Periods")) |>
  mutate_at(vars(Cumulative_Poverty),as.factor)


### Alcohol Abuse

# Let's merge that information with our dataset

alcohol <- alcohol_abuse |> 
  mutate(bcpr = PNR) |> 
  select(-PNR) 

alcohol <- alcohol |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Parental alcohol abuse, mother","Mother","Father")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

alcohol <- 
  alcohol |> 
  select(bcpr,
         Alcohol_Abuse = event, 
         Alcohol_Abuse_Date = Event_Date,
         Alcohol_Abuse_Detail = Event_Detail)


dnbc_11_children <- dnbc_11_children |> 
  left_join(alcohol,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Alcohol_Abuse = if_else(is.na(Alcohol_Abuse),"No Alcohol Abuse", Alcohol_Abuse))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Alcohol_Abuse_Detail = if_else(is.na(Alcohol_Abuse_Detail),"Not Applicable", Alcohol_Abuse_Detail))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Alcohol_Abuse, Alcohol_Abuse_Detail),as.factor)

# Perhaps let's keep the Alcohol Abuse variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Alcohol_Abuse = if_else(
    Alcohol_Abuse == "Parental alcohol abuse", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Alcohol_Abuse_Date,.before = lbgravnr)

# Let's calculate the number of alcohol abuse events

cumul_alcohol <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Alcohol_Abuse = sum(Alcohol_Abuse[E_QDATE > Alcohol_Abuse_Date])) |> 
  replace_na(list(Cumulative_Alcohol_Abuse = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Alcohol_Abuse_Date,-Alcohol_Abuse,-Alcohol_Abuse_Detail) |> 
  left_join(cumul_alcohol,by = "bcpr") 


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Alcohol_Abuse = case_when(
    Cumulative_Alcohol_Abuse == 0 ~ "Zero",
    Cumulative_Alcohol_Abuse == 1 ~ "One alcohol abuse event",
    Cumulative_Alcohol_Abuse >= 2 ~ "Two or more alcohol abuse events")) |>
  mutate_at(vars(Cumulative_Alcohol_Abuse),as.factor)


### Drug Abuse


# Let's merge that information with our dataset

drugs <- drug_abuse |> 
  mutate(bcpr = PNR) |> 
  select(-PNR) 

drugs <- drugs |> 
  mutate(Event_Date = event_date,
         Event_Detail = 
           if_else(event_detail == "Parental drug abuse, mother","Mother","Father")) |> 
  select(-event_detail) |> 
  mutate_at(vars(Event_Detail,event),as.factor)

drugs <- 
  drugs |> 
  select(bcpr,
         Drug_Abuse = event, 
         Drug_Abuse_Date = Event_Date,
         Drug_Abuse_Detail = Event_Detail)


dnbc_11_children <- dnbc_11_children |> 
  left_join(drugs,multiple = "all",by = "bcpr")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Drug_Abuse = if_else(is.na(Drug_Abuse),"No Drug Abuse", Drug_Abuse))

dnbc_11_children <- dnbc_11_children |> 
  mutate(Drug_Abuse_Detail = if_else(is.na(Drug_Abuse_Detail),"Not Applicable", Drug_Abuse_Detail))

dnbc_11_children <- dnbc_11_children |> 
  mutate_at(vars(Drug_Abuse, Drug_Abuse_Detail),as.factor)

# Perhaps let's keep the Alcohol Abuse variable as binary

dnbc_11_children <- dnbc_11_children |> 
  mutate(Drug_Abuse = if_else(
    Drug_Abuse == "Parental drug abuse", 1, 0))

# Now let's relocate

dnbc_11_children <- dnbc_11_children |> 
  relocate(Drug_Abuse_Date,.before = lbgravnr)

# Let's calculate the number of alcohol abuse events

cumul_drugs <- dnbc_11_children |> 
  group_by(bcpr) |> 
  summarise(Cumulative_Drug_Abuse = sum(Drug_Abuse[E_QDATE > Drug_Abuse_Date])) |> 
  replace_na(list(Cumulative_Drug_Abuse = 0))


# Now let's filter the duplicates and place the new variables

dnbc_11_children <- dnbc_11_children |> 
  filter(!duplicated(bcpr)) |> 
  select(-Drug_Abuse_Date,-Drug_Abuse,-Drug_Abuse_Detail) |> 
  left_join(cumul_drugs,by = "bcpr")


dnbc_11_children <- dnbc_11_children |> 
  mutate(Cumulative_Drug_Abuse = case_when(
    Cumulative_Drug_Abuse == 0 ~ "Zero",
    Cumulative_Drug_Abuse == 1 ~ "One drug abuse event",
    Cumulative_Drug_Abuse >= 2 ~ "Two or more drug abuse events")) |>
  mutate_at(vars(Cumulative_Drug_Abuse),as.factor)


# Adding maternal drinking

mat_drink <- mat_drink |> 
  select(lbgravnr,alcgrp) |> 
  mutate(Maternal_Alcohol = case_when(alcgrp == 1 ~ "Abstinent",
                                      alcgrp == 2 ~ "0.5 to 3 units per week",
                                      alcgrp == 3 ~ "4 or more units per week")) |>
  mutate_at(vars(Maternal_Alcohol),as.factor) |> 
  select(lbgravnr,Maternal_Alcohol)


# Adding maternal smoking

mat_smoke <- mat_smoke |> 
  select(lbgravnr,smok_cat2) |> 
  mutate(Maternal_Smoking = case_when(smok_cat2 == 1 ~ "No Smoking",
                                      smok_cat2 == 2 ~ "Quit Smoking Before Pregnancy",
                                      smok_cat2 == 3 ~ "Less than 10 Grams of Tobacco",
                                      smok_cat2 == 4 ~ "10 or more Grams of Tobacco")) |> 
  mutate_at(vars(Maternal_Smoking),as.factor)

mat_smoke <- mat_smoke |> 
  select(lbgravnr,Maternal_Smoking)

# Putting both of them in the original dataset

dnbc_11_children <- dnbc_11_children |> 
  select(-Smoking_Pregnancy, -Alcohol_Pregnancy) |> 
  left_join(mat_smoke,by = "lbgravnr") |> 
  left_join(mat_drink,by = "lbgravnr")


# Adding parental psychiatric disorders (registers)

diagn_parental_psychiatric <- diagn_parental_psychiatric |> 
  mutate(psych_noed_11_par = if_else(psych_noed_11_par == "", 
                                     "None", psych_noed_11_par)) |> 
  mutate_at(vars(psych_noed_11_par),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(diagn_parental_psychiatric |> 
              select(lbgravff,psych_noed_11_par),by = "lbgravff")

dnbc_11_children <- dnbc_11_children |> 
  mutate(psych_noed_11_par = if_else(is.na(psych_noed_11_par), 
                                     "None",psych_noed_11_par)) |> 
  mutate_at(vars(psych_noed_11_par),as.factor)


# Adding parental eating disorders (registers)

diagn_parental_ed <- diagn_parental_ed |> 
  mutate(ed11_par = if_else(ed11_par == "", 
                            "None", ed11_par)) |> 
  mutate_at(vars(ed11_par),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(diagn_parental_ed |> 
              select(lbgravff,ed11_par),by = "lbgravff")

dnbc_11_children <- dnbc_11_children |> 
  mutate(ed11_par = if_else(is.na(ed11_par), 
                            "None",ed11_par)) |> 
  mutate_at(vars(ed11_par),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  mutate(ed11_par = if_else(is.na(ed11_par), "None", ed11_par)) |> 
  mutate_at(vars(ed11_par),as.factor)


# Adding child's psychiatric disorders (registers)

diagn_children_psychiatric <- diagn_children_psychiatric |> 
  mutate(Child_Psych11 = if_else(is.na(psych_noed_11),"No", "Yes")) |> 
  mutate_at(vars(Child_Psych11),as.factor)

dnbc_11_children <- dnbc_11_children |> 
  left_join(diagn_children_psychiatric |> 
              select(lbgravff,Child_Psych11),by = "lbgravff")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Child_Psych11 = if_else(is.na(Child_Psych11), "No", Child_Psych11)) |> 
  mutate_at(vars(Child_Psych11),as.factor)


# Adding the autoimmune diseases

diagn_autoimmune <- diagn_autoimmune |> 
  left_join(dnbc_11_children |> select(lbgravff,E_QDATE), by = "lbgravff")

diagn_autoimmune <- diagn_autoimmune |> 
  filter(!is.na(E_QDATE)) |> 
  mutate(Autoimmune = if_else(d_inddto_autoimmune_disease <= E_QDATE,"Autoimmune","No Autoimmune")) |> 
  mutate_at(vars(Autoimmune),as.factor)

diagn_autoimmune <- diagn_autoimmune |> 
  filter(Autoimmune == "Autoimmune")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Autoimmune = if_else(lbgravff %in% diagn_autoimmune$lbgravff,  
                              "Autoimmune", "No Autoimmune"))

dnbc_11_children$Autoimmune <- as.factor(dnbc_11_children$Autoimmune)


# Adding the autoinflammatory conditions

diagn_autoinfl <- diagn_autoinfl |> 
  left_join(dnbc_11_children |> select(lbgravff,E_QDATE), by = "lbgravff")

diagn_autoinfl <- diagn_autoinfl |> 
  filter(!is.na(E_QDATE)) |> 
  mutate(Autoinflammatory = if_else(d_inddto_autoinflam_condition <= E_QDATE,"Autoinflammatory","No Autoinflammatory")) |> 
  mutate_at(vars(Autoinflammatory),as.factor)

diagn_autoinfl <- diagn_autoinfl |> 
  filter(Autoinflammatory == "Autoinflammatory")

dnbc_11_children <- dnbc_11_children |> 
  mutate(Autoinflammatory = if_else(lbgravff %in% diagn_autoinfl$lbgravff,  
                                    "Autoinflammatory", "No Autoinflammatory"))

dnbc_11_children$Autoinflammatory <- as.factor(dnbc_11_children$Autoinflammatory)


# Adding SDQ scale

sdq11_child <- sdq11_child |> 
  rename(cprosoc11_child = cprosoc) |> 
  rename(cpeer11_child = cpeer) |> 
  rename(chyper11_child = chyper) |> 
  rename(cemotion11_child = cemotion) |> 
  rename(cconduct11_child = cconduct) |> 
  rename(ctotal11_child = ctotal) |> 
  rename(cimpact11_child = cimpact) |> 
  rename(cinter11_child = cinter)

sdq11_child <- sdq11_child |> 
  select(lbgravff,cprosoc11_child,cpeer11_child,chyper11_child,cemotion11_child,
         cimpact11_child,cconduct11_child)

sdq11_parent <- sdq11_parent |> 
  rename(pprosoc11_parent = pprosoc) |> 
  rename(ppeer11_parent = ppeer) |> 
  rename(phyper11_parent = phyper) |> 
  rename(pemotion11_parent = pemotion) |> 
  rename(pconduct11_parent = pconduct) |> 
  rename(ptotal11_parent = ptotal) |> 
  rename(pimpact11_parent = pimpact) |> 
  rename(pinter11_parent = pinter)

sdq11_parent <- sdq11_parent |> 
  select(lbgravff,pprosoc11_parent,ppeer11_parent,phyper11_parent,
         pemotion11_parent,pimpact11_parent,
         pconduct11_parent)

sdq7_parent <- sdq7_parent |> 
  rename(pprosoc7_parent = pprosoc) |> 
  rename(ppeer7_parent = ppeer) |> 
  rename(phyper7_parent = phyper) |> 
  rename(pemotion7_parent = pemotion) |> 
  rename(pconduct7_parent = pconduct) |> 
  rename(ptotal7_parent = ptotal) |> 
  rename(pimpact7_parent = pimpact) |> 
  rename(pinter7_parent = pinter)

sdq7_parent <- sdq7_parent |> 
  select(lbgravff,pprosoc7_parent,ppeer7_parent,phyper7_parent,
         pemotion7_parent,pimpact7_parent,
         pconduct7_parent)

# Implement these variables finally to the dataframe

dnbc_11_children <- dnbc_11_children |> 
  left_join(sdq11_child,by = "lbgravff") |> 
  left_join(sdq11_parent, by = "lbgravff") |> 
  left_join(sdq7_parent, by = "lbgravff")


# Remove redundant variables

# Make further modifications

dnbc_11_children <- dnbc_11_children |> 
  select(-Showing_Sadness,-Stomach_Pains,-Adult_To_Talk_To,-Loneliness_Feeling)


# Create a more clean dataset, removing unneccesary columns

final11 <- dnbc_11_children |> 
  select(!starts_with("E0"))

final11 <- final11 |> 
  select(!starts_with("E1"))

final11 <- final11 |> 
  left_join(dnbc_11_children |> 
            select(lbgravff,E023_1:E023_15),by = "lbgravff")


####################################################
############ Adding the outcomes now ###############
####################################################

# Extracting the outcome from self-reported data

# Eating disorders at 11-y follow up

ED_11 |> 
  select(an,bn,bed,suban,subbn,subbed,deb,ed,subed) |> 
  pivot_longer(cols = c(an,bn,bed,suban,subbn,subbed,deb,ed,subed),
               names_to = "EDs",values_to = "ED_Counts") |> 
  mutate_at(vars(EDs),as.factor) |> 
  ggplot(aes(x = EDs, y = ED_Counts, fill = EDs)) + 
  geom_col() + 
  theme(legend.position = "topleft") + 
  labs(x = "Type of Eating Disorder", y = "Eating Disorder Frequency") + 
  scale_y_continuous(n.breaks = 10) +
  coord_flip()


# Missing values of ED disorders at 11

ED_11 |> 
  select(an,bn,bed,suban,subbn,subbed,deb,ed,subed) |> 
  as.data.frame() |> 
  plot_bar(ggtheme = theme_minimal())

##############################################

# Eating disorders at 18-y follow up

ED_18 |> 
  select(an,bn,bed,suban,subbn,subbed,deb,ed,subed) |> 
  pivot_longer(cols = c(an,bn,bed,suban,subbn,subbed,deb,ed,subed),
               names_to = "EDs",values_to = "ED_Counts") |> 
  mutate_at(vars(EDs),as.factor) |> 
  ggplot(aes(x = EDs, y = ED_Counts, fill = EDs)) + 
  geom_col() + 
  theme(legend.position = "topleft") + 
  labs(x = "Type of Eating Disorder", y = "Eating Disorder Frequency") + 
  scale_y_continuous(n.breaks = 10) +
  coord_flip()

# Missing values of ED disorders at 18

ED_18 |> 
  select(an,bn,bed,suban,subbn,subbed,deb,ed,subed) |> 
  as.data.frame() |> 
  plot_bar(ggtheme = theme_minimal())

#################################################

# We extract the ED / SubEd / Deb at 11-y follow up and put it into our sample

final11 <- final11 |> 
  left_join(ED_11 |> select(lbgravff,ed,subed,deb),by = "lbgravff") |> 
  rename(ED_11 = ed) |> 
  rename(Subed_11 = subed) |> 
  rename(Deb_11 = deb)

# We add the variables specifying the specific eating disorder

# This only covers the 11-y follow-up ED variables

final11 <- final11 |> 
  left_join(ED_11 |> 
              select(lbgravff, an, suban, bn, subbn, bed, subbed), 
            by = "lbgravff") |> 
  rename(AN_11 = an, SubAN_11 = suban, BN_11 = bn, SubBN_11 = subbn,
         BED_11 = bed, SubBed_11 = subbed)


# Calculate the percentages now of each eating disorder category

final11 |> 
  select(AN_11,BN_11,BED_11,SubAN_11,SubBN_11,SubBed_11,Deb_11,ED_11,Subed_11) |> 
  pivot_longer(cols = c(AN_11,BN_11,BED_11,SubAN_11,SubBN_11,
                        SubBed_11,Deb_11,ED_11,Subed_11),
               names_to = "EDs",values_to = "ED_Counts") |> 
  mutate_at(vars(EDs),as.factor) |> 
  ggplot(aes(x = EDs, y = ED_Counts, fill = EDs)) + 
  geom_col() + 
  theme(legend.position = "topleft") + 
  labs(x = "Type of Eating Disorder", y = "Eating Disorder Frequency") + 
  coord_flip()


# Convert these diseases as factors

final11 <- final11 |> 
  mutate(ED_11 = if_else(ED_11 == 0, "No_Eating_Disorder", "Eating_Disorder"),
         Subed_11 = if_else(Subed_11 == 0, "No_Sub_ED", "Sub_ED"),
         AN_11 = if_else(AN_11 == 0, "No_Anorexia", "Anorexia"),
         SubAN_11 = if_else(SubAN_11 == 0, "No_Sub_Anorexia", "Sub_Anorexia"),
         BN_11 = if_else(BN_11 == 0, "No_BN", "BN"),
         SubBN_11 = if_else(SubBN_11 == 0, "No_Sub_BN", "Sub_BN"),
         BED_11 = if_else(BED_11 == 0, "No_BED","BED"),
         SubBed_11 = if_else(SubBed_11 == 0, "No_Sub_BED", "Sub_BED"),
         Deb_11 = if_else(Deb_11 == 0, "No_Deb", "Deb")) |> 
  mutate_at(vars(ED_11,Subed_11,AN_11,SubAN_11,BN_11,SubBN_11,BED_11,SubBed_11,Deb_11),
            as.factor)


########### Extraction of the outcome using the registers ################

ed_lpr <- ed_lpr |> 
  rename(bcpr = pnr)

# We need to specify also the date of the 18-year followup completion to our data

final11 <- final11 |> 
  left_join(dnbc_18 |> 
              select(lbgravff,G_COMPLETED_DATE),by = "lbgravff")

# Now let's put the E_QDATE and G_Completed_Date to the ed_lpr dataframe

ed_lpr <- ed_lpr |> 
  left_join(final11 |> select(bcpr,E_QDATE,G_COMPLETED_DATE),by = "bcpr") |> 
  relocate(E_QDATE,.after = bcpr)

ed_lpr <- ed_lpr |> 
  relocate(G_COMPLETED_DATE, .after = E_QDATE)


#Extract EDs diagnosed after the 11-y follow-up 

ed_lpr <- ed_lpr |> 
  filter(!is.na(E_QDATE))


# Let's proceed with the extraction of values


ed_lpr <- ed_lpr |> 
  group_by(bcpr) |> 
  pivot_longer(cols = c(int1,int2,int3,int4),
               names_to = "Periods",values_to = "Dates") |> 
  ungroup()

# We need to keep dates who are after 11-y questionaire completion
# We also need those dates to be before or at the 18-y completion date

ed_lpr1 <- ed_lpr |> 
  group_by(bcpr) |> 
  filter(Dates > E_QDATE & Dates <= G_COMPLETED_DATE) |> 
  ungroup() 


ed_lpr1 <- ed_lpr1 |> 
  filter(!duplicated(bcpr))

# This will give us the EDs at the period of interest (11 to 18)

ed_lpr1 <- ed_lpr1 |> 
  select(bcpr,Periods) |> 
  mutate(ED_Diagn_18 = if_else(Periods == "int3",1,0)) |> 
  select(-Periods)


# Now we need to merge with our original data

final11 <- final11 |> 
  left_join(ed_lpr1,by = "bcpr") |> 
  relocate(ED_Diagn_18, .before = ED_11) |> 
  mutate(ED_Diagn_18 = if_else(is.na(ED_Diagn_18),0,ED_Diagn_18)) |> 
  mutate(ED_Diagn_18 = if_else(ED_Diagn_18 == 1, "ED", "No_ED")) |> 
  mutate_at(vars(ED_Diagn_18),as.factor)


# ED before 6

ed_6 <- ed_lpr |> 
  filter(Periods == "int1") 

ed_6 <- ed_6 |> 
  filter(!is.na(Dates))

final11 <- final11 |> 
  mutate(ed_bef6 = if_else(bcpr %in% ed_6$bcpr, "ED before 6", "No ED before 6"))

final11$ed_bef6 <- as.factor(final11$ed_bef6)


# ED between 6 and 11

ed_6_11 <- ed_lpr |> 
  filter(Periods == "int2") |> 
  filter(!is.na(Dates))

final11 <- final11 |> 
  mutate(ed_6_11 = if_else(bcpr %in% ed_6_11$bcpr, "ED_6_11", "No_ED_6_11")) |> 
  mutate_at(vars(ed_6_11),as.factor)


# Remove redundant variables in our dataframe

final11 <- final11 |> 
  select(-bcpr,-E_QDATE,-E_VERSION,-E_INVITDATE,-E_BESVAR)

final11 <- final11 |> 
  select(-lbgravnr)

# final11 is our main diagnostic dataframe


########################################################
######## Creation of the prognostic dataframe ##########
########################################################

nogle23_progn <- nogle23 |> filter(y18_besvar == 1)

final18 <- final11 |> 
  filter(lbgravff %in% nogle23_progn$lbgravff)

final18 <- final18 |> 
  left_join(ED_18 |> select(lbgravff,ed,subed,deb),by = "lbgravff") |> 
  rename(ED_18 = ed) |> 
  rename(Subed_18 = subed) |> 
  rename(Deb_18 = deb)

# I will also add the variables specifying the specific eating disorder

# This only covers the 18-y follow-up ED variables

final18  <- final18 |> 
  left_join(ED_18 |> 
              select(lbgravff, an, suban, bn, subbn, bed, subbed,pd), 
            by = "lbgravff") |> 
  rename(AN_18 = an, SubAN_18 = suban, BN_18 = bn, SubBN_18 = subbn,
         BED_18 = bed, SubBed_18 = subbed, Pd_18 = pd)


final18 <- final18 |> 
  mutate(ED_18 = if_else(ED_18 == 0, "No_Eating_Disorder", "Eating_Disorder"),
         Subed_18 = if_else(Subed_18 == 0, "No_Sub_ED", "Sub_ED"),
         AN_18 = if_else(AN_18 == 0, "No_Anorexia", "Anorexia"),
         SubAN_18 = if_else(SubAN_18 == 0, "No_Sub_Anorexia", "Sub_Anorexia"),
         BN_18 = if_else(BN_18 == 0, "No_BN", "BN"),
         SubBN_18 = if_else(SubBN_18 == 0, "No_Sub_BN", "Sub_BN"),
         BED_18 = if_else(BED_18 == 0, "No_BED","BED"),
         SubBed_18 = if_else(SubBed_18 == 0, "No_Sub_BED", "Sub_BED"),
         Deb_18 = if_else(Deb_18 == 0, "No_Deb", "Deb")) |> 
  mutate_at(vars(ED_18,Subed_18,AN_18,SubAN_18,BN_18,SubBN_18,BED_18,SubBed_18,Deb_18),
            as.factor)


# Some tables

# Going from DEB at 11 to ED at 18

prop.table(table(final18$Deb_11,final18$ED_18),margin = 1)

# Going from DEB at 11 to DEB at 18

prop.table(table(final18$Deb_11,final18$Deb_18),margin = 1)

# Going from ED at 11 to ED at 18

prop.table(table(final18$ED_11,final18$ED_18),margin = 1)


# Save the dataframes final11 and final18
# They are going to be used for the analysis

write_parquet(final11,"initial_diagnostic.parquet")

write_parquet(final18,"initial_prognostic.parquet")
