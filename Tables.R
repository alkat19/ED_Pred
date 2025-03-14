# Creation of Table 1 for both prognostic and diagnostic tests
library(tidyverse)
library(gtsummary)
library(gt)

# Load the data

load("prognostic_main_env.RDATA")

load("diagnostic_main_env.RData")


# Set up the working environment

# For prognostic

training_table <- train_progn_main %>% mutate(Set = "Training Data")
testing_table <- test_progn_main %>% mutate(Set = "Testing Data") 
progn_table <- rbind(training_table,testing_table)

progn_table$Set <- factor(progn_table$Set,levels = c("Training Data", "Testing Data"))


progn_table <- progn_table %>% 
  select(-lbgravff,-G_COMPLETED_DATE, -c(AN_11,BN_11,BED_11,SubAN_11,Subed_11,
                                         SubBed_11,SubBN_11,SubBed_11,Deb_11,phyper7_parent,pprosoc7_parent,
                                         pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent,ED_11))


# Convert all NAs as separate levels of the factors

progn_table <- progn_table %>% 
  mutate(across(where(~is.factor(.) || is.character(.)),
                ~ {
                  updated <- factor(replace_na(as.character(.), "NA"))
                  factor(updated, levels = c(setdiff(levels(updated),"NA"),"NA"))
                }))

progn_table <- progn_table %>% 
  mutate(Cumulative_Feeding = if_else(Cumulative_Feeding == 0, "No Feeding Disorders", "One or more Feeding Disorders"),
         Maternal_PD = if_else(Maternal_PD == "Irrelevant" | Maternal_PD == "No Psychiatric Disorder", "Irrelevant or No Psychiatric Disorder", Maternal_PD),
         Pregnancy_Plan = if_else(Pregnancy_Plan == "Irrelevant" | Pregnancy_Plan == "Not Planned", "Not Planned or Irrelevant", Pregnancy_Plan),
         Folic_Acid = if_else(Folic_Acid == "NA" | Folic_Acid == "No", "No or NA", Folic_Acid))


progn_t1 <- progn_table %>% 
  tbl_summary(by = Set,
              missing_text = "NA", 
              missing_stat = "{p_miss}%",
              type = list(where(is.numeric) ~ "continuous2"),
              digits = list(all_categorical() ~ c(0,1))) %>% 
  add_overall()

progn_t1 %>% as_gt() %>% gtsave("progn_t1.docx")



# For diagnostic

training_tablez <- train_diagn_main %>% mutate(Set = "Training Data")
testing_tablez <- test_diagn_main %>% mutate(Set = "Testing Data") 
diagn_table <- rbind(training_tablez,testing_tablez)

diagn_table$Set <- factor(diagn_table$Set,levels = c("Training Data", "Testing Data"))


diagn_table <- diagn_table %>% 
  select(-lbgravff,-G_COMPLETED_DATE, -c(phyper7_parent,pprosoc7_parent,
                                         pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent))

diagn_table <- diagn_table %>% 
  mutate(across(where(~is.factor(.) || is.character(.)),
                ~ {
                  updated <- factor(replace_na(as.character(.), "NA"))
                  factor(updated, levels = c(setdiff(levels(updated),"NA"),"NA"))
                }))


diagn_table <- diagn_table %>% 
  mutate(Cumulative_Feeding = if_else(Cumulative_Feeding == 0, "No Feeding Disorders", "One or more Feeding Disorders"),
         Maternal_PD = if_else(Maternal_PD == "Irrelevant" | Maternal_PD == "No Psychiatric Disorder", "Irrelevant or No Psychiatric Disorder", Maternal_PD),
         Pregnancy_Plan = if_else(Pregnancy_Plan == "Irrelevant" | Pregnancy_Plan == "Not Planned", "Not Planned or Irrelevant", Pregnancy_Plan),
         Folic_Acid = if_else(Folic_Acid == "NA" | Folic_Acid == "No", "No or NA", Folic_Acid))


diagn_t1 <- diagn_table %>% 
  tbl_summary(by = Set,
              missing_text = "NA", 
              missing_stat = "{p_miss}%",
              type = list(where(is.numeric) ~ "continuous2"),
              digits = list(all_categorical() ~ c(0,1))) %>% 
  add_overall()


diagn_t1 %>% as_gt() %>% gt::gtsave("diagn_t1.docx")
