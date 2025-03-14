############################################
############ Figure Creation ###############
############################################


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
library(hstats) # Interaction Statistics
library(tidylog) # Logs of pre-processing
library(arrow) # Read/Write parquet files
library(easyalluvial) # For alluvial plots
library(ggalluvial) # For alluvial plots
library(scales) # For ggplot2 modifications

##################################

# The 11-y follow up initial data

final11 <- read_parquet("initial_diagnostic.parquet")

# The 18-y follow up data

final18 <- read_parquet("initial_prognostic.parquet")


# Also open the Eating disorders data frames

# Various eating disorders & behaviours

ED_11 <- read_parquet("ED_11.parquet") 

ED_18 <- read_parquet("ED_18.parquet") 



# We need to add purging to the dataset of 11-y follow up

final11 <- final11 |> 
  left_join(ED_11 |> select(lbgravff, Pd_11 = pd), by = "lbgravff") |> 
  mutate(Pd_11 = if_else(Pd_11 == 1, "Purging", "No_Purging")) |> 
  mutate_at(vars(Pd_11),as.factor)

########## Figure 1 is a flowchart and created from the data manually ##########


########### Creation of Figure 2 (bar-plots) ###############


f1 <- final11 |> 
  mutate(ED_Diagn_11 = if_else(ed_6_11== "ED_6_11",1,0)) %>% 
  mutate(AN_11 = if_else(AN_11 == "Anorexia",1,0)) %>%
  mutate(BN_11 = if_else(BN_11 == "BN",1,0)) %>%
  mutate(BED_11 = if_else(BED_11 == "BED",1,0)) %>%
  mutate(SubAN_11 = if_else(SubAN_11 == "Sub_Anorexia",1,0)) %>%
  mutate(SubBN_11 = if_else(SubBN_11 == "Sub_BN",1,0)) %>%
  mutate(SubBed_11 = if_else(SubBed_11 == "Sub_BED",1,0)) %>%
  mutate(Deb_11 = if_else(Deb_11 == "Deb",1,0)) %>%
  mutate(ED_11 = if_else(ED_11 == "Eating_Disorder",1,0)) %>%
  mutate(Subed_11 = if_else(Subed_11 == "Sub_ED",1,0)) %>%
  mutate(Pd_11 = if_else(Pd_11 == "Purging", 1, 0)) |> 
  select(AN_11,BN_11,BED_11,SubAN_11,SubBN_11,SubBed_11,Deb_11,ED_11,Subed_11,ED_Diagn_11,Pd_11) |> 
  pivot_longer(cols = c(AN_11,BN_11,BED_11,SubAN_11,SubBN_11,Pd_11,
                        SubBed_11,Deb_11,ED_Diagn_11),
               names_to = "EDs",values_to = "ED_Counts") |> 
  mutate_at(vars(EDs),as.factor) |> 
  group_by(EDs) |> 
  summarise(Sums = sum(ED_Counts,na.rm = T)) |>
  mutate(EDs = if_else(EDs == "BN_11" | EDs == "SubBN_11", "(Sub)-Bulimia", EDs)) |>
  mutate_at(vars(EDs),as.factor) |> 
  filter(EDs != "BN_11" | EDs != "SubBN_11") |> 
  group_by(EDs) |> 
  summarise(Sums = sum(Sums)) |> 
  ungroup() |> 
  mutate(EDs = fct_reorder(EDs,Sums,.na_rm = T)) |> 
  ggplot(aes(x = EDs, y = Sums, fill = EDs,label = Sums)) + 
  geom_col() + 
  ggtext::geom_richtext() +
  ggsci::scale_fill_futurama(alpha = 0.5) +
  theme_gray(base_size = 12) +
  theme(legend.position = "topleft") +
  scale_y_continuous(n.breaks = 15, limits = c(0,2700)) +
  labs(x = "Type of Eating Disorder at 11", 
       y = "Eating Disorder Frequency at 11",
       subtitle = "N = 44,357") +
  coord_flip() +
  scale_x_discrete(labels = c("Diagnosed ED", "(Sub)-Bulimia", 
                              "Purging", "Sub-Anorexia",
                              "Threshold Anorexia", "BED", "Sub-BED","DEB")) 


f2 <- final18 |> 
  mutate(ED_Diagn_18 = if_else(ED_Diagn_18== "ED",1,0)) %>% 
  mutate(AN_18 = if_else(AN_18 == "Anorexia",1,0)) %>%
  mutate(BN_18 = if_else(BN_18 == "BN",1,0)) %>%
  mutate(BED_18 = if_else(BED_18 == "BED",1,0)) %>%
  mutate(SubAN_18 = if_else(SubAN_18 == "Sub_Anorexia",1,0)) %>%
  mutate(SubBN_18 = if_else(SubBN_18 == "Sub_BN",1,0)) %>%
  mutate(SubBed_18 = if_else(SubBed_18 == "Sub_BED",1,0)) %>%
  mutate(Deb_18 = if_else(Deb_18 == "Deb",1,0)) %>%
  mutate(ED_18 = if_else(ED_18 == "Eating_Disorder",1,0)) %>%
  mutate(Subed_18 = if_else(Subed_18 == "Sub_ED",1,0)) %>%
  mutate(Pd_18 = if_else(Pd_18 == "1", 1, 0)) |> 
  select(AN_18,BN_18,BED_18,SubAN_18,SubBN_18,SubBed_18,Deb_18,ED_Diagn_18,Pd_18) |>
  pivot_longer(cols = c(AN_18,BN_18,BED_18,SubAN_18,SubBN_18,SubBed_18,Deb_18,
                        ED_Diagn_18,Pd_18),
               names_to = "EDs",values_to = "ED_Counts") |> 
  mutate_at(vars(EDs),as.factor) |> 
  group_by(EDs) |> 
  summarise(Sums = sum(ED_Counts,na.rm = T)) |> 
  mutate(EDs = fct_reorder(EDs,Sums,.na_rm = T)) |> 
  ggplot(aes(x = EDs, y = Sums, fill = EDs, label = Sums)) + 
  geom_col() + 
  ggtext::geom_richtext() +
  ggsci::scale_fill_futurama(alpha = 0.5) +
  theme_gray(base_size = 12) +
  theme(legend.position = "topleft") +
  scale_y_continuous(n.breaks = 15, limits = c(0,2700)) +
  scale_x_discrete(labels = c("Sub-Bulimia", "Threshold Bulimia", "Purging", "Diagnosed ED",
                              "Threshold Anorexia", "BED", "Sub-BED", "Sub-Anorexia","DEB")) +
  labs(x = "Type of Eating Disorder at 18", 
       y = "Eating Disorder Frequency at 18",
       subtitle = "N = 26,127") + 
  coord_flip()

figure2 <- f1/f2


# Save the figure

ggsave("Figure2.pdf")


##############################################
######### Figure 3/Alluvial Plot #############
##############################################

# Load the data

load("prognostic_main_env.RDATA")


# Prepare the data for the alluvial plot

izi_aluv <- final18 %>% 
  mutate(ED_Diagn_11 = if_else(ed_6_11== "ED_6_11",1,0)) %>% 
  mutate(AN_11 = if_else(AN_11 == "Anorexia",1,0)) %>%
  mutate(BN_11 = if_else(BN_11 == "BN",1,0)) %>%
  mutate(BED_11 = if_else(BED_11 == "BED",1,0)) %>%
  mutate(SubAN_11 = if_else(SubAN_11 == "Sub_Anorexia",1,0)) %>%
  mutate(SubBN_11 = if_else(SubBN_11 == "Sub_BN",1,0)) %>%
  mutate(SubBed_11 = if_else(SubBed_11 == "Sub_BED",1,0)) %>%
  mutate(Deb_11 = if_else(Deb_11 == "Deb",1,0)) %>%
  mutate(ED_11 = if_else(ED_11 == "Eating_Disorder",1,0)) %>%
  mutate(Subed_11 = if_else(Subed_11 == "Sub_ED",1,0)) %>%
  mutate(ED_Diagn_18 = if_else(ED_Diagn_18== "ED",1,0)) %>% 
  mutate(AN_18 = if_else(AN_18 == "Anorexia",1,0)) %>%
  mutate(BN_18 = if_else(BN_18 == "BN",1,0)) %>%
  mutate(BED_18 = if_else(BED_18 == "BED",1,0)) %>%
  mutate(SubAN_18 = if_else(SubAN_18 == "Sub_Anorexia",1,0)) %>%
  mutate(SubBN_18 = if_else(SubBN_18 == "Sub_BN",1,0)) %>%
  mutate(SubBed_18 = if_else(SubBed_18 == "Sub_BED",1,0)) %>%
  mutate(Deb_18 = if_else(Deb_18 == "Deb",1,0)) %>%
  mutate(ED_18 = if_else(ED_18 == "Eating_Disorder",1,0)) %>%
  mutate(Subed_18 = if_else(Subed_18 == "Sub_ED",1,0)) %>%
  mutate(Pd_18 = if_else(Pd_18 == "1", 1, 0)) |> 
  select(Deb_11,ED_11,Subed_11,ED_Diagn_11,
         Deb_18,ED_18,Subed_18,ED_Diagn_18,id = lbgravff) |>
  mutate(Diagn_Thresh_11 = if_else(ED_11 == 1 | ED_Diagn_11 == 1, 1, 0)) %>%
  mutate(Diagn_Thresh_18 = if_else(ED_18 == 1 | ED_Diagn_18 == 1, 1, 0)) %>% 
  mutate(No_Disorder_18 = if_else(ED_18 == 0 & Subed_18 == 0 & ED_Diagn_18 == 0 & Deb_18 == 0, 1, 0)) %>% 
  mutate_at(vars(Deb_11,Subed_11, Diagn_Thresh_11,
                 Deb_18,Diagn_Thresh_18,Subed_18, No_Disorder_18, ED_Diagn_18, ED_18),as.factor)


izi_aluv_1 <- 
  izi_aluv %>% 
  select(Deb_11,Subed_11,Diagn_Thresh_11,id) %>% 
  pivot_longer(cols = -id) %>% 
  rename(Disease_11 = name, Status_11 = value)

izi_aluv_2 <- izi_aluv %>% 
  select(Deb_18,Subed_18,Diagn_Thresh_18, No_Disorder_18,id) %>% 
  pivot_longer(cols = -id) %>% 
  rename(Disease_18 = name, Status_18 = value)


izi_aluv_3 <- izi_aluv_1 %>% full_join(izi_aluv_2,by = "id")


# Final dataframe for the creation


izi <- izi_aluv_3 %>% 
  filter(Status_11 == 1) %>% 
  select(-Status_11) %>% 
  group_by(Status_18,Disease_11,Disease_18) %>% 
  summarise(Freq = n())

# Change the levels

izi$Disease_11 <- as.factor(izi$Disease_11)
izi$Disease_18 <- as.factor(izi$Disease_18)
izi$Status_18 <- as.factor(izi$Status_18)

levels(izi$Disease_11) <- c("Disordered Eating Behaviours at 11", 
                            "Diagnosed or Threshold Eating Disorder at 11",
                            "Subthreshold Eating Disorder at 11")

levels(izi$Disease_18) <- c("Disordered Eating Behaviours at 18",
                            "Diagnosed or Threshold Eating Disorder at 18",
                            "No Eating Disorder at 18",
                            "Subthreshold Eating Disorder at 18")

levels(izi$Status_18) <- c("Negative at 18", "Positive at 18")


izi <- izi %>% 
  mutate_at(vars(Status_18),as.factor)


# Check if the data is fine for the package

is_alluvia_form(as.data.frame(izi),axes = 1:3, silent = T)


al1 <- izi %>% 
  filter(Status_18 == "Positive at 18") %>% 
  select(-Status_18) %>% 
  rename(Disorder_11 = Disease_11) %>% 
  mutate(Disorder_11 = factor(Disorder_11,levels = c("Disordered Eating Behaviours at 11",
                                                     "Subthreshold Eating Disorder at 11",
                                                     "Diagnosed or Threshold Eating Disorder at 11")),
         Disease_18 = factor(Disease_18,levels = c("No Eating Disorder at 18",
                                                   "Disordered Eating Behaviours at 18",
                                                   "Diagnosed or Threshold Eating Disorder at 18",
                                                   "Subthreshold Eating Disorder at 18")))

# Compute the proportions

total_freq <- sum(al1$Freq)

al1$Prop <- al1$Freq / total_freq


# Now plotting the alluvial 

figure3 <- al1 %>% 
  ggplot(aes(y = Prop,axis1 = Disorder_11, axis2 = Disease_18)) +
  geom_alluvium(aes(fill = Disorder_11),width = 1/12,alpha = 0.6, color = "white") +
  geom_stratum(width = 1/12,fill = "hotpink4", color = "black") +
  ggtext::geom_richtext(stat = "stratum",aes(label = after_stat(stratum)), 
                        fill = "white", color = "black") +
  scale_x_discrete(limits = c("Disorder_11","Disease_18"),
                   labels = c("11-year follow-up", "18-year follow-up"),
                   expand = c(0.05,0.05)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = c(0,0), breaks = seq(0,1,by = 0.1)) +
  theme_gray(base_size = 14) +
  ggsci::scale_fill_futurama() +
  labs(y = "Percentage", x = "") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 14)
  )



##############################################
######### Figure 4/SHAP Prognostic ###########
##############################################


# And now we plot the SHAP-values plot

set.seed(66)

figure4 <- sv_importance(shapp_prognostic_main,max_display = 10,kind = "beeswarm") + 
  theme_gray(base_size = 12) +
  scale_y_discrete(labels = c("Conduct problems SDQ scale (child-reported)",
                              "Child's Body Mass Index at 7 years follow up",
                              "Maternal Body Mass Index",
                              "Stress in Children (SiC) score",
                              "Paternal Body Mass Index",
                              "Hyperactivity/Inattention SDQ scale (parent-reported)",
                              "Peer relationship problems SDQ scale (parent-reported)",
                              "Body satisfaction score",
                              "Emotional symptoms SDQ scale (child-reported)",
                              "Sex = Male")) +
  labs(x = "Average absolute SHAP value")

# Save the figure

ggsave("figure4.pdf")


#######################################
######### Figure 5 / DCA both Task ####
#######################################

load("diagnostic_main_env.RData")


figure5 <- dca_diagn_main / dca_progn_main


# Save the figure

ggsave("figure5.pdf")



#######################################
############# Extended Data ###########
#######################################


### Extended Data Figure 1

set.seed(66)

diagn_shap <- sv_importance(shapp_diagnostic_main,max_display = 10,kind = "bar", fill = "#008EA0FF") + 
  theme_gray(base_size = 12) +
  scale_x_continuous(limits = c(0,0.8)) +
  scale_y_discrete(labels = c("Lack of sleep = Rarely or Never",
                              "Depression Feelings = Yes",
                              "Lost contact with a friend = Yes",
                              "Peer relationship problems SDQ scale (child-reported)",
                              "Conduct problems SDQ scale (child-reported)",
                              "Obsessive Compulsive Disorder Symptoms = Frequent",
                              "Hyperactivity/Inattention SDQ scale (child-reported)",
                              "Stress in Children (SiC) score",
                              "Emotional symptoms SDQ scale (child-reported)",
                              "Body satisfaction score")) +
  labs(x = "", title = "Diagnostic Task")

set.seed(66)

progn_shap <- sv_importance(shapp_prognostic_main,max_display = 10,kind = "bar", fill = "#008EA0FF" ) + 
  theme_gray(base_size = 12) +
  scale_x_continuous(limits = c(0,0.8)) +
  scale_y_discrete(labels = c("Conduct problems SDQ scale (child-reported)",
                              "Child's Body Mass Index at 7 years follow up",
                              "Maternal Body Mass Index",
                              "Stress in Children (SiC) score",
                              "Paternal Body Mass Index",
                              "Hyperactivity/Inattention SDQ scale (parent-reported)",
                              "Peer relationship problems SDQ scale (parent-reported)",
                              "Body satisfaction score",
                              "Emotional symptoms SDQ scale (child-reported)",
                              "Sex = Male")) +
  labs(x = "Average absolute SHAP value", title = "Prognostic Task")

ext_fig1 <- diagn_shap / progn_shap


# Save figure

ggsave("ext_fig1.pdf")



# Extended data Figure 2

ext_fig2 <- xg_final_fit_diagnostic_main |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at time zero", 
       y = "Observed event proportion of composite outcome at time zero")

# Save figure

ggsave("ext_fig2.pdf")


# Extended data Figure 3

ext_fig3 <- xg_final_fit_prognostic_main |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at 18", 
       y = "Observed event proportion of composite outcome at 18")

# Save figure

setwd("E:/workdata/707912/Users/Alex Katsiferis/Prediction Project/Coding/Analysis Data/Pre-Processing Data")

ggsave("ext_fig3.pdf")


# Extended data Figure 4

ext_fig4 <- partials_progn_main

# Save the figure

ggsave("ext_fig4.pdf")


# Extended data Figure 5

set.seed(66)

ext_fig5 <- sv_importance(shapp_diagnostic_main,max_display = 10,kind = "beeswarm") + 
  theme_gray(base_size = 12) +
  scale_y_discrete(labels = c("Lack of sleep = Rarely or Never",
                              "Depression Feelings = Yes",
                              "Lost contact with a friend = Yes",
                              "Peer relationship problems SDQ scale (child-reported)",
                              "Conduct problems SDQ scale (child-reported)",
                              "Obsessive Compulsive Disorder Symptoms = Frequent",
                              "Hyperactivity/Inattention SDQ scale (child-reported)",
                              "Stress in Children (SiC) score",
                              "Emotional symptoms SDQ scale (child-reported)",
                              "Body satisfaction score"))

# Save figure

ggsave("ext_fig5.pdf")


# Extended data Figure 6

# Test the resampled performance of the logistic regression models

# First create some folds of the original dataset

set.seed(243)

overall_diagn_folds <- vfold_cv(data = diagn_df_main,v = 5,strata = Status11)
overall_progn_folds <- vfold_cv(data = progn_df_main,v = 5, strata = Status18)


# First for prognostic

logistic_imp_progn <- logistic_reg(mode = "classification",engine = "glm")


logistic_imp_progn_recipe_full <- recipe(Status18 ~ cemotion11_child + Sex + Body_Score + ppeer11_parent + 
                                           phyper11_parent + bmi7 + BMI_Mother_11 + cconduct11_child + BMI_Father + GMS, 
                                           data = progn_df_main) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_ns(all_numeric_predictors(),deg_free = 4)


logistic_imp_progn_wf_full <- workflow(logistic_imp_progn_recipe_full,logistic_imp_progn)


final_cv_progn <- logistic_imp_progn_wf_full %>% 
  fit_resamples(resamples = overall_progn_folds,metrics = metric_set(roc_auc,brier_class),
                control = control_resamples(save_pred = T))


# Second for diagnostic

logistic_imp_diagn <- logistic_reg(mode = "classification",engine = "glm")


logistic_imp_diagn_recipe_full <- recipe(Status11 ~ 
                                          Body_Score + Lack_Of_Sleep + cemotion11_child + OCD_Symptoms + chyper11_child + 
                                          cconduct11_child + cpeer11_child + Lost_Contact_Friend + Depression_Feelings +
                                          GMS,
                                          data = diagn_df_main) %>% 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(Lack_Of_Sleep, OCD_Symptoms, Lost_Contact_Friend,new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_ns(all_numeric_predictors(),deg_free = 4)


logistic_imp_diagn_wf_full <- workflow(logistic_imp_diagn_recipe_full,logistic_imp_diagn)


final_cv_diagn <- logistic_imp_diagn_wf_full %>% 
  fit_resamples(resamples = overall_diagn_folds,metrics = metric_set(roc_auc,brier_class),
                control = control_resamples(save_pred = T))


# Evaluate decision curve analysis for prognostic set using cross validation

# Full Model

formula_progn_1 = Status18 ~ Sex + rcs(cemotion11_child) + rcs(Body_Score) + 
  rcs(ppeer11_parent) + rcs(phyper11_parent) + rcs(bmi7) + 
  rcs(BMI_Mother_11) + rcs(cconduct11_child) + rcs(BMI_Father) + 
  rcs(GMS)

formula_progn_2 = Status18 ~ Sex + cemotion11_child

set.seed(2244)

progn_cv_main <- progn_df_main

progn_cv_main <- progn_cv_main %>% 
  mutate(Status18 = if_else(Status18 == "ED_Positive",1,0))

cv_samples <- vfold_cv(progn_cv_main,v = 5,strata = Status18)

crossval_preds_progn_1 <- 
  cv_samples %>% 
  rowwise() %>% 
  mutate(
    glm_analysis = 
      glm(formula = formula_progn_1,
          data = rsample::analysis(splits),
          family = binomial
      ) %>% 
      list(),
    df_assessment = broom::augment(glm_analysis,newdata = rsample::assessment(splits),
                                   type.predict = "response") %>% 
      list()) %>% 
  ungroup() %>% 
  pull(df_assessment) %>% 
  bind_rows() %>% 
  group_by(lbgravff) %>% 
  summarise(cv_pred_1 = mean(.fitted),.groups = "drop") %>% 
  ungroup()


df_cv_pred_1 <- progn_cv_main %>% 
  left_join(crossval_preds_progn_1, by = "lbgravff")


# Simple Model

crossval_preds_progn_2 <- 
  cv_samples %>% 
  rowwise() %>% 
  mutate(
    glm_analysis = 
      glm(formula = formula_progn_2,
          data = rsample::analysis(splits),
          family = binomial
      ) %>% 
      list(),
    df_assessment = broom::augment(glm_analysis,newdata = rsample::assessment(splits),
                                   type.predict = "response") %>% 
      list()) %>% 
  ungroup() %>% 
  pull(df_assessment) %>% 
  bind_rows() %>% 
  group_by(lbgravff) %>% 
  summarise(cv_pred_2 = mean(.fitted),.groups = "drop") %>% 
  ungroup()


df_cv_pred_1 <- df_cv_pred_1 %>% 
  left_join(crossval_preds_progn_2, by = "lbgravff")


dca_suppl2 <- dcurves::dca(data = df_cv_pred_1,
                           formula = Status18 ~ cv_pred_1 + cv_pred_2,
                           thresholds = seq(0,0.1,0.01),
                           label = list(cv_pred_1 = "Cross-validated reduced model",
                                        cv_pred_2 = "Cross-validated simple model")) %>% 
  plot(smooth = T) +
  theme_gray(base_family = 12) + 
  ggsci::scale_colour_futurama() +
  theme(legend.position = "topleft") +
  labs(y = "Net benefit for prognostic task", x = "Threshold probability for intervention")



# Evaluating decision curve analysis for diagnostic set using cross validation

# Full Model

formula_diagn_1 = Status11 ~ 
  rcs(Body_Score) + Lack_Of_Sleep + rcs(cemotion11_child) + 
  OCD_Symptoms + rcs(chyper11_child) + rcs(cconduct11_child) + 
  rcs(cpeer11_child) + Lost_Contact_Friend + Depression_Feelings + 
  rcs(GMS)

formula_diagn_2 = Status11 ~ cemotion11_child + Body_Score

set.seed(2244)

diagn_cv_main <- diagn_df_main

diagn_cv_main <- diagn_cv_main %>% 
  mutate(Status11 = if_else(Status11 == "ED_Positive",1,0))

cv_samples_diagn <- vfold_cv(diagn_cv_main,v = 5,strata = Status11)

crossval_preds_diagn_1 <- 
  cv_samples_diagn %>% 
  rowwise() %>% 
  mutate(
    glm_analysis = 
      glm(formula = formula_diagn_1,
          data = rsample::analysis(splits),
          family = binomial
      ) %>% 
      list(),
    df_assessment = broom::augment(glm_analysis,newdata = rsample::assessment(splits),
                                   type.predict = "response") %>% 
      list()) %>% 
  ungroup() %>% 
  pull(df_assessment) %>% 
  bind_rows() %>% 
  group_by(lbgravff) %>% 
  summarise(cv_pred_1 = mean(.fitted),.groups = "drop") %>% 
  ungroup()


df_cv_pred_1_diagn <- diagn_cv_main %>% 
  left_join(crossval_preds_diagn_1, by = "lbgravff")


# Simple Model

crossval_preds_diagn_2 <- 
  cv_samples_diagn %>% 
  rowwise() %>% 
  mutate(
    glm_analysis = 
      glm(formula = formula_diagn_2,
          data = rsample::analysis(splits),
          family = binomial
      ) %>% 
      list(),
    df_assessment = broom::augment(glm_analysis,newdata = rsample::assessment(splits),
                                   type.predict = "response") %>% 
      list()) %>% 
  ungroup() %>% 
  pull(df_assessment) %>% 
  bind_rows() %>% 
  group_by(lbgravff) %>% 
  summarise(cv_pred_2 = mean(.fitted),.groups = "drop") %>% 
  ungroup()


df_cv_pred_1_diagn <- df_cv_pred_1_diagn %>% 
  left_join(crossval_preds_diagn_2, by = "lbgravff")


dca_suppl1 <- dcurves::dca(data = df_cv_pred_1_diagn,
                           formula = Status11 ~ cv_pred_1 + cv_pred_2,
                           thresholds = seq(0,0.1,0.01),
                           label = list(cv_pred_1 = "Cross-validated reduced model",
                                        cv_pred_2 = "Cross-validated simple model")) %>% 
  plot(smooth = T) +
  theme_gray(base_family = 12) + 
  ggsci::scale_colour_futurama() +
  theme(legend.position = "top") +
  labs(y = "Net benefit for diagnostic task", x = NULL)


# Extended Figure 5 below

ext_fig6 <- dca_suppl1 / dca_suppl2

# Save figure

ggsave("ext_fig6.pdf")


