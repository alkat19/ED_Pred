####################################################
##### Analysis of secondary diagnostic outcome #####
####################################################


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

##################################

# Load the dataset

final11 <- read_parquet("initial_diagnostic.parquet")


# Specifying the outcome

final11_sec <- final11 %>% 
  mutate(Status11 = if_else(ED_11 == "Eating_Disorder" | 
                              ed_6_11 == "ED_6_11" |
                              Subed_11 == "Sub_ED" | 
                              Deb_11 == "Deb", 
                            "ED_Positive","ED_Negative"))  |>
  mutate_at(vars(Status11),as.factor)

# We create a data frame 

diagn_df_sec  <- final11_sec |> select(-ED_Diagn_18)


# Further modifications

diagn_df_sec <- diagn_df_sec |> 
  select(-bmi11,-ed_6_11) 

diagn_df_sec <- diagn_df_sec |> 
  select(-Feeling_Fat,-Want_For_Thin,
         -Binge_Eating,-Stomach_Binge,-AN_11,
         -BN_11,-BED_11,-SubAN_11,-SubBN_11,
         -SubBed_11,-Deb_11) 

diagn_df_sec <- diagn_df_sec |> 
  select(-ED_11,-Subed_11)

diagn_df_sec$Status11 <- relevel(diagn_df_sec$Status11,ref = "ED_Positive") 

diagn_df_sec <- as.data.frame(diagn_df_sec)

# Imputing NAs in the outcome with ED_Negative

diagn_df_sec <- diagn_df_sec |> 
  mutate(Status11 = if_else(is.na(Status11),"ED_Negative",Status11)) |> 
  mutate_at(vars(Status11),as.factor)


diagn_df_sec$Status11 <- relevel(diagn_df_sec$Status11,"ED_Positive")

# Renaming some variables 

diagn_df_sec <- diagn_df_sec |> 
  rename(Lost_Contact_Friend = "E023_15",
         Physical_Condition = "Self_Physical_Condition",
         Parent_Away_Work = "E023_12",
         Friend_Ill_Injured = "E023_14")


###################################
########## Modelling ##############
###################################

# Length of the diagnostic dataframe: 

dim(diagn_df_sec) # 44357 individuals

# Let's look at the outcome

diagn_df_sec %>% tabyl(Status11)

set.seed(19)

split_diagn_sec <- initial_split(diagn_df_sec,strata = Status11)

train_diagn_sec <- training(split_diagn_sec)

train_diagn_sec <- train_diagn_sec |> relocate(Status11,.before = lbgravff)

test_diagn_sec <- testing(split_diagn_sec)


# Create validation folds

set.seed(24)

diagn_folds_sec <- vfold_cv(train_diagn_sec,v = 5,strata = Status11)


# Create model

xg_model_diagnostic <-  boost_tree(trees = tune(), 
                                   min_n = tune(), 
                                   mtry = tune(), 
                                   tree_depth = tune(), 
                                   learn_rate = tune(), 
                                   stop_iter = tune(), 
                                   loss_reduction = tune(), 
                                   sample_size = tune()) |> 
  set_engine("xgboost", validation = 0.2, scale_pos_weight = tune()) |> 
  set_mode("classification")

# Recipe for pre-processing

xg_recipe_diagnostic_sec <- recipe(Status11 ~ ., data = train_diagn_sec) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,phyper7_parent,pconduct7_parent,pprosoc7_parent,
          pemotion7_parent,pimpact7_parent,BMI_Pre_Pregnancy, Maternal_Alcohol,
          ppeer7_parent,Maternal_Smoking) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors())


# Recipe for creation of imputed training set

xg_recipe_diagnostic_sec_v2 <- recipe(Status11 ~ ., data = train_diagn_sec) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,phyper7_parent,pconduct7_parent,pprosoc7_parent,
          pemotion7_parent,pimpact7_parent,BMI_Pre_Pregnancy, Maternal_Alcohol,
          ppeer7_parent,Maternal_Smoking) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) 


xg_wf_diagnostic_sec <- workflow(xg_recipe_diagnostic_sec,xg_model_diagnostic)


# For tuning saving

train_diagn_sec_xg_tun <- bake(prep(xg_recipe_diagnostic_sec), new_data = NULL)

# For logistic regression

train_diagn_sec_xg <- bake(prep(xg_recipe_diagnostic_sec_v2),new_data = NULL)


# Tuning of the model

set.seed(222)


tuning_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                min_n(),
                                loss_reduction(),
                                sample_size = sample_prop(),
                                finalize(mtry(),train_diagn_sec_xg_tun),
                                learn_rate(),
                                stop_iter(),
                                scale_pos_weight(),size = 30)


set.seed(2)

doParallel::registerDoParallel(cores = 30)

xg_tuning_diagnostic_sec <- tune_race_anova(xg_wf_diagnostic_sec, 
                                             resamples = diagn_folds_sec, 
                                             grid = tuning_grid, 
                                             metrics = metric_set(brier_class))


### Show me the best races of combinations

race_diagn <- plot_race(xg_tuning_diagnostic_sec) +
  labs(x = "Number of fold resamples in diagnostic set", y = "Brier Score") +
  viridis::scale_colour_viridis(option = "magma",discrete = T) +
  theme_gray(base_size = 12)


### Now finalize workflow

set.seed(455)

xg_final_diagnostic_sec <- finalize_workflow(xg_wf_diagnostic_sec, 
                                              select_best(xg_tuning_diagnostic_sec, 
                                                          metric = "brier_class"))

set.seed(99)

xg_final_fit_diagnostic_sec <- xg_final_diagnostic_sec |> 
  last_fit(split_diagn_sec, 
           metrics = metric_set(roc_auc,average_precision,brier_class))

xg_final_fit_diagnostic_sec %>% collect_metrics()


#######################################
############# Calibration #############
#######################################

xg_final_fit_diagnostic_sec |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at time zero", 
       y = "Observed event proportion of composite outcome at time zero")


#########################
##### SHAP-Values #######
#########################


set.seed(24)

xg_visuals_diagnostic_sec <- extract_workflow(xg_final_fit_diagnostic_sec) |> 
  fit(train_diagn_sec) |> 
  extract_fit_engine()

set.seed(26)

x_pred_prep_sec <- train_diagn_sec[sample(nrow(train_diagn_sec),1000),]

x_pred_baked_sec <- bake(prep(xg_recipe_diagnostic_sec), new_data = x_pred_prep_sec, 
                          has_role("predictor"), composition = "matrix")

set.seed(88)

shapp_diagnostic_sec <- shapviz(object = xg_visuals_diagnostic_sec, 
                                 X_pred = x_pred_baked_sec)

set.seed(66)

sv_importance(shapp_diagnostic_sec,max_display = 10,kind = "beeswarm") + 
  theme_gray(base_size = 12) +
  scale_y_discrete(labels = c("Obsessive Compulsive Disorder Symptoms = Occasional",
                              "Hyperactivity/Inattention SDQ scale (child-reported)",
                              "Conduct problems SDQ scale (child-reported)",
                              "Stress in Chidlren (SiC) score",
                              "Self-rated physical condition = Not Good or Poor or Very Poor",
                              "Obsessive Compulsive Disorder Symptoms = Frequent",
                              "Peer relationship problems SDQ scale (child-reported)",
                              "Emotional symptoms SDQ scale at 7 years follow up",
                              "Child's Body Mass Index at 7 years follow up",
                              "Body satisfaction score"))


###################################
######### Model comparison ########
###################################


# Create a final table with performance measures

test_diagn_sec_xg <- bake(prep(xg_recipe_diagnostic_sec_v2),new_data = test_diagn_sec)

test_diagn_sec_xg <- test_diagn_sec_xg |> 
  mutate(Status = if_else(Status11 == "ED_Positive", 1, 0))

predictions_diagnostic_sec <- xg_final_fit_diagnostic_sec |> 
  collect_predictions() |> 
  select(.pred_ED_Positive)

predictions_diagnostic_sec <- predictions_diagnostic_sec$.pred_ED_Positive

# Put predictions on the data frame

test_diagn_sec_xg$predictions_diagnostic_sec <- predictions_diagnostic_sec


# Now assess performance

# Compared with a logistic regression

train_diagn_sec_xg <- train_diagn_sec_xg %>% 
  mutate(Outcome = if_else(Status11 == "ED_Positive", 1, 0))

dd <- datadist(train_diagn_sec_xg)

options(datadist = "dd")

log_diagn_sec <- lrm(Outcome ~ rcs(Body_Score) +
                        rcs(cemotion11_child) +
                        OCD_Symptoms +
                        rcs(chyper11_child) + 
                        rcs(cconduct11_child) +
                        rcs(cpeer11_child) + 
                        rcs(GMS) +
                        rcs(bmi7) +
                        Physical_Condition,
                      data = train_diagn_sec_xg)

log_diagn_sec_Body_Score <- lrm(Outcome ~ bmi7 + Body_Score,
                                 data = train_diagn_sec_xg)

log_diagn_sec_single <- lrm(Outcome ~ Body_Score, data = train_diagn_sec_xg)

test_diagn_sec_xg <- as.data.frame(test_diagn_sec_xg)

preds_log_diagn_sec <- predictRisk(log_diagn_sec,newdata = test_diagn_sec_xg)

preds_log_body_sec <- predictRisk(log_diagn_sec_Body_Score, newdata = test_diagn_sec_xg)

preds_log_single_sec <- predictRisk(log_diagn_sec_single, newdata = test_diagn_sec_xg)

score_diagnostic_sec <- Score(object = list(
  "XGBoost" = predictions_diagnostic_sec,
  "GLM Full" = preds_log_diagn_sec,
  "GLM Body" = preds_log_body_sec,
  "GLM Single" = preds_log_single_sec),
  formula = Status ~ 1, data = test_diagn_sec_xg,
  metrics = c("auc","brier"),summary = "ipa",
  plots = "cal",se.fit = T)

summary(score_diagnostic_sec,digits = 2)

plotCalibration(score_diagnostic_sec)


###########################
######### DCA #############
###########################


test_diagn_sec_xg$predictions_logistic_sec <- preds_log_diagn_sec
test_diagn_sec_xg$predictions_body_sec <- preds_log_body_sec
test_diagn_sec_xg$predictions_single_sec <- preds_log_single_sec

dca_diagn_sec <- dca(Status ~ predictions_diagnostic_sec + predictions_logistic_sec + 
                        predictions_body_sec + predictions_single_sec,
                      data = test_diagn_sec_xg,
                      thresholds = seq(0,0.6,by = 0.02),
                      label = list(predictions_diagnostic_sec = "ML model",
                                   predictions_logistic_sec = "Reduced Model",
                                   predictions_body_sec = "Simple Model",
                                   predictions_single_sec = "Single Model")) |> 
  plot(smooth = T) +
  labs(x = NULL, y = "Net benefit for diagnostic task") +
  theme_gray(base_size = 12) +
  theme(legend.position = "top") +
  ggsci::scale_color_futurama()

dca_diagn_sec

##############################################
####### Partial Dependence Plots #############
##############################################


# Exploring the partial dependencies


sq1 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "cemotion11_child",
                        X = bake(prep(xg_recipe_diagnostic_sec), 
                                 new_data = NULL, has_role("predictor"), 
                                 composition = "matrix")),
            color = "#008EA0FF",show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = NULL, 
       x = "Emotional symptoms SDQ scale (child-reported)")



sq11 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "Body_Score",
                         X = bake(prep(xg_recipe_diagnostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",
             show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = "Risk at\ntime zero", 
       x = "Body satisfaction Score")



sq33 <- partial_dep(xg_visuals_diagnostic_sec, v = "OCD_Symptoms_Frequent.OCD.Symptoms",
                    X = bake(prep(xg_recipe_diagnostic_sec), 
                             new_data = NULL, has_role("predictor"), 
                             composition = "matrix"))

sq33 <- sq33$data |> 
  mutate(OCD_Symptoms = if_else(OCD_Symptoms_Frequent.OCD.Symptoms == 0, "Not Frequent","Frequent")) |> 
  ggplot(aes(x = OCD_Symptoms, y = y, fill = OCD_Symptoms)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Obsessive Compulsive Disorder Symptoms")



sq55 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "cpeer11_child",
                         X = bake(prep(xg_recipe_diagnostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray() + 
  labs(y = NULL, x = "Peer relationship problems SDQ scale (child-reported)")




sq99 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "cconduct11_child",
                         X = bake(prep(xg_recipe_diagnostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Conduct problems SDQ scale (child-reported)")



sq100 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "chyper11_child",
                          X = bake(prep(xg_recipe_diagnostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Hyperactivity/Inattention SDQ scale (child-reported)")



sq101 <- partial_dep(xg_visuals_diagnostic_sec, v = "Physical_Condition_Not.so.Good.or.Poor.or.Very.Poor",
                     X = bake(prep(xg_recipe_diagnostic_sec), 
                              new_data = NULL, has_role("predictor"), 
                              composition = "matrix"))

sq101 <- sq101$data |> 
  mutate(Physical_Condition = if_else(Physical_Condition_Not.so.Good.or.Poor.or.Very.Poor == 0, "Good or Excellent","Not Good or Poor or Very Poor")) |> 
  ggplot(aes(x = Physical_Condition, y = y, fill = Physical_Condition)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Self-rated physical condition")



sq102 <- partial_dep(xg_visuals_diagnostic_sec, v = "OCD_Symptoms_Occasional.OCD.Symptoms",
                    X = bake(prep(xg_recipe_diagnostic_sec), 
                             new_data = NULL, has_role("predictor"), 
                             composition = "matrix"))

sq102 <- sq102$data |> 
  mutate(OCD_Symptoms = if_else(OCD_Symptoms_Occasional.OCD.Symptoms == 0, "Not Occasional","Occasional")) |> 
  ggplot(aes(x = OCD_Symptoms, y = y, fill = OCD_Symptoms)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Obsessive Compulsive Disorder Symptoms")


sq103 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "bmi7",
                           X = bake(prep(xg_recipe_diagnostic_sec), 
                                    new_data = NULL, has_role("predictor"), 
                                    composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Child's Body Mass Index at 7 years follow up")


sq104 <- plot(partial_dep(xg_visuals_diagnostic_sec, v = "GMS",
                          X = bake(prep(xg_recipe_diagnostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Stress in Children (SiC) score")


# Put them all together

partials_diagn_sec <- (sq11 + sq1) / (sq104 + sq55) / (sq99 + sq100) / (sq103 + sq102) / (sq101 + sq33)

partials_diagn_sec


# Save some files needed

write_parquet(diagn_df_sec,"diagnostic_full_sec.parquet")

write_parquet(train_diagn_sec,"diagnostic_train_sec.parquet")

write_parquet(test_diagn_sec,"diagnostic_test_sec.parquet")



