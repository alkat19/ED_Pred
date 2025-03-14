###############################################
##### Analysis of main diagnostic outcome #####
###############################################


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

final11_main <- final11 %>%  
  mutate(Status11 = if_else(ED_11 == "Eating_Disorder" | 
                              ed_6_11 == "ED_6_11" |
                              Subed_11 == "Sub_ED",
                            "ED_Positive","ED_Negative"))  |>
  mutate_at(vars(Status11),as.factor)

# We create a data frame 

diagn_df_main  <- final11_main |> select(-ED_Diagn_18)


# Further modifications

diagn_df_main <- diagn_df_main |> 
  select(-bmi11,-ed_6_11) 

diagn_df_main <- diagn_df_main |> 
  select(-Feeling_Fat,-Want_For_Thin,
         -Binge_Eating,-Stomach_Binge,-AN_11,
         -BN_11,-BED_11,-SubAN_11,-SubBN_11,
         -SubBed_11,-Deb_11) 

diagn_df_main <- diagn_df_main |> 
  select(-ED_11,-Subed_11)

diagn_df_main$Status11 <- relevel(diagn_df_main$Status11,ref = "ED_Positive") 

diagn_df_main <- as.data.frame(diagn_df_main)

# Imputing NAs in the outcome with ED_Negative

diagn_df_main <- diagn_df_main |> 
  mutate(Status11 = if_else(is.na(Status11),"ED_Negative",Status11)) |> 
  mutate_at(vars(Status11),as.factor)


diagn_df_main$Status11 <- relevel(diagn_df_main$Status11,"ED_Positive")

# Renaming some variables 

diagn_df_main <- diagn_df_main |> 
  rename(Lost_Contact_Friend = "E023_15",
         Physical_Condition = "Self_Physical_Condition",
         Parent_Away_Work = "E023_12",
         Friend_Ill_Injured = "E023_14")


###################################
########## Modelling ##############
###################################

# Length of the diagnostic dataframe: 

dim(diagn_df_main) # 44357 individuals

# Let's look at the outcome

diagn_df_main %>% tabyl(Status11)

set.seed(19)

split_diagn_main <- initial_split(diagn_df_main,strata = Status11)

train_diagn_main <- training(split_diagn_main)

train_diagn_main <- train_diagn_main |> relocate(Status11,.before = lbgravff)

test_diagn_main <- testing(split_diagn_main)


# Create validation folds

set.seed(24)

diagn_folds_main <- vfold_cv(train_diagn_main,v = 5,strata = Status11)


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

xg_recipe_diagnostic_main <- recipe(Status11 ~ ., data = train_diagn_main) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,phyper7_parent,pconduct7_parent,pprosoc7_parent,
          pemotion7_parent,pimpact7_parent,BMI_Pre_Pregnancy, Maternal_Alcohol,
          ppeer7_parent,Maternal_Smoking) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors())


# Recipe for creation of imputed training set

xg_recipe_diagnostic_main_v2 <- recipe(Status11 ~ ., data = train_diagn_main) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,phyper7_parent,pconduct7_parent,pprosoc7_parent,
          pemotion7_parent,pimpact7_parent,BMI_Pre_Pregnancy, Maternal_Alcohol,
          ppeer7_parent,Maternal_Smoking) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) 


xg_wf_diagnostic_main <- workflow(xg_recipe_diagnostic_main,xg_model_diagnostic)


# For tuning saving

train_diagn_main_xg_tun <- bake(prep(xg_recipe_diagnostic_main), new_data = NULL)

# For logistic regression

train_diagn_main_xg <- bake(prep(xg_recipe_diagnostic_main_v2),new_data = NULL)


# Tuning of the model

set.seed(222)


tuning_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                min_n(),
                                loss_reduction(),
                                sample_size = sample_prop(),
                                finalize(mtry(),train_diagn_main_xg_tun),
                                learn_rate(),
                                stop_iter(),
                                scale_pos_weight(),size = 30)


set.seed(2)

doParallel::registerDoParallel(cores = 25)

xg_tuning_diagnostic_main <- tune_race_anova(xg_wf_diagnostic_main, 
                                             resamples = diagn_folds_main, 
                                             grid = tuning_grid, 
                                             metrics = metric_set(brier_class))


### Show me the best races of combinations

race_diagn <- plot_race(xg_tuning_diagnostic_main) +
  labs(x = "Number of fold resamples in diagnostic set", y = "Brier Score") +
  viridis::scale_colour_viridis(option = "magma",discrete = T) +
  theme_gray(base_size = 12)


### Now finalize workflow

set.seed(455)

xg_final_diagnostic_main <- finalize_workflow(xg_wf_diagnostic_main, 
                                              select_best(xg_tuning_diagnostic_main, 
                                                          metric = "brier_class"))

set.seed(99)

xg_final_fit_diagnostic_main <- xg_final_diagnostic_main |> 
  last_fit(split_diagn_main, 
           metrics = metric_set(roc_auc,average_precision,brier_class))

xg_final_fit_diagnostic_main %>% collect_metrics()


#######################################
############# Calibration #############
#######################################

xg_final_fit_diagnostic_main |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at time zero", 
       y = "Observed event proportion of composite outcome at time zero")


#########################
##### SHAP-Values #######
#########################


set.seed(24)

xg_visuals_diagnostic_main <- extract_workflow(xg_final_fit_diagnostic_main) |> 
  fit(train_diagn_main) |> 
  extract_fit_engine()

set.seed(26)

x_pred_prep_main <- train_diagn_main[sample(nrow(train_diagn_main),1000),]

x_pred_baked_main <- bake(prep(xg_recipe_diagnostic_main), new_data = x_pred_prep_main, 
                          has_role("predictor"), composition = "matrix")

set.seed(88)

shapp_diagnostic_main <- shapviz(object = xg_visuals_diagnostic_main, 
                                 X_pred = x_pred_baked_main)

set.seed(66)

sv_importance(shapp_diagnostic_main,max_display = 10,kind = "beeswarm") + 
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


###################################
######### Model comparison ########
###################################


# Create a final table with performance measures

test_diagn_main_xg <- bake(prep(xg_recipe_diagnostic_main_v2),new_data = test_diagn_main)

test_diagn_main_xg <- test_diagn_main_xg |> 
  mutate(Status = if_else(Status11 == "ED_Positive", 1, 0))

predictions_diagnostic_main <- xg_final_fit_diagnostic_main |> 
  collect_predictions() |> 
  select(.pred_ED_Positive)

predictions_diagnostic_main <- predictions_diagnostic_main$.pred_ED_Positive

# Put predictions on the data frame

test_diagn_main_xg$predictions_diagnostic_main <- predictions_diagnostic_main


# Now assess performance

# Compared with a logistic regression

train_diagn_main_xg <- train_diagn_main_xg %>% 
  mutate(Outcome = if_else(Status11 == "ED_Positive", 1, 0))

train_diagn_main_xg$Depression_Feelings <- as.character(train_diagn_main_xg$Depression_Feelings)

dd <- datadist(train_diagn_main_xg)

options(datadist = "dd")

log_diagn_main <- lrm(Outcome ~ rcs(Body_Score) +
                        Lack_Of_Sleep + 
                        rcs(cemotion11_child) +
                        OCD_Symptoms +
                        rcs(chyper11_child) + 
                        rcs(cconduct11_child) +
                        rcs(cpeer11_child) + 
                        Lost_Contact_Friend +
                        Depression_Feelings + 
                        rcs(GMS),
                      data = train_diagn_main_xg)

log_diagn_main_Body_Score <- lrm(Outcome ~ cemotion11_child + Body_Score,
                                 data = train_diagn_main_xg)

test_diagn_main_xg <- as.data.frame(test_diagn_main_xg)

preds_log_diagn_main <- predictRisk(log_diagn_main,newdata = test_diagn_main_xg)

preds_log_body_main <- predictRisk(log_diagn_main_Body_Score, newdata = test_diagn_main_xg)


# Define a model with only Body Score

log_diagn_main_single <-  lrm(Outcome ~ Body_Score,
                              data = train_diagn_main_xg)

test_diagn_main_xg <- as.data.frame(test_diagn_main_xg)


preds_log_diagn_single <- predictRisk(log_diagn_main_single, newdata = test_diagn_main_xg)


score_diagnostic_main <- Score(object = list(
  "XGBoost" = predictions_diagnostic_main,
  "GLM Full" = preds_log_diagn_main,
  "GLM Body" = preds_log_body_main,
  "GLM Single" = preds_log_diagn_single),
  formula = Status ~ 1, data = test_diagn_main_xg,
  metrics = c("auc","brier"),summary = "ipa",
  plots = "cal",se.fit = T)

summary(score_diagnostic_main,digits = 2)

plotCalibration(score_diagnostic_main)


###########################
######### DCA #############
###########################


test_diagn_main_xg$predictions_logistic_main <- preds_log_diagn_main
test_diagn_main_xg$predictions_body_main <- preds_log_body_main
test_diagn_main_xg$predictions_body_single <- preds_log_diagn_single


dca_diagn_main <- dca(Status ~ predictions_diagnostic_main + predictions_logistic_main + 
                        predictions_body_main + predictions_body_single,
                      data = test_diagn_main_xg,
                      thresholds = seq(0,0.1,by = 0.01),
                      label = list(predictions_diagnostic_main = "ML model",
                                   predictions_logistic_main = "Reduced model",
                                   predictions_body_main = "Simple model",
                                   predictions_body_single = "Single model")) |> 
  plot(smooth = T) +
  labs(x = NULL, y = "Net benefit for diagnostic task") +
  theme_gray(base_size = 12) +
  theme(legend.position = "top") +
  ggsci::scale_color_futurama()

dca_diagn_main

##############################################
####### Partial Dependence Plots #############
##############################################


# Exploring the partial dependencies


sq1 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "cemotion11_child",
                        X = bake(prep(xg_recipe_diagnostic_main), 
                                 new_data = NULL, has_role("predictor"), 
                                 composition = "matrix")),
            color = "#008EA0FF",show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = NULL, 
       x = "Emotional symptoms SDQ scale (child-reported)")



sq11 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "Body_Score",
                         X = bake(prep(xg_recipe_diagnostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",
             show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = "Risk at\ntime zero", 
       x = "Body satisfaction Score")



sq33 <- partial_dep(xg_visuals_diagnostic_main, v = "OCD_Symptoms_Frequent.OCD.Symptoms",
                   X = bake(prep(xg_recipe_diagnostic_main), 
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



sq55 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "cpeer11_child",
                         X = bake(prep(xg_recipe_diagnostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray() + 
  labs(y = NULL, x = "Peer relationship problems SDQ scale (child-reported)")




sq99 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "cconduct11_child",
                         X = bake(prep(xg_recipe_diagnostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Conduct problems SDQ scale (child-reported)")



sq100 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "chyper11_child",
                          X = bake(prep(xg_recipe_diagnostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Hyperactivity/Inattention SDQ scale (child-reported)")



sq101 <- partial_dep(xg_visuals_diagnostic_main, v = "Lost_Contact_Friend_Yes",
                   X = bake(prep(xg_recipe_diagnostic_main), 
                            new_data = NULL, has_role("predictor"), 
                            composition = "matrix"))

sq101 <- sq101$data |> 
  mutate(Lost_Contact_Friend = if_else(Lost_Contact_Friend_Yes == 0, "No","Yes")) |> 
  ggplot(aes(x = Lost_Contact_Friend, y = y, fill = Lost_Contact_Friend)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Lost contact with a friend")



sq102 <- partial_dep(xg_visuals_diagnostic_main, v = "Lack_Of_Sleep_Rarely.Never",
                   X = bake(prep(xg_recipe_diagnostic_main), 
                            new_data = NULL, has_role("predictor"), 
                            composition = "matrix"))

sq102 <- sq102$data |> 
  mutate(Lack_Of_Sleep = if_else(Lack_Of_Sleep_Rarely.Never == 0, "Sometimes or Frequent","Rarely or Never")) |> 
  ggplot(aes(x = Lack_Of_Sleep, y = y, fill = Lack_Of_Sleep)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Lack of sleep")


sq103 <- partial_dep(xg_visuals_diagnostic_main, v = "Depression_Feelings_Yes",
                     X = bake(prep(xg_recipe_diagnostic_main), 
                              new_data = NULL, has_role("predictor"), 
                              composition = "matrix"))

sq103 <- sq103$data |> 
  mutate(Depression_Feelings = if_else(Depression_Feelings_Yes == 0, "No","Yes")) |> 
  ggplot(aes(x = Depression_Feelings, y = y, fill = Depression_Feelings)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Depression Feelings")


sq104 <- plot(partial_dep(xg_visuals_diagnostic_main, v = "GMS",
                         X = bake(prep(xg_recipe_diagnostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Stress in Children (SiC) score")


# Put them all together

partials_diagn_main <- (sq11 + sq1) / (sq104 + sq55) / (sq99 + sq100) / (sq101 + sq102) / (sq103 + sq33)

partials_diagn_main

# Save some files needed

write_parquet(diagn_df_main,"diagnostic_full_main.parquet")

write_parquet(train_diagn_main,"diagnostic_train_main.parquet")

write_parquet(test_diagn_main,"diagnostic_test_main.parquet")
