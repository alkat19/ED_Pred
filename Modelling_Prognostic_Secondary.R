#################################################
##### Analysis of secondary prognostic outcome ##
#################################################


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
library(ranger) # Feauture importance with random forest
library(bonsai) # Tree-based models in tidymodels

##################################

# Load the dataset

final18 <- read_parquet("initial_prognostic.parquet")


# We care about diagnosed and threshold eating disorders

final18_sec <- final18 %>% 
  mutate(Status18 = if_else(
    ED_18 == "Eating_Disorder" | ED_Diagn_18 == "ED" |
      Subed_18 == "Sub_ED", 
    "ED_Positive","ED_Negative"))  |>
  mutate_at(vars(Status18),as.factor)

############# Further modification of the data frame ###########

# We create a data frame 

progn_df_sec  <- final18_sec


# Remove redundant outcome definitions and create a final data frame

progn_df_sec$G_COMPLETED_DATE <- final18_sec$G_COMPLETED_DATE
progn_df_sec$lbgravff <- final18_sec$lbgravff


progn_df_sec <- progn_df_sec |> 
  select(-Feeling_Fat,-Want_For_Thin,
         -Binge_Eating,-Stomach_Binge,-AN_18,-BN_18,-BED_18,
         -SubAN_18,-SubBN_18,-SubBed_18,-Deb_18,-Pd_18,-ED_Diagn_18) 

progn_df_sec <- progn_df_sec |> 
  select(-ED_18,-Subed_18)

progn_df_sec$Status18 <- relevel(progn_df_sec$Status18,ref = "ED_Positive") 

progn_df_sec <- as.data.frame(progn_df_sec)

# Renaming some variables 

progn_df_sec <- progn_df_sec |> 
  rename(Lost_Contact_Friend = "E023_15",
         Physical_Condition = "Self_Physical_Condition",
         Parent_Away_Work = "E023_12",
         Friend_Ill_Injured = "E023_14") 

# Imputing NAs in outcome

progn_df_sec <- progn_df_sec |> 
  mutate(Status18 = if_else(is.na(Status18),"ED_Negative",Status18)) |> 
  mutate_at(vars(Status18),as.factor)


progn_df_sec$Status18 <- relevel(progn_df_sec$Status18,"ED_Positive")

#####################################
########## Modelling  ###############
#####################################


# Length of the prognostic data frame

dim(progn_df_sec) # 26127 observations

set.seed(19) 

split_progn_sec <- initial_split(progn_df_sec,strata = Status18)

train_progn_sec <- training(split_progn_sec)

train_progn_sec <- train_progn_sec |> relocate(Status18,.before = lbgravff)

test_progn_sec <- testing(split_progn_sec)


# Create validation folds (cv5)

set.seed(24)

progn_folds_sec <- vfold_cv(train_progn_sec,v = 5,strata = Status18)


# Create a random forest

xg_model_prognostic <- boost_tree(mtry = tune(),
                                  trees = tune(),
                                  min_n = tune(),
                                  tree_depth = tune(),
                                  sample_size = tune(),
                                  stop_iter = tune(),
                                  learn_rate = tune(),
                                  loss_reduction = tune()) |> 
  set_engine("xgboost",validation = 0.2,scale_pos_weight = tune()) |> 
  set_mode("classification")

# Recipe for pre-processing

xg_recipe_prognostic_sec <- recipe(Status18 ~ ., data = train_progn_sec) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,AN_11,BN_11,BED_11,SubAN_11,Subed_11,
          SubBed_11,SubBN_11,SubBed_11,Deb_11,phyper7_parent,pprosoc7_parent,
          BMI_Pre_Pregnancy, Maternal_Smoking, Maternal_Alcohol,
          pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent,ED_11) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

# Create the imputed training set

xg_recipe_prognostic_sec_v2 <- recipe(Status18 ~ ., data = train_progn_sec) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,AN_11,BN_11,BED_11,SubAN_11,Subed_11,
          SubBed_11,SubBN_11,SubBed_11,Deb_11,phyper7_parent,pprosoc7_parent,
          BMI_Pre_Pregnancy, Maternal_Smoking, Maternal_Alcohol,
          pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent,ED_11) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors())


# For tuning saving

train_progn_progn_sec_xg_tun <- bake(prep(xg_recipe_prognostic_sec), new_data = NULL)

# For logistic regression
train_progn_sec_xg <- bake(prep(xg_recipe_prognostic_sec_v2),new_data = NULL)


##########################################

# Specify the xgboost workflow

xg_wf_prognostic_sec <- workflow(xg_recipe_prognostic_sec,xg_model_prognostic)


# Tuning of the model

set.seed(222)


tuning_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                min_n(),
                                loss_reduction(),
                                sample_size = sample_prop(),
                                finalize(mtry(),train_progn_progn_sec_xg_tun),
                                learn_rate(),
                                stop_iter(),
                                scale_pos_weight(),size = 30)


set.seed(2)

doParallel::registerDoParallel(cores = 30)

xg_tuning_prognostic_sec <- tune_race_anova(xg_wf_prognostic_sec, 
                                             resamples = progn_folds_sec, 
                                             grid = tuning_grid, 
                                             metrics = metric_set(brier_class))

# Best races of combinations

race_progn <- plot_race(xg_tuning_prognostic_sec) +
  labs(x = "Number of fold resamples in prognostic set", y = "Brier Score") +
  viridis::scale_colour_viridis(option = "magma",discrete = T) +
  theme_gray(base_size = 12)

# Tabular format of race

show_best(xg_tuning_prognostic_sec,metric = "brier_class")


### Now finalize workflow

set.seed(455)

xg_final_prognostic_sec <- finalize_workflow(xg_wf_prognostic_sec, 
                                              select_best(xg_tuning_prognostic_sec, 
                                                          metric = "brier_class"))

set.seed(99)

xg_final_fit_prognostic_sec <- xg_final_prognostic_sec |> 
  last_fit(split_progn_sec, 
           metrics = metric_set(roc_auc,average_precision,brier_class))

xg_final_fit_prognostic_sec %>% collect_metrics()


#######################################
############# Calibration #############
#######################################

xg_final_fit_prognostic_sec |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at 18", 
       y = "Observed event proportion of composite outcome at 18")


##############################
######## SHAP Values #########
##############################

set.seed(24)

xg_visuals_prognostic_sec <- extract_workflow(xg_final_fit_prognostic_sec) |> 
  fit(train_progn_sec) |> 
  extract_fit_engine()

set.seed(26)

x_pred_prep_sec <- train_progn_sec[sample(nrow(train_progn_sec),1000),]

x_pred_baked_sec <- bake(prep(xg_recipe_prognostic_sec), new_data = x_pred_prep_sec, 
                          has_role("predictor"), composition = "matrix")

set.seed(88)

shapp_prognostic_sec <- shapviz(object = xg_visuals_prognostic_sec, 
                                 X_pred = x_pred_baked_sec)

set.seed(66)

sv_importance(shapp_prognostic_sec,max_display = 10,kind = "beeswarm") + 
  theme_gray(base_size = 12) +
  scale_y_discrete(labels = c("Hours of sleep",
                              "Child's Body Mass Index",
                              "Maternal Body Mass Index",
                              "Depression feelings = Yes",
                              "Emotional symptoms SDQ scale (child-reported)",
                              "Prosocial behaviour SDQ scale (parent-reported)",
                              "Children's Body Mass Index at 7 years follow up",
                              "Close female friends =  Three or more",
                              "Body satisfaction score",
                              "Sex = Male")) +
  labs(x = "Average absolute SHAP value")


###########################
#### Model Comparison #####
###########################

# Apply the recipe to the testing data as well

test_progn_sec_xg <- bake(prep(xg_recipe_prognostic_sec_v2),new_data = test_progn_sec)

test_progn_sec_xg <- test_progn_sec_xg |> 
  mutate(Status = if_else(Status18 == "ED_Positive", 1, 0))

predictions_prognostic_sec <- xg_final_fit_prognostic_sec |> 
  collect_predictions() |> 
  select(.pred_ED_Positive)

predictions_prognostic_sec <- predictions_prognostic_sec$.pred_ED_Positive

# Put predictions on the data frame

test_progn_sec_xg$predictions_sec <- predictions_prognostic_sec


# Create a model that only uses top 2 most relevant predictors

log_sex <- logistic_reg(mode = "classification",engine = "glm")

log_recipe_sex_sec <- recipe(Status18 ~ Sex + Body_Score, data = train_progn_sec_xg)

log_wf_sex_sec <- workflow(log_recipe_sex_sec,log_sex)

preds_simple_sex_sec <- predict(log_wf_sex_sec |> 
                                   fit(train_progn_sec_xg),
                                 new_data = test_progn_sec_xg,type = "prob")$.pred_ED_Positive


# A full logistic regression model

train_progn_sec_xg <- train_progn_sec_xg %>% 
  mutate(Outcome = if_else(Status18 == "ED_Positive", 1, 0))

train_progn_sec_xg <- train_progn_sec_xg |> 
  mutate(Sex = if_else(Sex == "Unknown",NA,Sex))

train_progn_sec_xg$Sex <- as.character(train_progn_sec_xg$Sex)

train_progn_sec_xg$Depression_Feelings <- as.character(train_progn_sec_xg$Depression_Feelings)

dd <- datadist(train_progn_sec_xg)

options(datadist = "dd")

log_progn_sec <- lrm(Outcome ~ Sex + rcs(Body_Score) + Close_Female_Friends +
                       rcs(bmi7) + rcs(pprosoc11_parent) + Depression_Feelings +
                       rcs(cemotion11_child) + rcs(BMI_Mother_11) +
                       rcs(bmi11) + rcs(Sleeping_Hours),
                       data = train_progn_sec_xg,x = T, y = T)

log_progn_sec_single <- lrm(Outcome ~ Sex, data = train_progn_sec_xg, x = T, y = T)

test_progn_sec_xg <- as.data.frame(test_progn_sec_xg)

preds_log_progn_sec <- predictRisk(log_progn_sec, test_progn_sec_xg)

preds_log_progn_sec_single <- predictRisk(log_progn_sec_single, test_progn_sec_xg)


# Now assess performance

# Compared with a logistic regression

score_prognostic_sec <- Score(object = list(
  "XGBoost" = predictions_prognostic_sec,
  "Logistic Reg" = preds_log_progn_sec,
  "Simple model" = preds_simple_sex_sec,
  "Single model" = preds_log_progn_sec_single),
  formula = Status ~ 1, data = test_progn_sec_xg ,
  metrics = c("auc","brier"),summary = "ipa",
  plots = "cal",se.fit = T)

summary(score_prognostic_sec,digits =2,pvalue.digits = 2)

plotCalibration(score_prognostic_sec,legend = F,round = F)


###################
###### DCA ########
###################

test_progn_sec_xg$preds_sex_sec <- preds_simple_sex_sec
test_progn_sec_xg$preds_logistic_sec <- preds_log_progn_sec
test_progn_sec_xg$preds_logistic_sec_single <- preds_log_progn_sec_single



dca_progn_sec <- dca(Status ~ predictions_prognostic_sec + preds_logistic_sec + 
                       preds_sex_sec + preds_logistic_sec_single,
                      data = test_progn_sec_xg,
                      thresholds = seq(0,0.2,by = 0.02),
                      label = list(predictions_prognostic_sec = "ML model", 
                                   preds_logistic_sec = "Reduced model",
                                   preds_sex_sec = "Simple model",
                                   preds_logistic_sec_single = "Single model"
                      )) |> 
  plot(smooth = T) +
  labs(y = "Net benefit for prognostic task", 
       x = "Threshold probability for intervention") +
  theme_gray(base_size = 12) +
  theme(legend.position = "topleft") +
  ggsci::scale_colour_futurama()

dca_progn_sec


#######################################
##### Partial Effects Plots ########
#######################################

dd <- datadist(train_progn_sec_xg)

options(datadist = "dd")


# XGBOoost Partial Effects

# Exploring the partial dependencies


sq1 <- partial_dep(xg_visuals_prognostic_sec, v = "Sex_Male",
                   X = bake(prep(xg_recipe_prognostic_sec), 
                            new_data = NULL, has_role("predictor"), 
                            composition = "matrix"))

sq1 <- sq1$data |> 
  mutate(Sex = if_else(Sex_Male == 0, "Female","Male")) |> 
  ggplot(aes(x = Sex, y = y, fill = Sex)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="Risk at 18-years", x = "Sex")



sq11 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "cemotion11_child",
                         X = bake(prep(xg_recipe_prognostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",
             show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = "", 
       x = "Emotional symptoms SDQ scale (child-reported)")



sq33 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "Body_Score",
                         X = bake(prep(xg_recipe_prognostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) +
  labs(y = NULL, x = "Body satisfaction score")



sq55 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "pprosoc11_parent",
                         X = bake(prep(xg_recipe_prognostic_sec), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray() + 
  labs(y = NULL, x = "Prosocial behaviour SDQ scale (parent-reported)")


sq99 <- partial_dep(xg_visuals_prognostic_sec, v = "Depression_Feelings_Yes",
                   X = bake(prep(xg_recipe_prognostic_sec), 
                            new_data = NULL, has_role("predictor"), 
                            composition = "matrix"))

sq99 <- sq99$data |> 
  mutate(Depression_Feelings = if_else(Depression_Feelings_Yes == 0, "No","Yes")) |> 
  ggplot(aes(x = Depression_Feelings, y = y, fill = Depression_Feelings)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Depression Feelings")


sq100 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "bmi7",
                          X = bake(prep(xg_recipe_prognostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Child's Body Mass Index at 7 years follow up")



sq101 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "BMI_Mother_11",
                          X = bake(prep(xg_recipe_prognostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Maternal Body Mass Index")



sq102 <- partial_dep(xg_visuals_prognostic_sec, v = "Close_Female_Friends_Three.or.more",
                   X = bake(prep(xg_recipe_prognostic_sec), 
                            new_data = NULL, has_role("predictor"), 
                            composition = "matrix"))

sq102 <- sq102$data |> 
  mutate(Close_Female_Friends = if_else(Close_Female_Friends_Three.or.more == 0, "Less or none","Three or more")) |> 
  ggplot(aes(x = Close_Female_Friends, y = y, fill = Close_Female_Friends)) +
  scale_fill_manual(values = c("#008EA0FF","#008EA0FF")) +
  geom_col() +
  theme_gray(base_size = 12) + 
  theme(legend.position = "topleft") +
  labs(y ="", x = "Close Female Friends")


sq104 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "bmi11",
                          X = bake(prep(xg_recipe_prognostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Child's Body Mass Index")


sq105 <- plot(partial_dep(xg_visuals_prognostic_sec, v = "Sleeping_Hours",
                          X = bake(prep(xg_recipe_prognostic_sec), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Hours of sleep")


# Put them all together

partials_progn_sec <- (sq1 + sq99) / (sq102 + sq55) / (sq11 + sq100) / (sq101 + sq33) / (sq104 + sq105)

partials_progn_sec


# Save some files needed

write_parquet(progn_df_sec,"prognostic_full_sec.parquet")

write_parquet(train_progn_sec,"prognostic_train_sec.parquet")

write_parquet(test_progn_sec,"prognostic_test_sec.parquet")
