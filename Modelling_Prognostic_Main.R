
###############################################
##### Analysis of main prognostic outcome #####
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
library(ranger) # Feauture importance with random forest
library(bonsai) # Tree-based models in tidymodels

##################################

# Load the dataset

final18 <- read_parquet("initial_prognostic.parquet")


# We care about diagnosed and threshold eating disorders

final18_main <- final18 %>%  
  mutate(Status18 = if_else(ED_18 == "Eating_Disorder" | 
                              ED_Diagn_18 == "ED",
                            "ED_Positive","ED_Negative"))  |>
  mutate_at(vars(Status18),as.factor)


############# Further modification of the data frame ###########

# We create a data frame 

progn_df_main  <- final18_main


# Remove redundant outcome definitions and create a final data frame

progn_df_main$G_COMPLETED_DATE <- final18_main$G_COMPLETED_DATE
progn_df_main$lbgravff <- final18_main$lbgravff


progn_df_main <- progn_df_main |> 
  select(-Feeling_Fat,-Want_For_Thin,
         -Binge_Eating,-Stomach_Binge,-AN_18,-BN_18,-BED_18,
         -SubAN_18,-SubBN_18,-SubBed_18,-Deb_18,-Pd_18,-ED_Diagn_18) 

progn_df_main <- progn_df_main |> 
  select(-ED_18,-Subed_18)

progn_df_main$Status18 <- relevel(progn_df_main$Status18,ref = "ED_Positive") 

progn_df_main <- as.data.frame(progn_df_main)

# Renaming some variables 

progn_df_main <- progn_df_main |> 
  rename(Lost_Contact_Friend = "E023_15",
         Physical_Condition = "Self_Physical_Condition",
         Parent_Away_Work = "E023_12",
         Friend_Ill_Injured = "E023_14") 

# Imputing NAs in outcome

progn_df_main <- progn_df_main |> 
  mutate(Status18 = if_else(is.na(Status18),"ED_Negative",Status18)) |> 
  mutate_at(vars(Status18),as.factor)


progn_df_main$Status18 <- relevel(progn_df_main$Status18,"ED_Positive")

#####################################
########## Modelling  ###############
#####################################


# Length of the prognostic data frame

dim(progn_df_main) # 26127 observations

set.seed(19) 

split_progn_main <- initial_split(progn_df_main,strata = Status18)

train_progn_main <- training(split_progn_main)

train_progn_main <- train_progn_main |> relocate(Status18,.before = lbgravff)

test_progn_main <- testing(split_progn_main)


# Create validation folds (cv5)

set.seed(24)

progn_folds_main <- vfold_cv(train_progn_main,v = 5,strata = Status18)


# Create an xgboost model

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

xg_recipe_prognostic_main <- recipe(Status18 ~ ., data = train_progn_main) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,AN_11,BN_11,BED_11,SubAN_11,Subed_11,
          SubBed_11,SubBN_11,SubBed_11,Deb_11,phyper7_parent,pprosoc7_parent,
          BMI_Pre_Pregnancy, Maternal_Smoking, Maternal_Alcohol,
          pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent,ED_11) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors())

# Recipe for creation of the imputed training set

xg_recipe_prognostic_main_v2 <- recipe(Status18 ~ ., data = train_progn_main) |> 
  step_rm(lbgravff,G_COMPLETED_DATE,AN_11,BN_11,BED_11,SubAN_11,Subed_11,
          SubBed_11,SubBN_11,SubBed_11,Deb_11,phyper7_parent,pprosoc7_parent,
          BMI_Pre_Pregnancy, Maternal_Smoking, Maternal_Alcohol,
          pconduct7_parent,pemotion7_parent,pimpact7_parent,ppeer7_parent,ED_11) |> 
  step_string2factor(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors(),new_level = "Unknown") |> 
  step_impute_median(all_numeric_predictors())


# For tuning saving

train_progn_progn_main_xg_tun <- bake(prep(xg_recipe_prognostic_main), new_data = NULL)

# For logistic regression
train_progn_main_xg <- bake(prep(xg_recipe_prognostic_main_v2),new_data = NULL)


##########################################

# Specify the xgboost workflow

xg_wf_prognostic_main <- workflow(xg_recipe_prognostic_main,xg_model_prognostic)


# Tuning of the model

set.seed(222)


tuning_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                min_n(),
                                loss_reduction(),
                                sample_size = sample_prop(),
                                finalize(mtry(),train_progn_progn_main_xg_tun),
                                learn_rate(),
                                stop_iter(),
                                scale_pos_weight(),size = 30)


set.seed(2)

doParallel::registerDoParallel(cores = 30)

xg_tuning_prognostic_main <- tune_race_anova(xg_wf_prognostic_main, 
                                             resamples = progn_folds_main, 
                                             grid = tuning_grid, 
                                             metrics = metric_set(brier_class))

 # Best races of combinations

race_progn <- plot_race(xg_tuning_prognostic_main) +
  labs(x = "Number of fold resamples in prognostic set", y = "Brier Score") +
  viridis::scale_colour_viridis(option = "magma",discrete = T) +
  theme_gray(base_size = 12)

# Tabular format of race

show_best(xg_tuning_prognostic_main,metric = "brier_class")


### Now finalize workflow

set.seed(455)

xg_final_prognostic_main <- finalize_workflow(xg_wf_prognostic_main, 
                                              select_best(xg_tuning_prognostic_main, 
                                                          metric = "brier_class"))

set.seed(99)

xg_final_fit_prognostic_main <- xg_final_prognostic_main |> 
  last_fit(split_progn_main, 
           metrics = metric_set(roc_auc,average_precision,brier_class))

xg_final_fit_prognostic_main %>% collect_metrics()


#######################################
############# Calibration #############
#######################################

xg_final_fit_prognostic_main |> cal_plot_windowed() + 
  theme_gray(base_size = 12) +
  labs(x = "Predicted probability of composite outcome at 18", 
       y = "Observed event proportion of composite outcome at 18")


##############################
######## SHAP Values #########
##############################

set.seed(24)

xg_visuals_prognostic_main <- extract_workflow(xg_final_fit_prognostic_main) |> 
  fit(train_progn_main) |> 
  extract_fit_engine()

set.seed(26)

x_pred_prep_main <- train_progn_main[sample(nrow(train_progn_main),1000),]

x_pred_baked_main <- bake(prep(xg_recipe_prognostic_main), new_data = x_pred_prep_main, 
                          has_role("predictor"), composition = "matrix")

set.seed(88)

shapp_prognostic_main <- shapviz(object = xg_visuals_prognostic_main, 
                                 X_pred = x_pred_baked_main)

set.seed(66)

sv_importance(shapp_prognostic_main,max_display = 10,kind = "beeswarm") + 
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


###########################
#### Model Comparison #####
###########################

# Apply the recipe to the testing data as well

test_progn_main_xg <- bake(prep(xg_recipe_prognostic_main_v2),new_data = test_progn_main)

test_progn_main_xg <- test_progn_main_xg |> 
  mutate(Status = if_else(Status18 == "ED_Positive", 1, 0))

predictions_prognostic_main <- xg_final_fit_prognostic_main |> 
  collect_predictions() |> 
  select(.pred_ED_Positive)

predictions_prognostic_main <- predictions_prognostic_main$.pred_ED_Positive

# Put predictions on the data frame

test_progn_main_xg$predictions_main <- predictions_prognostic_main


# Create a model that only uses top 2 most relevant predictors

log_sex <- logistic_reg(mode = "classification",engine = "glm")

log_recipe_sex_main <- recipe(Status18 ~ Sex + cemotion11_child, data = train_progn_main_xg)

log_wf_sex_main <- workflow(log_recipe_sex_main,log_sex)

preds_simple_sex_main <- predict(log_wf_sex_main |> 
                                   fit(train_progn_main_xg),
                                 new_data = test_progn_main_xg,type = "prob")$.pred_ED_Positive


# A full logistic regression model

train_progn_main_xg <- train_progn_main_xg %>% 
  mutate(Outcome = if_else(Status18 == "ED_Positive", 1, 0))

train_progn_main_xg <- train_progn_main_xg |> 
  mutate(Sex = if_else(Sex == "Unknown",NA,Sex))

train_progn_main_xg$Sex <- as.character(train_progn_main_xg$Sex)

dd <- datadist(train_progn_main_xg)

options(datadist = "dd")

log_progn_main <- lrm(Outcome ~ Sex + rcs(cemotion11_child) +
                        rcs(Body_Score) + rcs(ppeer11_parent) +
                        rcs(phyper11_parent) + rcs(bmi7) +
                        rcs(BMI_Mother_11) + rcs(cconduct11_child) +
                        rcs(BMI_Father) + rcs(GMS),
                       data = train_progn_main_xg,x = T, y = T)

log_progn_main_single <- lrm(Outcome ~ Sex,data = train_progn_main_xg, x = T, y = T)

test_progn_main_xg <- as.data.frame(test_progn_main_xg)

preds_log_progn_main <- predictRisk(log_progn_main, test_progn_main_xg)

preds_log_progn_main_single <- predictRisk(log_progn_main_single, test_progn_main_xg)

# Now assess performance

# Compared with a logistic regression

score_prognostic_main <- Score(object = list(
  "XGBoost" = predictions_prognostic_main,
  "Logistic Reg" = preds_log_progn_main,
  "Simple model" = preds_simple_sex_main,
  "Single model" = preds_log_progn_main_single ),
  formula = Status ~ 1, data = test_progn_main_xg ,
  metrics = c("auc","brier"),summary = "ipa",
  plots = "cal",se.fit = T)

summary(score_prognostic_main,digits =2,pvalue.digits = 2)

plotCalibration(score_prognostic_main,legend = F,round = F)


###################
###### DCA ########
###################

test_progn_main_xg$preds_sex_main <- preds_simple_sex_main
test_progn_main_xg$preds_logistic_main <- preds_log_progn_main
test_progn_main_xg$preds_logistic_main_single <- preds_log_progn_main_single


dca_progn_main <- dca(Status ~ predictions_prognostic_main + 
                        preds_logistic_main + preds_sex_main + preds_logistic_main_single,
                      data = test_progn_main_xg,
                      thresholds = seq(0,0.1,by = 0.01),
                      label = list(predictions_prognostic_main = "ML model", 
                                   preds_logistic_main = "Reduced model",
                                   preds_sex_main = "Simple model",
                                   preds_logistic_main_single = "Single model"
                      )) |> 
  plot(smooth = T) +
  labs(y = "Net benefit for prognostic task", 
       x = "Threshold probability for intervention") +
  theme_gray(base_size = 12) +
  theme(legend.position = "topleft") +
  ggsci::scale_colour_futurama()

dca_progn_main


#######################################
##### Partial Effects Plots ########
#######################################

dd <- datadist(train_progn_main_xg)

options(datadist = "dd")


# XGBOoost Partial Effects

# Exploring the partial dependencies


sq1 <- partial_dep(xg_visuals_prognostic_main, v = "Sex_Male",
                        X = bake(prep(xg_recipe_prognostic_main), 
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



sq11 <- plot(partial_dep(xg_visuals_prognostic_main, v = "cemotion11_child",
                         X = bake(prep(xg_recipe_prognostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",
             show_points = F) +
  theme_gray(base_size = 12) + 
  labs(y = "", 
       x = "Emotional symptoms SDQ scale (child-reported)")



sq33 <- plot(partial_dep(xg_visuals_prognostic_main, v = "Body_Score",
                         X = bake(prep(xg_recipe_prognostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) +
  labs(y = NULL, x = "Body satisfaction score")



sq55 <- plot(partial_dep(xg_visuals_prognostic_main, v = "ppeer11_parent",
                         X = bake(prep(xg_recipe_prognostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray() + 
  labs(y = NULL, x = "Peer relationship problems SDQ scale (parent-reported)")




sq99 <- plot(partial_dep(xg_visuals_prognostic_main, v = "phyper11_parent",
                         X = bake(prep(xg_recipe_prognostic_main), 
                                  new_data = NULL, has_role("predictor"), 
                                  composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Hyperactivity/Inattention SDQ scale (parent-reported)")



sq100 <- plot(partial_dep(xg_visuals_prognostic_main, v = "bmi7",
                          X = bake(prep(xg_recipe_prognostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Child's Body Mass Index at 7 years follow up")



sq101 <- plot(partial_dep(xg_visuals_prognostic_main, v = "BMI_Mother_11",
                          X = bake(prep(xg_recipe_prognostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Maternal Body Mass Index")



sq102 <- plot(partial_dep(xg_visuals_prognostic_main, v = "cconduct11_child",
                          X = bake(prep(xg_recipe_prognostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Conduct problems SDQ scale (child-reported)")


sq103 <- plot(partial_dep(xg_visuals_prognostic_main, v = "BMI_Father",
                          X = bake(prep(xg_recipe_prognostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Paternal Body Mass Index")


sq104 <- plot(partial_dep(xg_visuals_prognostic_main, v = "GMS",
                          X = bake(prep(xg_recipe_prognostic_main), 
                                   new_data = NULL, has_role("predictor"), 
                                   composition = "matrix")),color = "#008EA0FF",show_points = F) + 
  theme_gray(base_size = 12) + 
  labs(y = NULL, x = "Stress in Children (SiC) score")


# Put them all together

partials_progn_main <- (sq1 + sq11) / (sq33 + sq55) / (sq99 + sq100) / (sq101 + sq102) / (sq103 + sq104)

partials_progn_main


# Save some files needed

write_parquet(progn_df_main,"prognostic_full_main.parquet")

write_parquet(train_progn_main,"prognostic_train_main.parquet")

write_parquet(test_progn_main,"prognostic_test_main.parquet")
