rm(list = ls())
options(scipen = 999)
set.seed(10)

# Packages for Data Manipulation
library("dplyr")
library("tidyverse")
library("data.table")

# Stuff for Graphs and Plots
library("ggplot2")
library("ggpubr")
library("gplots")
library("plotly")
library("ggrepel")
library("RColorBrewer")

##I define a personalized theme for ggplot based on a default theme
mytheme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

library("tools4uplift")

# Package for XGBoosting
library("xgboost")

# Package for HCF
library("grf")
library("psych") #for describe()

# Miscellaneous
library("matrixStats")
library("reshape2")
library("Rcpp")
library("mltools")
library("glmnet")
library("caret")
library("mlr") #for hp tuning
library('caret')
library('ROCR')
library('pROC')

## 1.0 Data Preparation ##

# Import dataset
USERS <- read.csv("users.csv")
data <- USERS %>% select(
                        -baseline_score, -treatment_score, -total_score
                        ) %>%
                        rename(y = resub, treat = treated)
data <- as.data.frame(data)

# Converting categorical variables into factors
data_factor <- data %>% mutate_at(
                            vars(u_gender, u_format_pref, u_genre_pref,
                                u_other_sub, u_occupation, u_plan),
                                funs(factor)
                            )

# We also perform one hot encoding, to be used in models which do not support factors (i.e. xgb)
data_cat <- data_factor %>% select(
                            u_gender, u_format_pref, u_genre_pref,
                            u_other_sub, u_occupation, u_plan
                            )
data_noncat <- data_factor %>% select(
                                -u_gender, -u_format_pref, -u_genre_pref,
                                -u_other_sub, -u_occupation, -u_id, -u_plan
                                )
data_oh <- one_hot(as.data.table(data_cat))
data_oh <- cbind(data_factor$u_id, data_oh, data_noncat)
colnames(data_oh)[1] <- "u_id"
data_oh$y <- as.factor(data_oh$y)

# Dividing our Datasets
split <- SplitUplift(data, 0.7, c("treat", "y"))
train <- split[[1]]
test <- split[[2]]

# Dividing train and test by treat and control
train_treat <- subset(train, treat == 1)
train_ctrl <- subset(train, treat == 0)

# Reproducing the sample split on the hot encoded dataset
split_oh <- SplitUplift(data_oh, 0.7, c("treat", "y"))
train_oh <- as.data.frame(split_oh[[1]])
test_oh <- as.data.frame(split_oh[[2]])

#Dividing treatment by treatment and control
train_oh_treat <- subset(train_oh, treat == 1)
train_oh_ctrl <- subset(train_oh, treat == 0)

# Define the set of covariates (without y and treat)
features <- colnames(train)[2:(length(colnames(train)) - 2)]
features_oh <- colnames(train_oh)[2:(length(colnames(train_oh)) - 2)]

# Comparison test df across models
df_comparison <- data.frame(test)

## 2.0 SINGLE MODEL - LOGIT ##
logitformula <- paste("y~", paste(features, collapse = "+"))

logit_model <- glm(
    formula = logitformula,
    data = train, family = binomial(link = logit)
    )

# Prediction
test_logit1 <- data.frame(test)
test_logit1$pred_prob_logit1 <- predict(
    logit_model, newdata = test, type = "response"
    )
#summary(test$pred_prob_logit1)
#plot(test$pred_prob_logit1)
#test$pred_logit1 <- ifelse(test$pred_prob_logit1 > 0.5, 1, 0)

#tau???
# perf_logit1??


## 3.1 TWO MODEL - LOGIT ##
# Fitting the model
test_logit2 <- data.frame(test)

logit_ctrl <- glm(
    formula = logitformula,
    data = train_ctrl, family = binomial(link = logit)
    )
logit_treat <- glm(
    formula = logitformula,
    data = train_treat, family = binomial(link = logit)
    )

# Prediction
test_logit2$pred_prob_C_logit2 <- predict(
    logit_ctrl, newdata = test, type = "response"
    )
test_logit2$pred_prob_T_logit2 <- predict(
    logit_treat, newdata = test, type = "response"
    )
test_logit2$tau_logit2 <- test_logit2$pred_prob_T_logit2 - test_logit2$pred_prob_C_logit2
#test$pred_logit2 <- ifelse(test$pred_prob > 0.5, 1, 0)
perf_logit2 <- PerformanceUplift(
                            data = test_logit2, treat = "treat",
                            outcome = "y", prediction = "tau_logit2",
                            equal.intervals = TRUE, nb.group = 10
                            )
# adding two model logit tau to df_comparison
df_comparison$tau_logit2 <- test_logit2$tau_logit2


## 3.2 TWO MODEL - XGBOOST ##

#Setting Up the xgboost learner
xgb_learner <- makeLearner(
                            "classif.xgboost", predict.type = "prob",
                            par.vals = list(
                                            objective = "binary:logistic",
                                            eval_metric = "error"
                                            )
                            )

#set parameter space for hyperparameter turning
xgb_params <- makeParamSet(
                            makeIntegerParam(
                                "nrounds", lower = 100, upper = 500
                                ),
                            makeIntegerParam(
                                "max_depth", lower = 1, upper = 10
                                ),
                            makeNumericParam("eta", lower = .1, upper = .5),
                            makeNumericParam(
                                "lambda", lower = -1, upper = 0,
                                trafo = function(x) 10^x
                                )
                            )
ctrl <- makeTuneControlRandom(maxit = 15)

#set resampling strategy (CV with 5 iterations)
resample_desc <- makeResampleDesc("CV", iters = 5)

#creating the model for treatment group:
task_treat <- makeClassifTask(
                            data = train_oh_treat[,
                                !(colnames(train_oh_treat) == "treat")],
                                target = "y"
                            )
tuned_params_treat <- tuneParams(
                            learner = xgb_learner, task = task_treat,
                            resampling = resample_desc,
                            par.set = xgb_params, control = ctrl
                            )
treatment_xgbmodel <- mlr::train(
                                 learner = setHyperPars(learner = xgb_learner,
                                 par.vals = tuned_params_treat$x),
                                 task = task_treat
                                 )

#saving the model
#saveRDS(treatment_xgbmodel, "xgb_treatment_model.rds")
#to load model
#treatment_xgbmodel <- readRDS("xgb_treatment_model.rds") for now this is the old model, but will be overwritten

#creating the model for control group:
task_ctrl <- makeClassifTask(
                            data = train_oh_ctrl[,
                            !(colnames(train_oh_ctrl) == "treat")], target = "y"
                            )
tuned_params_ctrl <- tuneParams(
                            learner = xgb_learner, task = task_ctrl,
                            resampling = resample_desc,
                            par.set = xgb_params, control = ctrl
                            )
control_xgbmodel <- mlr::train(
                             learner = setHyperPars(learner = xgb_learner,
                             par.vals = tuned_params_ctrl$x), task = task_ctrl
                             )

#saving the model
#saveRDS(control_xgbmodel, "xgb_control_model.rds")
#to load model
#control_xgbmodel <- readRDS("xgb_control_model.rds") for now this is the old model, but will be overwritten

#making treatment effect estimates on train and test data:
train_oh$pred_T_xgb <- predict(
                            treatment_xgbmodel, newdata = train_oh[,
                            !(colnames(train_oh) == "treat")]
                            )$data[[2]]
train_oh$pred_C_xgb <- predict(
                            control_xgbmodel, newdata = train_oh[,
                            !(colnames(train_oh) == "treat") &
                            !(colnames(train_oh) == "pred_T_xgb")]
                            )$data[[2]]
train_oh$tau_xgb <- train_oh$pred_T_xgb - train_oh$pred_C_xgb

test_oh$pred_T_xgb <- predict(
                            treatment_xgbmodel, newdata = test_oh[,
                                !(colnames(test_oh) == "treat")]
                            )$data[[2]]
test_oh$pred_C_xgb <- predict(
                            control_xgbmodel, newdata = test_oh[,
                                !(colnames(test_oh) == "treat") &
                            !(colnames(test_oh) == "pred_T_xgb")]
                            )$data[[2]]
test_oh$tau_xgb <- test_oh$pred_T_xgb - test_oh$pred_C_xgb


# performance evaluator with performance uplift
perf_xgb <- PerformanceUplift(
                            data = test_oh, treat = "treat",
                            outcome = "y", prediction = "tau_xgb",
                            equal.intervals = TRUE, nb.group = 10
                            )

# adding two model xgb tau to df_comparison
df_comparison$tau_xgb <- test_oh$tau_xgb

## 4.1 HONEST CAUSAL FOREST ##
cf <- causal_forest(
                    X = as.matrix(train[, !(colnames(train) == "treat") &
                                  !(colnames(train) == "y")]),
                    Y = train$y,
                    W = train$treat,
                    honesty = TRUE,
                    honesty.fraction = c(0.3, 0.4, 0.5, 0.6, 0.7),
                    alpha = c(0.01, 0.05, 0.1, 0.15, 0.2),
                    imbalance.penalty = c(0, 0.5, 1, 1.5),
                    num.trees = dim(USERS)[1] / 5,
                    tune.parameters = "all"
                    )

#saveRDS(cf, "hcf_model.rds")
#cf <- readRDS("hcf_model.rds")

# On test set
test_hcf <- data.frame(test)

test_pred_hcf <- predict(cf, newdata = as.matrix(test_hcf[,
                                            !(colnames(test_hcf) == "treat") &
                                            !(colnames(test_hcf) == "y")]),
                                            estimate.variance = TRUE)
tauhat_hcf_test <- test_pred_hcf$predictions
tauhat_hcf_test_se <- sqrt(test_pred_hcf$variance.estimates)
test_hcf$tau_hcf <- tauhat_hcf_test

# Variable importance
var_imp <- c(variable_importance(cf))
names(var_imp) <- c("u_gender", "u_age", "weekly_utilisation",
                    "u_sub_utilisation", "u_format_pref", "u_genre_pref",
                    "u_rating_given", "u_other_sub", "u_occupation", "u_plan")
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp <- as.data.frame(sorted_var_imp)


# Model evaluation (TO BE DISCUSSED: DELETE?)
# as we don't observe the true counterfactual, we will rely on the transformed outcome
# https://gsbdbi.github.io/ml_tutorial/hte_tutorial/hte_tutorial.html#introduction
p <- mean(test_hcf$treat)
y_star <- ((test_hcf$treat - p) / (p * (1 - p))) * test$y

# Compute the sample average treatment effect to use as a baseline comparison
tauhat_sample_ate <- with(train, mean(y[treat == 1]) - mean(y[treat == 0]))

# Compute test mse for all methods
mse <- data.frame(
    Sample_ATE_Loss = (y_star - tauhat_sample_ate)^2,
    Causal_Forest_Loss = (y_star - tauhat_cf_test)^2)

mse_summary <- describe(mse)

# Confidence Intervals
plot_htes <- function(hcf_preds, ci = FALSE, z = 1.96) {
  if (is.null(hcf_preds$predictions) || NROW(hcf_preds$predictions) == 0)
    stop("hcf_preds must include a matrix called 'predictions'")
  #check if the treatment effects are heterogeneous with rank
  out <- ggplot(
    mapping = aes(
      x = rank(hcf_preds$predictions),
      y = hcf_preds$predictions
    )
  ) +
    geom_point() +
    labs(x = "Rank of Estimated Treatment Effect",
    y = "Estimated Treatment Effect") +
    theme_light()
  if (ci && NROW(hcf_preds$variance.estimates) > 0) {
    out <- out +
      geom_errorbar(
        mapping = aes(
          ymin = hcf_preds$predictions + z * sqrt(hcf_preds$variance.estimates),
          ymax = hcf_preds$predictions - z * sqrt(hcf_preds$variance.estimates)
        ), size = 0.05
      )
  }
  return(out)
}
plot_htes(test_pred_hcf, ci = TRUE)

perf_hcf <- PerformanceUplift(
                            data = test_hcf, treat = "treat",
                            outcome = "y", prediction = "tau_hcf",
                            equal.intervals = TRUE
                            )

# adding hcf tau to df_comparison
df_comparison$tau_hcf <- test_hcf$tau_hcf

## 5.0 PERFORMANCE EVALUATION ##
## To do: write performance evaluation scripts and import them here