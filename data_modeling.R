rm(list = ls())
options(scipen = 999)
set.seed(10)

# Packages for Data Manipulation
library("dplyr")
library("tidyverse")
library("data.table")

# Packages for Graphs and Plots
library("ggplot2")
library("ggpubr")
library("gplots")
library("plotly")
library("ggrepel")
library("RColorBrewer")
# Defining a personalized theme for ggplot based on a default theme
mytheme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Package for Models and Various Utils 
library("tools4uplift")
library("xgboost")
library("grf")
library("psych") #for describe()
library("matrixStats")
library("reshape2")
library("Rcpp")
library("mltools")
library("glmnet")
library("caret")
library("mlr") #for hp tuning
library('caret')

# Importing Personal functions
source('./functions.R')


### 1.0 Data Preparation ###

# Importing dataset
data <- read.csv("./users.csv") #relative path

data_no_factor <- data

# Converting categorical variables into factors
data <- data %>% mutate_at(
            vars(u_gender, u_age, u_format_pref, u_genre_pref,
                 u_other_sub, u_occupation, u_plan),
            funs(factor)
        )


# Perform one hot encoding to be used in models which do not support factors (i.e. xgb)
data_cat <- data %>% select(
                            u_gender, u_age, u_format_pref, u_genre_pref,
                            u_other_sub, u_occupation, u_plan)

data_noncat <- data %>% select(-u_gender, -u_age, -u_format_pref, -u_genre_pref,
                                -u_other_sub, -u_occupation, -u_id, -u_plan)


dummy <- dummyVars(" ~ .", data=data_cat)
newdata <- data.frame(predict(dummy, newdata = data_cat))

# Modifying names of one hot encoded vars (needed for later stages)
for (k in 1:ncol(newdata)) {
  splitted_str=strsplit(colnames(newdata)[k], ".", fixed = TRUE)
  colnames(newdata)[k]=paste(splitted_str[[1]][1],splitted_str[[1]][2], sep = "", collapse = NULL)
}

data_oh <- cbind(data$u_id, newdata, data_noncat)
colnames(data_oh)[1] <- 'u_id'
data_oh$y <- as.factor(data_oh$y)

# Splitting the datasets in train-test
set.seed(10)
split <- SplitUplift(data, 0.7, c("treat", "y"))
train <- split[[1]]
test <- split[[2]]

set.seed(10)
split_oh <- SplitUplift(data_oh, 0.7, c("treat", "y"))
train_oh <- as.data.frame(split_oh[[1]])
test_oh <- as.data.frame(split_oh[[2]])

# Defining the set of covariates (without y and treat)
features <- colnames(train)[2:(length(colnames(train)) - 2)]
features_oh <- colnames(train_oh)[2:(length(colnames(train_oh)) - 2)]

# Comparison df across models for final testing
df_comparison <- data.frame(test)



#### 2. TRADITIONAL A/B TESTING ####

basic_model_ab <- lm(y ~ treat, data = data)
summary(basic_model_ab)

# Even when adding all the regressors
formula_abtest <- as.formula(paste("y~ treat+", paste(features,collapse="+")))
adv_model_ab <- lm(formula_abtest, data = data)
summary(adv_model_ab)


#### 3. THE TWO MODELS APPROACH ####

# Dividing train sets by treat and control
train_treat <- subset(train, treat == 1)
train_ctrl <- subset(train, treat == 0)

train_oh_treat <- subset(train_oh, treat == 1)
train_oh_ctrl <- subset(train_oh, treat == 0)


#### 3.1 TWO MODEL - LOGIT ####

logitformula <- as.formula(paste("y~", paste(features, collapse = "+")))


# Training the first model on control units in the train set
logit_ctrl <- glm(
    formula = logitformula,
    data = train_ctrl, family = binomial(link = logit))

# Training the second model on treated units in the train set
logit_treat <- glm(
    formula = logitformula,
    data = train_treat, family = binomial(link = logit))

# Extract for each customer the predicted prob of resub from both models
pred_prob_C_logit2 <- predict(logit_ctrl, newdata = test, type = "response")
pred_prob_T_logit2 <- predict(logit_treat, newdata = test, type = "response")

# Adding the tau from the two-model logit to the comparison df
df_comparison$tau_logit2 <- pred_prob_T_logit2 - pred_prob_C_logit2


# Evaluating the performance of the model
perf_logit2 <- PerformanceUplift(
                            data = df_comparison, treat = "treat",
                            outcome = "y", prediction = "tau_logit2",
                            equal.intervals = TRUE, nb.group = 10, rank.precision = 2
                        )



#### 3.2 TWO MODEL - XGBOOST ####

# Setting up the xgboost learner
xgb_learner <- makeLearner("classif.xgboost", predict.type = "prob",
                            par.vals = list(objective = "binary:logistic",
                                            eval_metric = "error")
                             )

# Setting parameter space for hyperparameter turning
xgb_params <- makeParamSet(
                            makeIntegerParam("nrounds", lower = 100, upper = 500),
                            makeIntegerParam("max_depth", lower = 1, upper = 10),
                            makeNumericParam("eta", lower = .1, upper = .5),
                            makeNumericParam("lambda", lower = -1, upper = 0,
                                             trafo = function(x) 10^x)
                        )
ctrl <- makeTuneControlRandom(maxit = 15)

# Setting resampling strategy (CV with 5 iterations)
resample_desc <- makeResampleDesc("CV", iters = 5)

# Creating the model for treatment group:
task_treat <- makeClassifTask(
                      data = train_oh_treat[,!(colnames(train_oh_treat) == "treat")],
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


# Creating the model for control group:
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

pred_T_xgb <- predict(treatment_xgbmodel, 
                      newdata = test_oh[,!(colnames(test_oh) == "treat")])$data[[2]]
pred_C_xgb <- predict(control_xgbmodel,
                      newdata = test_oh[,!(colnames(test_oh) == "treat") & 
                                          !(colnames(test_oh) == "pred_T_xgb")])$data[[2]]

# Adding two model xgb tau to df_comparison
df_comparison$tau_xgb <- pred_T_xgb -pred_C_xgb


# Performance evaluation with performance uplift
perf_xgb <- PerformanceUplift(
                            data = df_comparison, treat = "treat",
                            outcome = "y", prediction = "tau_xgb",
                            equal.intervals = TRUE, nb.group = 10,
                            rank.precision = 2
                            )

#### 4. SINGLE MODEL WITH INTERACTIONS - LOGISTIC REGRESSION ####

## Baseline Model ####

# Defining the formula for the logit model
outcome <- 'y'
treat <- 'treat'

treat_formula <- c()
for (k in seq(1:length(features))) {
  treat_formula <- paste(treat_formula, paste(features[k], treat, sep = ":"), sep="+")
}

baseline_formula <- as.formula(paste(paste(outcome, "~", treat, "+"),paste(features,collapse="+"),treat_formula))

# Creating a dataframe for comparisons
comparison_interlogit <- test


# Estimating the baseline model, i.e. a logit model with all the interaction
# terms between the treatment and the features

interlogit_baseline_output <- InterModel.logit(baseline_formula, train, 
                                               test, treat='treat', outcome='y') #personal function

interlogit_baseline_model <- interlogit_baseline_output[[1]]
print(interlogit_baseline_model)
summary(interlogit_baseline_model)

# Adding the estimated tau to df_comparison
comparison_interlogit$tau_interlogit_baseline <- interlogit_baseline_output[[2]]

##-- Model Optimization ####

# Using revisited functions from package tools4uplift 
# for extracting a set of best features

my_path <- LassoPath(train, baseline_formula)

best_feat_output <- BestFeatures_mod(train_oh, test_oh, treat='treat', outcome='y', predictors=features_oh, rank.precision = 2, path=my_path,
                                  equal.intervals = TRUE, nb.group = 10) #personal function

best_features <- best_feat_output[[1]]
best_lambda <- best_feat_output[[2]]


# Using the best features for writing the formula for our final model
best_feat_formula <- as.formula(paste("y~", paste(best_features, collapse="+")))

interlogit_opt_output <- InterModel.logit(best_feat_formula, train_oh, test_oh, treat='treat', outcome='y', lambda = best_lambda)

interlogit_opt_model <- interlogit_opt_output[[1]]
interlogit_opt_model_penalized <- interlogit_opt_output[[3]]

summary(interlogit_opt_model)


comparison_interlogit$tau_interlogit_opt <- interlogit_opt_output[[2]]
comparison_interlogit$tau_interlogit_opt_penalized <- interlogit_opt_output[[4]]



# Checking which is the best performing single logit model

perf_intermodel_baseline <- PerformanceUplift(data = comparison_interlogit, treat = "treat",
                                              outcome = "y", prediction = "tau_interlogit_baseline", equal.intervals = TRUE, nb.group = 10,
                                              rank.precision = 2)
perf_intermodel_opt <- PerformanceUplift(data = comparison_interlogit, treat = "treat",
                                              outcome = "y", prediction = "tau_interlogit_opt", equal.intervals = TRUE, nb.group = 10,
                                              rank.precision = 2)
perf_intermodel_opt_penalized <- PerformanceUplift(data = comparison_interlogit, treat = "treat",
                                              outcome = "y", prediction = "tau_interlogit_opt_penalized", equal.intervals = TRUE, nb.group = 10,
                                              rank.precision = 2)

perf_list <- list(perf_intermodel_baseline, perf_intermodel_opt, perf_intermodel_opt_penalized)

for(perf in perf_list){
  print(QiniArea(perf))}


# Adding the best performing model to df for final comparison, i.e. baseline interlogit
df_comparison$tau_interlogit <- comparison_interlogit$tau_interlogit_baseline


#### 5. HONEST CAUSAL FOREST (HCF) ####

data_no_factor <- data_no_factor %>% select ( -u_id)

set.seed(10)
split_no_factor <- SplitUplift(data_no_factor, 0.7, c("treat", "y"))
train_no_factor <- split_no_factor[[1]]
test_no_factor <- split_no_factor[[2]]


cf <- causal_forest(
                    X = as.matrix(train_no_factor[, !(colnames(train_no_factor) == "treat") &
                                  !(colnames(train_no_factor) == "y")]),
                    Y = train_no_factor$y,
                    W = train_no_factor$treat,
                    honesty = TRUE,
                    honesty.fraction = c(0.3, 0.4, 0.5, 0.6, 0.7),
                    alpha = c(0.01, 0.05, 0.1, 0.15, 0.2),
                    imbalance.penalty = c(0, 0.5, 1, 1.5),
                    num.trees = dim(data_no_factor)[1] / 5,
                    tune.parameters = "all"
                    )

#saveRDS(cf, "hcf_model.rds")
#cf <- readRDS("hcf_model.rds")

test_no_factor_pred <- predict(cf, newdata = as.matrix(test_no_factor[,
                                            !(colnames(test_no_factor) == "treat") &
                                            !(colnames(test_no_factor) == "y")]),
                                            estimate.variance = TRUE)

tauhat_hcf_test_no_factor <- test_no_factor_pred$predictions
tauhat_hcf_test_no_factor_se <- sqrt(test_no_factor_pred$variance.estimates)

# Adding hcf tau to df_comparison
df_comparison$tau_hcf <- tauhat_hcf_test_no_factor

# Variable importance
var_imp <- c(variable_importance(cf))
names(var_imp) <- c("u_gender", "u_age", "weekly_utilisation",
                    "u_sub_utilisation", "u_format_pref", "u_genre_pref",
                    "u_rating_given", "u_other_sub", "u_occupation", "u_plan")
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp <- as.data.frame(sorted_var_imp)


# Confidence Intervals plot
plot_htes(test_no_factor_pred, ci = TRUE) #personal function



perf_hcf <- PerformanceUplift( data = df_comparison , treat = "treat",
                                outcome = "y", prediction = "tau_hcf",
                                equal.intervals = TRUE
                        )


#### 6.0 PERFORMANCE EVALUATION ####
# Here we select the best performing model on the basis of the Qini coefficient
# i.e. the area under the Qini Curve

perf_list <- list(perf_xgb, perf_intermodel_baseline, perf_hcf)

for(perf in perf_list){
  print(QiniArea(perf))}

# Plotting Qini Curves
qini_plots <- MultiQiniPlots(perf_list, names=c('XGB', 'InterLogit', 'HCF')) #personal function
qini_plots[[1]]
qini_plots[[2]]


# Plotting Uplift as a function of the percentage of user base targeted
uplift_plot <- MovingDifference(perf_list, names=c('XGB', 'InterLogit', 'HCF')) #personal function
uplift_plot


# Saving the comparison df as .csv 
write.csv(df_comparison, "comparison_data.csv", row.names = FALSE)
