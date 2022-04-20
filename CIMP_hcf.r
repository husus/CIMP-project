rm(list = ls())
options(scipen = 999)
set.seed(10)

# Packages for Data Manipulation
library("dplyr")
library("data.table")

# Stuff for Graphs and Plots
library("ggplot2")

library("tools4uplift")

# Package for HCF
library("grf")
library("psych") #for describe()

mytheme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# # Miscellaneous
# library("matrixStats")
# library("reshape2")
# library("Rcpp")
# library("mltools")
# library("glmnet")
# library("caret")
# library("mlr") #for hp tuning
# library("rpart")       # Classification and regression trees, or CART (4.1-13)
# library("rpart.plot")  # Plotting trees (3.0.6)

# total number of users under study - Increased from 1000 to 100000 (experiment)
num_users <- 1000

u_id <- seq(1, num_users)

u_age <- sample(
                c(1, 2, 3, 4, 5, 6), num_users, replace = T,
                prob = c(0.1, 0.30, 0.35, 0.15, 0.05, 0.05)
                )
# 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

u_gender <- sample(c(1, 2), num_users, replace = T,
                    prob = c(0.6, 0.4))
# 1=M, 2=F

u_weekly_utilisation <- sample(0:7, num_users, replace = T)
# number of days using the service in a week

u_sub_utilisation <- round(runif(num_users, 0, 1), 2)
# proportion of time spent on the service since the first subscription

#u_rating_given <- sample(0:5,num_users,replace=T)
u_rating_given <- round(runif(num_users, 0, 5), 2)

# rating on a scale from 0 to 5 given by each user to the platform

u_format_pref <- sample(
                        c(1, 2, 3), num_users, replace = T,
                        prob = c(0.5, 0.4, 0.1)
                        )
# 1=TV-series, 2=movies, 3=documentaries

u_genre_pref <- sample(1:7, num_users, replace = T)
# 1=action, 2=comedy, 3=romance, 4=sci-fi, 5=animation, 6=drama, 7=horror

u_other_sub <- sample(0:1, num_users, replace = T)
# binary variable where 0=not subscribed to other streaming platforms, 1=yes

u_plan <- sample(1:2, num_users, replace = T)
# type of subscription plan: 1=individual 2=family

# creating the data table with all the users
USERS <- data.table(u_id, u_gender, u_age, u_weekly_utilisation,
                    u_sub_utilisation, u_format_pref,
                    u_genre_pref, u_rating_given, u_other_sub, u_plan)


#USERS

# defining the users' occupation variable based on some conditions
USERS$u_occupation[u_age==1] <- 1
USERS$u_occupation[u_age==2|u_age==3] <- sample(c(2,3,4),nrow(USERS[u_age==2|u_age==3]),replace=T,prob=c(0.4,0.4,0.2))
USERS$u_occupation[u_age==4|u_age==5] <- sample(c(2,3,4),nrow(USERS[u_age==4|u_age==5]),replace=T,prob=c(0.1,0.7,0.2))
USERS$u_occupation[u_age==6] <- sample(c(3,5),nrow(USERS[u_age==6]),replace=T,prob=c(0.3,0.7))
# occupation: 1=student 2=part-time 3=full-time 4=unemployed 5=retired
# age: 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

# we suppose that our streaming service is focused on action and sci-fi tv-series 
# u_genre_pref(1=action||4=sci-fi), u_format_pref(1=series), u_age(-), u_occupation(-)
# u_other_sub(-), u_rating_given(+), u_sub_utilisation(+), u_weekly_utilisation(+)
# add error term from rnorm

# score = u_genre_pref(1|4) 80 + u_format_pref(1) 100 + u_age(1|2) 30 - u_age(3|4|5|6) 30 
#         + u_occupation(1|4|5) 30 - u_occupation(2|3) 30 - u_other_sub*55 + u_rating_given*50
#         + u_sub_utilisation*130 + u_weekly_utilisation*115 + error 
# NB: coefficients arbitrarly given
# USERS[,score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55+rnorm(1)*100]

# Deterministic Portion of Utility
USERS[,baseline_score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55]
USERS[u_genre_pref==1|u_genre_pref==4, baseline_score:=baseline_score+80]
USERS[u_format_pref==1, baseline_score:=baseline_score+100]
USERS[,baseline_score:=ifelse(u_age==1|u_age==2,baseline_score+30,baseline_score-30)] 
USERS[,baseline_score:=ifelse(u_occupation==2|u_occupation==3,baseline_score-30,baseline_score+30)] 
USERS[u_plan==2, baseline_score := baseline_score+50]

# Random Component of Utility (observable to customers but unobservable to the econometrician)
USERS$baseline_score <- USERS$baseline_score + rnorm(1, 0, 70)

# NOTE: Remember to set appropriate size for the noise

# Creating Treatment Effects

# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1, num_users, replace = T)

# The impact of our policy can be divided into two components:
# First: an additive component independent of covariates and positive on average
USERS[, treatment_score := ifelse(treated == 1, rnorm(num_users, 40, 20), 0)]
# NOTE: Remember to set appropriate size for the random component


# Second: part of the effect depends on some user's characteristics (interactions).
# For example, #to capture the higher price sensitivity of young people and students/unemployed:
USERS[treated==1,treatment_score := ifelse(u_age==1|u_age==2,treatment_score+70,treatment_score)]
USERS[treated==1,treatment_score:=ifelse(u_occupation==2|u_occupation==3|u_occupation==5,treatment_score,treatment_score+100)] 
# As well as the price sensitivity of people subscribed to individual plans
USERS[treated==1, treatment_score:=ifelse(u_plan==1,treatment_score+70, treatment_score)]
# We may assume we face different degrees of competition depending on the favorite genre of users: 
USERS[treated == 1,treatment_score := ifelse(u_genre_pref == 2 | u_genre_pref == 3,treatment_score, treatment_score + 50)] 
# Finally, a voucher would reduce multihoming costs of being subscribed to multiple platforms
USERS[u_other_sub == 1 & treated == 1, treatment_score := treatment_score + 60]


# Unifying baseline and treatment  scores
USERS$total_score <- USERS$baseline_score + USERS$treatment_score

#How to assign churn?
#Assume that 15% of customer churn
threshold_churn <- quantile(USERS$baseline_score, prob = c(.15))
USERS[, resub := ifelse(total_score > threshold_churn, 1, 0)]
summary(USERS$resub)

# Adding additional noise by allowing an erratic behavior of 5% of customer
perc_err <- num_users * 0.05
USERS[sample(USERS$u_id, perc_err), resub := ifelse(resub == 0, 1, 0)]

data <- USERS %>% select(
                        -baseline_score, -treatment_score,
                        -total_score, -u_id
                        ) %>%
                        rename(y = resub, treat = treated)
data <- as.data.frame(data)

# Dividing our Dataset (technically train-test-validation but yeah no)
split <- SplitUplift(data, 0.7, c("treat", "y"))
train <- split[[1]]
test <- split[[2]]

#CAUSAL FOREST
cf <- causal_forest(
                    X = as.matrix(train[, !(colnames(train) == "treat") &
                                  !(colnames(train) == "y")]),
                    Y = train$y,
                    W = train$treat,
                    num.trees = num_users / 10
                    )

#saveRDS(cf, "hcf_model.rds")
#cf <- readRDS("hcf_model.rds")

# #prediction with out of bag
# oob_pred <- predict(cf, estimate.variance = TRUE)

# oob_tauhat_cf <- oob_pred$predictions
# oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)

# #write.csv(oob_pred, "oob_pred.csv")

# #heterogeneity
# hist(oob_tauhat_cf, main = "Causal forests: out-of-bag CATE")

#on test set
test_pred <- predict(cf, newdata = as.matrix(test[,
                                                !(colnames(test) == "treat") &
                                                !(colnames(test) == "y")]),
                                                estimate.variance = TRUE)
tauhat_cf_test <- test_pred$predictions
tauhat_cf_test_se <- sqrt(test_pred$variance.estimates)

test$tau_hcf <- tauhat_cf_test

# Variable importance
var_imp <- c(variable_importance(cf))
names(var_imp) <- c("u_gender", "u_age", "weekly_utilisation",
                    "u_sub_utilisation", "u_format_pref", "u_genre_pref",
                    "u_rating_given", "u_other_sub", "u_occupation", "u_plan")
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp <- as.data.frame(sorted_var_imp)


# Model evaluation
# as we don't observe the true counterfactual, we will rely on the transformed outcome
# https://gsbdbi.github.io/ml_tutorial/hte_tutorial/hte_tutorial.html#introduction
p <- mean(test$treat)
y_star <- ((test$treat - p) / (p * (1 - p))) * test$y

# Compute the sample average treatment effect to use as a baseline comparison
tauhat_sample_ate <- with(train, mean(y[treat == 1]) - mean(y[treat == 0]))

# Compute test mse for all methods
mse <- data.frame(
    Sample_ATE_Loss = (y_star - tauhat_sample_ate)^2,
    Causal_Forest_Loss = (y_star - tauhat_cf_test)^2)

mse_summary <- describe(mse)

# Confidence Intervals
plot_htes <- function(cf_preds, ci = FALSE, z = 1.96) {
  if (is.null(cf_preds$predictions) || NROW(cf_preds$predictions) == 0)
    stop("cf_preds must include a matrix called 'predictions'")
  out <- ggplot(
    mapping = aes(
      x = rank(cf_preds$predictions),
      y = cf_preds$predictions
    )
  ) +
    geom_point() +
    labs(x = "Rank", y = "Estimated Treatment Effect") +
    theme_light()
  if (ci && NROW(cf_preds$variance.estimates) > 0) {
    out <- out +
      geom_errorbar(
        mapping = aes(
          ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
          ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates)
        ), size = 0.1
      )
  }
  return(out)
}
plot_htes(test_pred, ci = TRUE)

perf_hcf <- PerformanceUplift(
                            data = test, treat = "treat",
                            outcome = "y", prediction = "tau_hcf",
                            equal.intervals = TRUE
                            )

#Plotting Qini curve and Qini Coeff on the test set
QiniPlot <- function (performance, modeltype) {
  
  #Plot qini curves (abs and %) starting from performance obejcts
  # of Tools4Uplift Package
  # Args
  # performance: performance object for estimating Qini Curves
  # modeltype: model type for filling up the title
  
  df <- data.frame(matrix(nrow = 10, ncol = 3))
  df[, 1] <- performance[[1]]
  df[, 2] <- round(performance[[6]], 2)
  df[, 3] <- round(performance[[7]], 2)
  colnames(df) <- c("Dec", "num.incr", "perc.incr")
  firstrow <- numeric(3)
  df <- rbind(firstrow, df)

  # Plot Qini curves
  qini_curve_1 <- ggplot(df,
                        aes(x = Dec, y = num.incr)
                        ) + geom_point(color = "blue") +
                        geom_line(color = "blue") +
                        mytheme + labs(title = sprintf("Qini Curve (abs) - %s",
                                        modeltype),
                                        y = "Incr. Numb. of Resub. Cust.",
                                        x = "Perc. of Customers Targeted") +
                                        scale_x_continuous(
                                                    breaks = seq(0, 1, 0.1)) +
                                                    geom_segment(x = 0, y = 0,
                                                    xend = 1, yend = df[11, 2],
                                                    color = "red",
                                                    linetype = "dashed",
                                                    size = 0.5)
  qini_curve_2 <- ggplot(df,
                        aes(x = Dec, y = perc.incr)
                        ) + geom_point(color = "blue") +
                        geom_line(color = "blue") +
                        mytheme + labs(
                        title = sprintf("Qini Curve (perc.) - %s", modeltype),
                        y = "Incr. % of Resub. Cust.",
                        x = "Perc. of Customers Targeted") + xlim(0, 1) +
                        geom_segment(x = 0, y = 0, xend = 1, yend = df[11, 3],
                        color = "red", linetype = "dashed", size = 0.5)
  plot_list <- list(qini_curve_1, qini_curve_2)
  return(plot_list)
}

plot_list <- QiniPlot(performance = perf_hcf, modeltype = "HCF")

plot_list[[1]]
plot_list[[2]]

# Qini coefficient of xgb
Q <- QiniArea(perf_hcf)