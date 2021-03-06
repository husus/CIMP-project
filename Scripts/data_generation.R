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

source('./functions.R')


# total number of users under study
num_users <- 10000


# Users ID
u_id <- seq(1, num_users)

# Age of users: 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55
u_age <- sample(
            c(1, 2, 3, 4, 5, 6), 5, replace=T,
            prob = c(0.1, 0.30, 0.35, 0.15, 0.05, 0.05)
          )

# Gender of users: 1=male, 2=female, 3=non-binary
u_gender <- sample( c(1, 2, 3), num_users, replace=T, prob = c(0.55, 0.40, 0.05) )


# Variable for the average number of days in a week (0-7) using the platform 
# Sampling from a truncated normal distribution (NB: function created in './functions.R')
u_weekly_utilisation <- rtnorm(num_users, 2.5, 0.8, 0, 7)
# hist(u_weekly_utilisation)


# Variable expressed in proportion accounting for the time spent on the platform since the 1st subscription 
# Sampling from a truncated normal distribution
u_sub_utilisation <- rtnorm(num_users, 0.7, 0.25, 0.001, 1)
# hist(u_sub_utilisation)


# Variable measuring the average rating given to shows on our platform on a scale from 1 (lowest) to 5 (highest) 
# Sampling from a truncated normal distribution
u_rating_given <- rtnorm(num_users, 4.2, 1, 1, 5)
# hist(u_rating_given)


# Variable for the preferred content format: 1=TV-series, 2=movies, 3=documentaries
u_format_pref <- sample(
                    c(1, 2, 3), num_users, replace = T,
                    prob = c(0.5, 0.4, 0.1)
                  )

# Variable for the preferred genre: 1=action, 2=comedy, 3=romance, 4=sci-fi, 5=animation, 6=drama, 7=horror
u_genre_pref <- sample(1:7, num_users, replace = T)


# Binary variable for other subscriptions: 0=not subscribed to other streaming platforms, 1=yes
u_other_sub <- sample(0:1, num_users, replace = T, prob=c(0.7,0.3))


# Binary variable for the type of subscription plan: 0=individual 1=family
u_plan <- sample(0:1, num_users, replace = T, prob=c(0.6,0.4))


# Standardizing numerical variables
u_rating_given_std = as.vector(scale(u_rating_given, center=TRUE, scale=TRUE))
u_sub_utilisation_std = as.vector(scale(u_sub_utilisation, center=TRUE, scale=TRUE))
u_weekly_utilisation_std = as.vector(scale(u_weekly_utilisation, center=TRUE, scale=TRUE))



# Creating the data table with all the users
USERS <- data.table(u_id, u_gender, u_age, u_format_pref, u_genre_pref, u_plan,
                    u_rating_given, u_sub_utilisation,u_weekly_utilisation, u_other_sub,
                    u_rating_given_std, u_sub_utilisation_std, u_weekly_utilisation_std)

# Defining users' occupation variable based on some conditions related to their age
# 1=student, 2=part-time, 3=full-time, 4=unemployed, 5=retired
USERS$u_occupation <- NA
USERS$u_occupation[u_age==1] <- sample(c(1,2,3), nrow(USERS[u_age==1]),replace=T,prob=c(0.9,0.07,0.03))
USERS$u_occupation[u_age==2|u_age==3] <- sample(c(2,3,4),nrow(USERS[u_age==2|u_age==3]),replace=T,prob=c(0.4,0.4,0.2))
USERS$u_occupation[u_age==4|u_age==5] <- sample(c(2,3,4),nrow(USERS[u_age==4|u_age==5]),replace=T,prob=c(0.1,0.7,0.2))
USERS$u_occupation[u_age==6] <- sample(c(3,5),nrow(USERS[u_age==6]),replace=T,prob=c(0.3,0.7))




#### DERIVING THE UTILITY SCORE ####
# In this section, we define the utility score that will be used to determine
# who is going to churn among the currently active users.
# The utility score is the result of two macro components:
# 1) A baseline component, which does not depend on the treatment and incorporates both
# a deterministic component (a function of some individual characteristics) and a stochastic one,
# which depends on unobservable factors to the analyst (but observable to the customers). 

# Deriving the Deterministic portion of Utility
USERS[,baseline_score:=u_rating_given_std*1+u_sub_utilisation_std*8+u_weekly_utilisation_std*4]
USERS[,baseline_score:=baseline_score-u_other_sub*1+u_plan*3]
USERS[u_genre_pref==1|u_genre_pref==4, baseline_score:=baseline_score+4]
USERS[u_format_pref==1, baseline_score:=baseline_score+1.5]
USERS[u_age==1|u_age==2,baseline_score:=baseline_score+4] 
USERS[u_occupation==2|u_occupation==3, baseline_score:=baseline_score-3.3]

hist(USERS$baseline_score)
summary(USERS$baseline_score)

# Factorization of categorical features
USERS1 = USERS %>% mutate_at(
                    vars(u_gender, u_age, u_format_pref, u_genre_pref, u_other_sub, u_occupation, u_plan),
                    funs(factor)
                  )

# Fitting a linear regression to check that the variables can wholly explain the baseline score
formula_test = formula("baseline_score ~ u_gender  + u_age + u_weekly_utilisation + u_sub_utilisation +
                  u_format_pref + u_genre_pref + u_rating_given + u_other_sub + u_plan + u_occupation")
testmodel = lm(formula_test, data = USERS1)

summary(testmodel) #R-squared=1
# sd(USERS$baseline_score) #measuring the standard deviation


# Adding the Random component (noise) to the Utility so to lower the R-squared value
USERS$baseline_score_noise <- USERS$baseline_score + rnorm(num_users, 0, 6.5)
# hist(USERS$baseline_score)
# summary(USERS$baseline_score_noise)

# Checking the value of R-squared by fitting in a linear regression
USERS1$baseline_score_noise <- USERS$baseline_score_noise
# hist(USERS$baseline_score)
# summary(USERS1$baseline_score_noise)

formula_test_noise = as.formula("baseline_score_noise ~ u_gender  + u_age + u_weekly_utilisation + u_sub_utilisation +
                  u_format_pref + u_genre_pref + u_rating_given + u_other_sub + u_plan + u_occupation")

testmodel_noise = lm(formula_test_noise, data = USERS1)
summary(testmodel_noise) #R-squared around 0.70




# 2) A treatment score trying to capture the treatment effect. 
# As a matter of fact, it is reasonable to assume that receiving a discount voucher would have a positive impact on utility.
# We also assume that not receiving the treatment has no effect on utility (there is no spillovers, John Hnery effect, etc)
# This second component is broken down into deterministic (depending on consumers characteristics,
# which will be source of treatment effect heterogeneity in our analysis)
# and stochastic components as well.

# First of all, we randomly assign treatment to 50% of the total users
USERS$treated <- sample(0:1, num_users, replace = T)

# Initializing treatment score. It'd be 0 for the control group
USERS$treatment_score = 0

# Then, we assign the deterministic part of the treatment effect 
# For example, to capture the higher price sensitivity of young people and students/unemployed:
USERS[treated==1,treatment_score := ifelse(u_age==1|u_age==2, treatment_score+1, treatment_score)]
USERS[(treated==1&u_occupation==1)|(treated==1&u_occupation==4), treatment_score:=treatment_score+1]
# As well as the price sensitivity of people subscribed to individual plans
USERS[treated==1, treatment_score:=ifelse(u_plan==0, treatment_score+3.5, treatment_score)]
# We may assume we face different degrees of competition depending on the favorite genre of users: 
USERS[treated == 1,treatment_score := ifelse(u_genre_pref == 2 | u_genre_pref == 3, treatment_score, treatment_score+2)] 
# Finally, a voucher would reduce multi-homing costs of being subscribed to multiple platforms
USERS[u_other_sub == 1 & treated == 1, treatment_score := treatment_score + 3.5]



# Checking here as well that by fitting a linear regression we would get R-squared=1
USERS1$treatment_score = USERS$treatment_score
USERS1$treated = USERS$treated
USERS_TREATED =subset(USERS1, treated==1)
# summary(USERS_TREATED$treatment_score)
# hist(USERS_TREATED$treatment_score)

formula_treat_test = as.formula("treatment_score ~u_age + u_genre_pref + u_other_sub + u_plan + u_occupation")
testmodel_treat = lm(formula_treat_test, data = USERS_TREATED)
summary(testmodel_treat) #R-squared=1


# Assigning the Random component
# On average the TE is assumed to be positive
# The average noise is set to around 40% of the median of the baseline utility score
USERS[, treatment_score_noise := ifelse(treated == 1, treatment_score+rtnorm(num_users, min=0, max=Inf, mean=0, sd=2.5), 0)]

# hist(USERS$treatment_score)
# summary(USERS$treatment_score_noise)

USERS1$treatment_score_noise = USERS$treatment_score_noise 
USERS_TREATED =subset(USERS1, treated==1)
# summary(USERS_TREATED$treatment_score_noise)
# hist(USERS_TREATED$treatment_score_noise)

formula_test_noise = as.formula("treatment_score_noise ~ u_gender  + u_age + u_weekly_utilisation + 
                  u_sub_utilisation + u_format_pref + u_genre_pref + u_rating_given + u_other_sub + u_plan + u_occupation")

testmodel_noise = lm(formula_test_noise, data = USERS_TREATED)
summary(testmodel_noise)



# Combining the baseline and the treatment scores 
USERS$total_score <- USERS$baseline_score_noise + USERS$treatment_score_noise

summary(USERS$total_score)
hist(USERS$total_score)


# How to assign churn? Assume that 15% of customer churn and
# we set a churn threshold equal to the 15th percentile of the baseline score with noise incorporated
# Every user with a total score below that threshold will churn
threshold_churn <- quantile(USERS$baseline_score_noise, prob = c(.15))
USERS[, resub := ifelse(total_score > threshold_churn, 1, 0)] #our y variable
# summary(USERS$resub)

# Adding additional noise by allowing an erratic behavior of 5% of customer
# perc_err <- num_users * 0.05
# USERS[sample(USERS$u_id, perc_err), resub := ifelse(resub == 0, 1, 0)]


## Final check
USERS1$resub <- USERS$resub

final_test_formula = as.formula("resub ~ u_gender + treated + u_age + u_weekly_utilisation + 
                  u_sub_utilisation + u_format_pref + u_genre_pref + u_rating_given + 
                  u_other_sub + u_plan + u_occupation + treated*u_age + treated*u_genre_pref + 
                  treated*u_plan + treated*u_other_sub + treated*u_occupation")

final_test <- lm(final_test_formula, data = USERS1)
summary(final_test)



final_test_formula = as.formula("resub ~ u_gender + treated + u_age + u_weekly_utilisation + 
                        u_sub_utilisation + u_format_pref + u_genre_pref + u_rating_given + 
                        u_other_sub + u_plan + u_occupation")

final_test <- lm(final_test_formula, data = USERS1)
summary(final_test)


# Removing unnecessary cols and rename a couple of vars
data_export <- USERS %>% select(
  -baseline_score, -baseline_score_noise, -treatment_score, -treatment_score_noise, -total_score, -u_weekly_utilisation_std,
  -u_sub_utilisation_std, -u_rating_given_std) %>%
  rename(y = resub, treat = treated)
data_export <- as.data.frame(data_export)

# Saving USERS data frame to .csv file
# (Uncomment below if want to overwrite the existing csv saved in the folder)
# write.csv(data_export, "users.csv", row.names = FALSE)




#### EXPLORATORY DATA ANALYSIS ####
data <- as.data.frame(USERS)
# NB. function created in './functions.R'
list_plot=ResubPlots(data=data, vars=c('u_age','u_occupation', 'u_plan', 'u_genre_pref', 'u_other_sub'),
                    target='resub', treat = 'treated')
ggarrange(plotlist = list_plot[[2]])
ggarrange(plotlist = list_plot[[1]])
