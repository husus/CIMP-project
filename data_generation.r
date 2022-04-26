m(list = ls())
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


# total number of users under study
num_users <- 10000

u_id <- seq(1, num_users)

u_age <- sample(
                c(1, 2, 3, 4, 5, 6), num_users, replace = T,
                prob = c(0.1, 0.30, 0.35, 0.15, 0.05, 0.05)
                )
# 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

u_gender <- sample(c(1, 2), num_users, replace = T, prob = c(0.6, 0.4))
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


## Creating Treatment Effects ##

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


## Unifying baseline and treatment  scores ##
USERS$total_score <- USERS$baseline_score + USERS$treatment_score

# How to assign churn? Assume that 15% of customer churn
threshold_churn <- quantile(USERS$baseline_score, prob = c(.15))
USERS[, resub := ifelse(total_score > threshold_churn, 1, 0)]
summary(USERS$resub)

# Adding additional noise by allowing an erratic behavior of 5% of customer
perc_err <- num_users * 0.05
USERS[sample(USERS$u_id, perc_err), resub := ifelse(resub == 0, 1, 0)]

#Save USERS to .csv
write.csv(USERS, "users.csv", row.names = FALSE)