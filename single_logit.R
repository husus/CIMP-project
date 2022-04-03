rm(list = ls())
options(scipen=999)
set.seed(10)

# list of all the necessary packages 
listOfPackages <- list("dplyr","tidyverse","data.table","ggplot2","ggpubr",
                    "gplots","plotly","ggrepel","RColorBrewer",
                    "reshape2","margins", "tools4uplift","xgboost","grf","ggplot2","matrixStats",
                    "Rcpp","car","mltools","glmnet",
                    "caret","mlr")

# Uncomment the next lines if there are packages not installed yet
# for (i in listOfPackages) {
#       install.packages(i, dependencies = TRUE)
# }

# packages for Data Manipulation 
library("dplyr")
library('tidyverse')
library("data.table")

# general Tools for Regressions and Marginal Effects
library("car")
library("margins")

# mackages for modeling
library("tools4uplift") #for uplift modeling
library('xgboost') #for XGBoosting
library("grf") #for honest causal forest

# miscellaneous
library("matrixStats")
library("reshape2")
library("Rcpp")
library('mltools')
library('glmnet')
library('caret')
library("mlr")

# packages for Graphs and Plots
library("ggplot2")
library("ggpubr")
library("gplots")
library("plotly")
library("ggrepel")
library("RColorBrewer")

# Defining a personalized theme for ggplot
mytheme<-theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

###############################################################


# total number of users under study
num_users <- 1500 

u_id <- seq(1,num_users)

set.seed(10)
u_age <- sample(c(1,2,3,4,5,6),num_users,replace=T,prob=c(0.1,0.30,0.35,0.15,0.05,0.05))
# 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

u_gender <- sample(c(1,2),num_users,replace=T,prob=c(0.6,0.4))
# 1=M, 2=F

u_weekly_utilisation <- sample(0:7,num_users,replace=T) #to add probability
# number of days using the service in a week

u_sub_utilisation <- round(runif(num_users,0,1),4) #to add probability
# proportion of time spent on the service since the first subscription

u_rating_given <- sample(0:5,num_users,replace=T) #to add probability
# rating on a scale from 0 to 5 given by each user to the platform

u_format_pref <- sample(c(1,2,3),num_users,replace=T,prob=c(0.5,0.4,0.1))
# 1=TV-series, 2=movies, 3=documentaries

u_genre_pref <- sample(1:7,num_users,replace=T)
# 1=action, 2=comedy, 3=romance, 4=sci-fi, 5=animation, 6=drama, 7=horror

u_other_sub <- sample(0:1,num_users,replace=T)
# binary variable where 0=not subscribed to other streaming platforms, 1=yes

u_plan <- sample(1:2,num_users,replace=T)
# type of subscription plan: 1=individual 2=family

# creating the data table with all the users
USERS <- data.table(u_id, u_gender, u_age, u_weekly_utilisation, u_sub_utilisation, u_format_pref,
                    u_genre_pref, u_rating_given, u_other_sub, u_plan)


# defining the users' occupation variable based on some conditions
USERS$u_occupation[u_age==1] <- 1
USERS$u_occupation[u_age==2|u_age==3] <- sample(c(2,3,4),nrow(USERS[u_age==2|u_age==3]),replace=T,prob=c(0.4,0.4,0.2))
USERS$u_occupation[u_age==4|u_age==5] <- sample(c(2,3,4),nrow(USERS[u_age==4|u_age==5]),replace=T,prob=c(0.1,0.7,0.2))
USERS$u_occupation[u_age==6] <- sample(c(3,5),nrow(USERS[u_age==6]),replace=T,prob=c(0.3,0.7))
# occupation: 1=student 2=part-time 3=full-time 4=unemployed 5=retired
# age: 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

table(USERS$u_age, USERS$u_occupation)
barplot(table(USERS$u_age, USERS$u_occupation), beside=T, legend=T, col=rainbow(6))


# we suppose that our streaming service is focused on action and sci-fi tv-series 
# u_genre_pref(1=action||4=sci-fi), u_format_pref(1=series), u_age(-), u_occupation(-)
# u_other_sub(-), u_rating_given(+), u_sub_utilisation(+), u_weekly_utilisation(+)
# add error term from rnorm

# score = u_genre_pref(1|4) 80 + u_format_pref(1) 100 + u_age(1|2) 30 - u_age(3|4|5|6) 30 
#         + u_occupation(1|4|5) 30 - u_occupation(2|3) 30 - u_other_sub*55 + u_rating_given*50
#         + u_sub_utilisation*130 + u_weekly_utilisation*115 + error
# what about the subscription plan?

# NB: coefficients arbitrarly given
USERS[,base_score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55+rnorm(1)*100]
USERS[u_genre_pref==1|u_genre_pref==4, base_score:=base_score+80]
USERS[u_format_pref==1, base_score:=base_score+100]
USERS[,base_score:=ifelse(u_age==1|u_age==2,base_score+30,base_score-30)] 
USERS[,base_score:=ifelse(u_occupation==2|u_occupation==3,base_score-30,base_score+30)] 

USERS$base_score_scaled <- scale(USERS$base_score)  #scaling the scores

# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1,num_users,replace=T)


#if positive score, the user doesn't churn and re-subscribe (1), otherwise they churn (0)
USERS[,resub:=ifelse(base_score_scaled>0,1,0)]

# to create some error in the dataset, for some random ids switch btw 0 and 1
USERS[sample(USERS$u_id,150),resub:=ifelse(resub==1,0,1)]



control <- USERS[treated==0]

treatment <- USERS[treated==1]

summary(treatment)

library(ggplot2)

ggplot(treatment, aes(treatment$u_age)) +
       geom_bar(fill = "#0073C2FF")

ggplot(control, aes(control$u_age)) +
       geom_bar(fill = "dark red")

library(RColorBrewer)

pal6 <- brewer.pal(6, "Set2")
barplot(table(treatment$u_age, treatment$resub), beside=T, col=pal6, xlab='resub', main='treatment group')
legend("topright", legend=1:6, fill=pal6, cex = 0.7, title='age')
barplot(table(control$u_age, control$resub), beside=T, col=pal6, xlab='resub', main='control group')
legend("topright", legend=1:6, fill=pal6, cex = 0.7, title='age')

pal5 <- brewer.pal(5, "Set2")
barplot(table(treatment$u_occupation, treatment$resub), beside=T, col=pal5, xlab='resub', main='treatment group')
legend("topright", legend=1:5, fill=pal5, cex = 0.7, title='occup')
barplot(table(control$u_occupation, control$resub), beside=T, col=pal5, xlab='resub', main='control group')
legend("topright", legend=1:5, fill=pal5, cex = 0.7, title='occup')

barplot(table(treatment$u_rating_given, treatment$resub), beside=T, col=pal6, xlab='resub', main='treatment group')
legend("top", legend=0:5, fill=pal6, cex = 0.5, title='rating')
barplot(table(control$u_rating_given, control$resub), beside=T, col=pal6, xlab='resub', main='control group')
legend("top", legend=0:5, fill=pal6, cex = 0.5, title='rating')

hist(treatment$u_sub_utilisation)
hist(control$u_sub_utilisation)

pal7 <- brewer.pal(7, "Set2")
barplot(table(treatment$u_genre_pref, treatment$resub), beside=T, col=pal7, xlab='resub', main='treatment group')
legend("bottom", legend=1:7, fill=pal7, cex = 0.5, title='genre')
barplot(table(control$u_genre_pref, control$resub), beside=T, col=pal7, xlab='resub', main='control group')
legend("bottom", legend=1:7, fill=pal7, cex = 0.5, title='genre')

pal3 <- brewer.pal(3, "Set2")
barplot(table(treatment$u_format_pref, treatment$resub), beside=T, col=pal3, xlab='resub', main='treatment group')
legend("topright", legend=1:3, fill=pal3, cex = 0.7, title='occup')
barplot(table(control$u_format_pref, control$resub), beside=T, col=pal3, xlab='resub', main='control group')
legend("topright", legend=1:3, fill=pal3, cex = 0.7, title='occup')

barplot(table(treatment$resub, treatment$u_gender), col=rainbow(2), xlab='gender', main='treatment group')
legend("topright", legend=0:1, fill=pal, cex = 0.5, title='resub')
barplot(table(control$u_gender, control$resub), beside=T, col=pal7, xlab='resub', main='control group')
legend("bottom", legend=1:7, fill=pal, cex = 0.5, title='gender')



Y <- USERS[,resub]
W <- USERS[,treated]
X <- USERS[-resub -treated -base_score -base_score_scaled, ]






