rm(list = ls())
options(scipen=999)
set.seed(10)

library("dplyr")
library("data.table")

# total number of users under study
num_users <- 10000 

u_id <- seq(1,num_users)


u_age <- sample(c(1,2,3,4,5,6),num_users,replace=T,prob=c(0.1,0.30,0.35,0.15,0.05,0.05))
# 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

u_gender <- sample(c(1,2),num_users,replace=T,prob=c(0.6,0.4))
# 1=M, 2=F

u_weekly_utilisation <- sample(0:7,num_users,replace=T)
# number of days using the service in a week

u_sub_utilisation <- round(runif(num_users,0,1),4)
# proportion of time spent on the service since the first subscription

u_rating_given <- sample(0:5,num_users,replace=T)
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

age_group = c('<18','[18,25)','[25,35)','[35,45)','[45,55)','≥55')
occup_group = c('student','part-time','full-time','unemployed','retired')
barplot(table(USERS$u_age, USERS$u_occupation), beside=T, 
        col=rainbow(6), names.arg=occup_group, main='Distribution of age vs occupation groups',
        legend=TRUE, legend.text=age_group, args.legend = list(bty = 'n', y.intersp = 1.5) )

# we suppose that our streaming service is focused on action and sci-fi tv-series 
# u_genre_pref(1=action||4=sci-fi), u_format_pref(1=series), u_age(-), u_occupation(-)
# u_other_sub(-), u_rating_given(+), u_sub_utilisation(+), u_weekly_utilisation(+)
# add error term through rnorm

# score = u_genre_pref(1|4) 80 + u_format_pref(1) 100 + u_age(1|2) 30 - u_age(3|4|5|6) 30 
#         + u_occupation(1|4|5) 30 - u_occupation(2|3) 30 - u_other_sub*55 + u_rating_given*50
#         + u_sub_utilisation*130 + u_weekly_utilisation*115 + error
# what about the subscription plan?

# NB: coefficients arbitrarily given
USERS[,base_score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55+rnorm(1)*100]
USERS[u_genre_pref==1|u_genre_pref==4, base_score:=base_score+80]
USERS[u_format_pref==1, base_score:=base_score+100]
USERS[,base_score:=ifelse(u_age==1|u_age==2,base_score+30,base_score-30)] 
USERS[,base_score:=ifelse(u_occupation==2|u_occupation==3,base_score-30,base_score+30)] 

USERS$base_score_scaled <- scale(USERS$base_score)  #scaling the scores


# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1,num_users,replace=T)


summary(USERS$base_score)
hist(USERS$base_score, main="Distribution of users' baseline scores",
    xlab="Baseline scores", ylab="Frequency",
    xlim=range(-500,1800), col='light blue')

hist(USERS$base_score_scaled, main="Distribution of users' baseline scores scaled",
    xlab="Baseline scores scaled", ylab="Frequency",
    xlim=range(-5,5), col='light blue')

# Adding a random component to the underlying score (observable to customers but unobservable to econometricians)
USERS$new_base_score = USERS$base_score + rnorm(1,0,50)*10

# NOTE: modified size/magnitude of the noise

summary(USERS$new_base_score)
hist(USERS$new_base_score, main="Distribution of users' baseline scores modified",
    xlab="Modified baseline scores", ylab="Frequency",
    xlim=range(0,2000), col='light blue')


# The impact of our policy can be divided into two components: 
# First: an additive component independent of covariates and positive on average
USERS[, treatment_score := ifelse(treated==1,rnorm(num_users,40,20),0)]
summary(USERS$treatment_score)
hist(USERS$treatment_score, main="Distribution of users' treatment scores",
    xlab="Treatment scores", ylab="Frequency", col='light blue')
# NOTE: Remember to set appropriate size for the random component


# Second: part of the effect depends on some user's characteristics (interactions).
# For example, #to capture the higher price sensitivity of young people and students/unemployed:
USERS[treated==1,treatment_score:=ifelse(u_age==1|u_age==2,treatment_score+70,treatment_score)]
USERS[treated==1,treatment_score:=ifelse(u_occupation==2|u_occupation==3|u_occupation==5,treatment_score,treatment_score+100)] 
# We may assume we face different degrees of competition depending on the favorite genre of users: 
USERS[treated==1,treatment_score:=ifelse(u_genre_pref==2|u_genre_pref==3,treatment_score,treatment_score+50)] 
# Finally, a voucher would reduce multihoming costs of being subscribed to multiple platforms
USERS[u_other_sub==1&treated==1, treatment_score:=treatment_score+60]


summary(USERS$treatment_score)
hist(USERS$treatment_score, main="Final distribution of users' treatment scores",
    xlab="Treatment scores", ylab="Frequency", col='light blue')


# Sum the baseline and the treatment scores to get each user's total score
USERS$total_score = USERS$base_score + USERS$treatment_score
summary(USERS$total_score)
hist(USERS$total_score, main="Distribution of users' total scores",
    xlab="Total scores", ylab="Frequency", col='light blue')



# Assign which customer will churn and which will re-subscribe (Y variable)
# We assume that 15% of customer churn, in particular the first quantile
threshold_churn=quantile(USERS$base_score, prob=0.15)
USERS[, resub := ifelse(total_score>threshold_churn,1,0) ] 
summary(USERS$resub)

# to create some error in the dataset, for some (100) random ids switch between 0 and 1
# USERS[sample(USERS$u_id,100),resub:=ifelse(resub==1,0,1)]




######## EDA (resub vs no resub) ########
control <- USERS[treated==0]
treatment <- USERS[treated==1]

summary(treatment)


# check the distribution of users' characteristics among Treatment and Control groups

barplot(table(treatment$u_age), col='#CC0000',
        names.arg=age_group, main='Distribution of age groups in Treatment',
        xlab='Age group', ylab='count')

barplot(table(control$u_age), col='#66CCFF',
        names.arg=age_group, main='Distribution of age groups in Control',
        xlab='Age group', ylab='count')




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
legend("top", legend=0:1, fill=pal, cex = 0.5, title='resub')
barplot(table(control$u_gender, control$resub), beside=T, col=pal7, xlab='resub', main='control group')
legend("bottom", legend=1:7, fill=pal, cex = 0.5, title='gender')

# To do:
# 1. add probability to the features about utilisation --> skewed distribution

