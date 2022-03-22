rm(list = ls())
library("dplyr")
library("data.table")
options(scipen=999)

# total number of users under study
num_users <- 1000 

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

# creating the data table with all the users
USERS <- data.table(u_id, u_gender, u_age, u_weekly_utilisation, u_sub_utilisation, u_format_pref,
                    u_genre_pref, u_rating_given, u_other_sub)

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
# add error term through rnorm

# score = u_genre_pref(1|4) 80 + u_format_pref(1) 100 + u_age(1|2) 30 - u_age(3|4|5|6) 30 
#         + u_occupation(1|4|5) 30 - u_occupation(2|3) 30 - u_other_sub*55 + u_rating_given*50
#         + u_sub_utilisation*130 + u_weekly_utilisation*115 + error

# NB: coefficients arbitrarily given
USERS[,score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55+rnorm(1)*100]
USERS[u_genre_pref==1|u_genre_pref==4, score:=score+80]
USERS[u_format_pref==1, score:=score+100]
USERS[,score:=ifelse(u_age==1|u_age==2,score+30,score-30)] 
USERS[,score:=ifelse(u_occupation==2|u_occupation==3,score-30,score+30)] 

USERS$score <- scale(USERS$score)  #scaling the scores

# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1,num_users,replace=T)


USERS[,churn:=ifelse(score>0,0,1)] #if positive score, the user doesn't churn (0), otherwise they churn (1)

# to create some error in the dataset, for some random ids switch between 0 and 1
seed(10)
USERS[sample(USERS$u_id,100),churn:=ifelse(churn==1,0,1)]
