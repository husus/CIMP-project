rm(list = ls())
options(scipen=999)
set.seed(10)

# list of all the necessary packages 
listOfPackages <- list("dplyr","tidyverse","data.table","ggplot2","ggpubr",
                    "gplots","plotly","ggrepel","RColorBrewer",
                    "reshape2","margins", "tools4uplift","xgboost","grf","ggplot2","matrixStats",
                    "Rcpp","car","mltools","glmnet",
                    "caret","mlr", "ROCR")

# Uncomment the next lines if there are packages not installed yet
# for (i in listOfPackages) {
#       install.packages(i, dependencies = TRUE)
# }

# packages for Data Manipulation 
library("dplyr")
# library('tidyverse')
library("data.table")

# general tools for regressions and Marginal Effects
# library("car")
# library("margins")

# packages for modeling
library("tools4uplift") #for uplift modeling
# library('xgboost') #for XGBoosting
# library("grf") #for honest causal forest

# miscellaneous
# library("matrixStats")
# library("reshape2")
# library("Rcpp")
# library('mltools')
# library('glmnet')
library('caret')
# library("mlr")
library('ROCR')

# packages for Graphs and Plots
library("ggplot2")
# library("ggpubr")
# library("gplots")
library("plotly")
# library("ggrepel")
library("RColorBrewer")

# Defining a personalized theme for ggplot
mytheme<-theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

###############################################################

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

u_rating_given <- round(runif(num_users,0,5),2)
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
pal6 <- brewer.pal(6, "Set2")

barplot(table(USERS$u_age, USERS$u_occupation), beside=T, col=pal6,
        names.arg=occup_group, main='Distribution of age vs occupation groups')
legend("topright", legend=age_group, fill=pal6, cex = 0.9,
        text.width = 3.5, y.intersp = 1.5, title='Age group')


# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1,num_users,replace=T)


# we suppose that our streaming service is focused on action and sci-fi tv-series 
# u_genre_pref(1=action||4=sci-fi), u_format_pref(1=series), u_age(-), u_occupation(-)
# u_other_sub(-), u_rating_given(+), u_sub_utilisation(+), u_weekly_utilisation(+)
# add error term through rnorm

# score = u_genre_pref(1|4) 80 + u_format_pref(1) 100 + u_age(1|2) 30 - u_age(3|4|5|6) 30 
#         + u_occupation(1|4|5) 30 - u_occupation(2|3) 30 - u_other_sub*55 + u_rating_given*50
#         + u_sub_utilisation*130 + u_weekly_utilisation*115 + u_plan(2) 50 + error
# what about the subscription plan? (+) for family plan, more incentivized to resub
#                     for treatment score: (-) for family bc incentive not enough, indiv more price sensitive

# NB: coefficients arbitrarily given
set.seed(10)
USERS[, base_score := u_rating_given*50 + u_sub_utilisation*130 + u_weekly_utilisation*115
                        - u_other_sub*55 + rnorm(1)*100] #NOTE: magnitude of the noise
USERS[u_genre_pref==1|u_genre_pref==4, base_score := base_score+80]
USERS[u_format_pref==1, base_score := base_score+100]
USERS[,base_score := ifelse(u_age==1|u_age==2,base_score+30, base_score-30)] 
USERS[,base_score := ifelse(u_occupation==2|u_occupation==3, base_score-30, base_score+30)]
USERS[u_plan==2, base_score := base_score+50] 

# USERS$base_score_scaled <- scale(USERS$base_score)  #scaling the scores

summary(USERS$base_score)

hist(USERS$base_score, main="Distribution of users' baseline scores",
    xlab="Baseline scores", ylab="Frequency",
    xlim=range(-500,1800), col='light blue')

hist(USERS$base_score_scaled, main="Distribution of users' baseline scores scaled",
    xlab="Baseline scores scaled", ylab="Frequency",
    xlim=range(-5,5), col='light blue')



# The impact of our policy can be divided into two components: 
# First: an additive component independent of covariates and positive on average
set.seed(10)
USERS[, treatment_score := ifelse(treated==1, rnorm(num_users,40,20), 0)]
summary(USERS$treatment_score)
hist(USERS$treatment_score, main="Distribution of users' treatment scores",
    xlab="Treatment scores", ylab="Frequency", col='light blue')
# NOTE: Remember to set appropriate size for the random component


# Second: part of the effect depends on some user's characteristics (interactions).
# To capture higher price sensitivity of young people and students/unemployed
USERS[treated==1,treatment_score:=ifelse(u_age==1|u_age==2,treatment_score+70,treatment_score)]
USERS[treated==1,treatment_score:=ifelse(u_occupation==2|u_occupation==3|u_occupation==5,treatment_score,treatment_score+100)]
# As well as the price sensitivity of people subscribed to individual plans
 USERS[treated==1, treatment_score:=ifelse(u_plan==1,treatment_score+70, treatment_score)]
# We assume we face different degrees of competition depending on the favorite genre of users: 
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
# We assume that 45% of customer churn, in particular the first quantile
threshold_churn = quantile(USERS$base_score, prob=0.45)
USERS[, resub := ifelse(total_score>threshold_churn,1,0)]

# to create some error in the dataset, for some (100) random ids switch between 0 and 1
set.seed(10)
perc_err = round(num_users*0.05,0)
USERS[sample(USERS$u_id,perc_err), resub := ifelse(resub==1,0,1)]
summary(USERS$resub)


# convert categorical variables to factor 
cat_var <- c('u_gender', 'u_occupation','u_format_pref', 'u_genre_pref', 'u_other_sub', 'u_plan')
USERS[, cat_var] <- lapply(USERS[,..cat_var] , factor)



set.seed(10)
DATA <- USERS %>% select( -base_score, -treatment_score, -total_score)
TRAIN_DATA <- DATA %>% sample_frac(.8)
trainIndex <- TRAIN_DATA[,u_id]

TEST_DATA <- DATA %>% filter(!u_id %in% trainIndex)
TRAIN_DATA <- TRAIN_DATA %>% select(-u_id)
TEST_DATA <- TEST_DATA %>% select(-u_id)

# X_train <- TRAIN_DATA %>% select(-resub, -base_score, -base_score_scaled, -treatment_score, -total_score)
# y_train <- TRAIN_DATA %>% select(resub)

# X_test <- TEST_DATA %>% select(-resub, -u_id, -base_score, -base_score_scaled, -treatment_score, -total_score)
# y_test <- TEST_DATA %>% select(resub)


###### Single Logistic Regression Model #####
# features <- names(TRAIN_DATA)[1:11]
# logitformula <- paste("resub~", paste(features, collapse='+'))

# logit_model <- glm(formula=logitformula, data=TRAIN_DATA, family= binomial(link=logit))

# summary(logit_model)
# anova(logit_model, test="Chisq") #to analyze the table of deviance

# # Prediction
# TEST_DATA$pred_prob <- predict(logit_model, newdata=TEST_DATA, type='response')
# summary(TEST_DATA$pred_prob)
# plot(TEST_DATA$pred_prob)
# TEST_DATA$pred <- ifelse(TEST_DATA$pred_prob > 0.5,1,0)

# # Prediction Model Evaluation 
# misClasificError <- mean(TEST_DATA$pred != TEST_DATA$resub)
# print(paste('Accuracy:', 1-misClasificError))

# cm <- confusionMatrix(data=factor(TEST_DATA$pred), reference = factor(TEST_DATA$resub))
# cm
# barplot(table(TEST_DATA$pred, TEST_DATA$resub), col=rainbow(2), 
#         xlab='True Y', main='Prediction Evaluation', names.arg=c(0,1), 
#         legend=T, args.legend=list(x='topleft',title='predicted', 
#         cex=0.9,text.width = 0.2, y.intersp = 1.4))

# # Plotting ROC curve
# p <- prediction(TEST_DATA$pred_prob, TEST_DATA$resub)
# perf <- performance(p, measure = "tpr", x.measure = "fpr")
# plot(perf)

# # Area under the curve (TP vs FP)
# auc <- performance(p, measure = "auc")
# auc <- auc@y.values[[1]]
# print(paste('AUC:', auc))



###### Two-Model Logistic Regression (treatment vs control) #####
CTL <- DATA %>% filter(treated==0) #control group
TRT <- DATA %>% filter(treated==1) #treatment group


### Treatment Group Model ###
set.seed(10)
TRAIN_T <- TRT %>% sample_frac(.8)
trainIndex <- TRAIN_T[,u_id]

TEST_T <- TRT %>% filter(!u_id %in% trainIndex)
TRAIN_T <- TRAIN_T %>% select(-u_id, -treated)
TEST_T <- TEST_T %>% select(-u_id, -treated)

new_features <- names(TRAIN_T)[1:10]
new_logitformula <- paste("resub~", paste(new_features, collapse='+'))

# Fitting the model
treat_logit <- glm(formula=new_logitformula, data=TRAIN_T, family= binomial(link=logit))
summary(treat_logit)
anova(treat_logit, test="Chisq")

# Prediction
TEST_T$pred_prob <- predict(treat_logit, newdata=TEST_T, type='response')
summary(TEST_T$pred_prob)
plot(TEST_T$pred_prob)
TEST_T$pred <- ifelse(TEST_T$pred_prob > 0.5,1,0)

# Evaluation
cm <- confusionMatrix(data=factor(TEST_T$pred), reference = factor(TEST_T$resub))
cm
barplot(table(TEST_T$pred, TEST_T$resub), col=rainbow(2), 
        xlab='True Y', main='Prediction Evaluation', names.arg=c(0,1), 
        legend=T, args.legend=list(x='topleft',title='predicted', 
        cex=0.9,text.width = 0.2, y.intersp = 1.4))

# ROC curve
p <- prediction(TEST_T$pred_prob, TEST_T$resub)
perf <- performance(p, measure = "tpr", x.measure = "fpr")
plot(perf)

# AUC (TP vs FP)
auc <- performance(p, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('AUC:', auc))



### Control Group Model ###
set.seed(10)
TRAIN_C <- CTL %>% sample_frac(.8)
trainIndex <- TRAIN_C[,u_id]

TEST_C <- CTL %>% filter(!u_id %in% trainIndex)
TRAIN_C <- TRAIN_C %>% select(-u_id, -treated)
TEST_C <- TEST_C %>% select(-u_id, -treated)

# Fitting the model
control_logit <- glm(formula=new_logitformula, data=TRAIN_C, family= binomial(link=logit))
summary(control_logit)
anova(control_logit, test="Chisq")

# Prediction
TEST_C$pred_prob <- predict(control_logit, newdata=TEST_C, type='response')
summary(TEST_C$pred_prob)
plot(TEST_C$pred_prob)
TEST_C$pred <- ifelse(TEST_C$pred_prob > 0.5,1,0)

# Evaluation
cm <- confusionMatrix(data=factor(TEST_C$pred), reference = factor(TEST_C$resub))
cm
barplot(table(TEST_C$pred, TEST_C$resub), col=rainbow(2), 
        xlab='True Y', main='Prediction Evaluation', names.arg=c(0,1), 
        legend=T, args.legend=list(x='topleft',title='predicted', 
        cex=0.9,text.width = 0.2, y.intersp = 1.4))

# ROC curve
p <- prediction(TEST_C$pred_prob, TEST_C$resub)
perf <- performance(p, measure = "tpr", x.measure = "fpr")
plot(perf)

# AUC (TP vs FP)
auc <- performance(p, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('AUC:', auc))



##### Model Predicted Probabilities of Resubscription per each user (FACTUAL) #####
TRT <- TRT %>% select(-u_id)
TRT$pred_prob <- predict(treat_logit, newdata=TRT[, 1:10], type='response')

CTL <- CTL %>% select(-u_id)
CTL$pred_prob <- predict(control_logit, newdata=CTL[, 1:10], type='response')

##### Applying the Treatment Model on the Control Group (COUNTERFACTUAL) #####
CTL$prob_counterfac <- predict(treat_logit, newdata=CTL[, 1:10], type='response')
CTL$counterfac <- ifelse(CTL$prob_counterfac > 0.5,1,0)

# relative frequency of resub in control group
CTL %>%
    group_by(resub) %>%
    summarise(n = n()) %>%
    mutate(freq = paste(round(prop.table(n)*100, 2), '%'))

# relative frequency of control group counterfactual
CTL %>%
    group_by(counterfac) %>%
    summarise(n = n()) %>%
    mutate(freq = paste(round(prop.table(n)*100, 2), '%'))


##### Applying the Control Model on the Treatment Group #####
TRT$prob_counterfac <- predict(control_logit, newdata=TRT[, 1:10], type='response')
TRT$counterfac <- ifelse(TRT$prob_counterfac > 0.5,1,0)

# relative frequency of resub in treatment group
TRT %>%
    group_by(resub) %>%
    summarise(n = n()) %>%
    mutate(freq = paste(round(prop.table(n)*100, 2), '%'))

# relative frequency of treatment group counterfactual
TRT %>%
    group_by(counterfac) %>%
    summarise(n = n()) %>%
    mutate(freq = paste(round(prop.table(n)*100, 2), '%'))


# Combine into one single dataframe CTL and TRT
two_model_data <- rbind(CTL, TRT)
two_model_data <- two_model_data[sample(1:nrow(two_model_data)), ] #shuffle rows
two_model_data$tau <- two_model_data$pred_prob - two_model_data$prob_counterfac

##### Performance Uplift Calculation #####
perf_two_model <- PerformanceUplift(data = two_model_data, treat = "treated", outcome = "resub", 
                            prediction = "pred_prob", equal.intervals = TRUE, nb.group = 10)
perf_two_model


# Plotting Qini curve and Qini Coeff
QiniPlot <- function (performance, modeltype) {
  
  #Plot qini curves (abs and %) starting from performance obejcts
  # of Tools4Uplift Package
  # Args:
  #  performance: performance object for estimating Qini Curves
  #  modeltype: model type for filling up the title 
  
  df=data.frame(matrix(nrow=10, ncol=3))
  df[,1]=performance[[1]]
  df[,2]=round(performance[[6]],2)
  df[,3]=round(performance[[7]],2)
  colnames(df)=c("Dec", "num.incr", "perc.incr")
  firstrow=numeric(3)
  df=rbind(firstrow,df)
  
  # Plot Qini curves
  qini_curve_1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
    mytheme+labs(title=sprintf("Qini Curve (abs) - %s", modeltype), y="Incr. Numb. of Resub. Cust.", x="Perc. of Customers Targeted")+
    scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                           linetype="dashed", size=0.5)
  
  qini_curve_2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
    mytheme+labs(title=sprintf("Qini Curve (perc.) - %s", modeltype), y="Incr. % of Resub. Cust.", x="Perc. of Customers Targeted")+
    xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
                            linetype="dashed", size=0.5)
  
  plot_list=list(qini_curve_1, qini_curve_2)
  
  return(plot_list)
}

plot_list = QiniPlot(performance=perf_two_model, modeltype = 'Two-Model Logit')
plot_list[[1]]
plot_list[[2]]
