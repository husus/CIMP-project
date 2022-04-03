rm(list = ls())
options(scipen=999)
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

# General Tools for Regressions and Marginal Effects
library("car")
library("margins")

# Package for regression uplifting models
library("tools4uplift")

# Package for XGBoosting
library("xgboost")

# Package for HCF
library("grf")

# Miscellaneous
library("matrixStats")
library("reshape2")
library("Rcpp")
library("mltools")
library("glmnet")
library("caret")
library("mlr")


#Automatically Install Missing Packages
listOfPackages <- c("dplyr","tidyverse","data.table","ggplot2","ggpubr",
                    "gplots","plotly","ggrepel","RColorBrewer",
                    "reshape2","margins", "tools4uplift","xgboost","grf",
                    "ggplot2","matrixStats",
                    "Rcpp","car","mltools","glmnet",
                    "caret","mlr")
for (i in listOfPackages) {
     if(! i %in% installed.packages()){
         install.packages(i, dependencies = TRUE)
         library(i)
     }
}


# total number of users under study - Increased from 1000 to 100000 (experiment)
num_users <- 100000

u_id <- seq(1,num_users)

u_age <- sample(c(1,2,3,4,5,6),num_users,replace=T,prob=c(0.1,0.30,0.35,0.15,0.05,0.05))
# 1=<18 2=[18,25) 3=[25,35) 4=[35,45), 5=[45,55), 6=>55

u_gender <- sample(c(1,2),num_users,replace=T,prob=c(0.6,0.4))
# 1=M, 2=F

u_weekly_utilisation <- sample(0:7,num_users,replace=T)
# number of days using the service in a week

u_sub_utilisation <- round(runif(num_users,0,1),2)
# proportion of time spent on the service since the first subscription

#u_rating_given <- sample(0:5,num_users,replace=T)
u_rating_given<-round(runif(num_users,0,5),2)

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


#USERS

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
# NB: coefficients arbitrarly given
# USERS[,score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55+rnorm(1)*100]

# Deterministic Portion of Utility
USERS[,baseline_score:=u_rating_given*50+u_sub_utilisation*130+u_weekly_utilisation*115-u_other_sub*55]
USERS[u_genre_pref==1|u_genre_pref==4, baseline_score:=baseline_score+80]
USERS[u_format_pref==1, baseline_score:=baseline_score+100]
USERS[,baseline_score:=ifelse(u_age==1|u_age==2,baseline_score+30,baseline_score-30)] 
USERS[,baseline_score:=ifelse(u_occupation==2|u_occupation==3,baseline_score-30,baseline_score+30)] 

summary(USERS$baseline_score)
hist(USERS$baseline_score)

# Random Component of Utility (observable to customers but unobservable to the econometrician)
USERS$baseline_score=USERS$baseline_score+rnorm(1,0,70)

# NOTE: Remember to set appropriate size for the noise

summary(USERS$baseline_score)
hist(USERS$baseline_score)

# Creating Treatment Effects

# treatment variable randomly assigned to the users
USERS$treated <- sample(0:1,num_users,replace=T)

# The impact of our policy can be divided into two components: 
# First: an additive component independent of covariates and positive on average
USERS[,treatment_score:=ifelse(treated==1,rnorm(num_users,40,20),0)]
summary(USERS$treatment_score)
hist(USERS$treatment_score)
# NOTE: Remember to set appropriate size for the random component


# Second: part of the effect depends on some user's characteristics (interactions).
# For example, #to capture the higher price sensitivity of young people and students/unemployed:
USERS[treated==1,treatment_score:=ifelse(u_age==1|u_age==2,treatment_score+70,treatment_score)]
USERS[treated==1,treatment_score:=ifelse(u_occupation==2|u_occupation==3|u_occupation==5,treatment_score,treatment_score+100)] 
# We may assume we face different degrees of competition depending on the favorite genre of users: 
USERS[treated==1,treatment_score:=ifelse(u_genre_pref==2|u_genre_pref==3,treatment_score,treatment_score+50)] 
# Finally, a voucher would reduce multihoming costs of being subscribed to multiple platforms
USERS[u_other_sub==1&treated==1, treatment_score:=treatment_score+60]

# Overall, we get
summary(USERS$treatment_score)
hist(USERS$treatment_score)

# Unifying baseline and treatment  scores
USERS$total_score=USERS$baseline_score+USERS$treatment_score
summary(USERS$total_score)
hist(USERS$total_score)

#How to assign churn?
#Assume that 15% of customer churn
threshold_churn=quantile(USERS$baseline_score, prob=c(.15))
USERS[,resub:=ifelse(total_score>threshold_churn,1,0)] 
summary(USERS$resub)

USERS

# Adding additional noise by allowing an erratic behavior of 5% of customer 
set.seed(10)
perc_err=num_users*0.05
USERS[sample(USERS$u_id,perc_err),resub:=ifelse(resub==0,1,0)]

USERS


# Scaling scores
# USERS$score_scaled <- scale(USERS$score)  #scaling the scores
# USERS[,churn:=ifelse(score>0,0,1)] #if positive score, the user doesn't churn (0), otherwise they churn (1)
# to create some error in the dataset, for some random ids switch btw 0 and 1
# seed(10)
# USERS[sample(USERS$u_id,100),churn:=ifelse(churn==1,0,1)]

data = USERS %>% select(-baseline_score, -treatment_score, -total_score) %>% rename(y=resub, treat=treated)
data=as.data.frame(data)

# Converting categorical variables into factors
data = data %>%
  mutate_at(vars(u_gender, u_format_pref, u_genre_pref, u_other_sub, u_occupation),
            funs(factor))

# We also perform one hot encoding, to be used in models which do not support factors

data_cat= data %>% select(u_gender, u_format_pref, u_genre_pref, u_other_sub, u_occupation)
data_noncat= data %>% select(-u_gender, -u_format_pref, -u_genre_pref, -u_other_sub, -u_occupation, -u_id)
data_oh = one_hot(as.data.table(data_cat))
data_oh=cbind(data$u_id, data_oh, data_noncat)
colnames(data_oh)[1]='u_id'
data_oh$y=as.factor(data_oh$y)

# Dividing our Dataset in Three parts: Training, Test and Holdout

#holdout_data_id = sample(seq_len(nrow(data)), size = num_users*0.20)
#working_data = data[-holdout_data_id, ]
#holdout_data = data[holdout_data_id, ]
#split = SplitUplift(holdout_data, 0.6, c("treat", "y"))
set.seed(10)
split = SplitUplift(data, 0.6, c("treat", "y"))
train=split[[1]]
test=split[[2]]

# Reproducing the sample split on the hot encoded dataset
set.seed(10)
split_oh = SplitUplift(data_oh, 0.6, c("treat", "y"))
train_oh=as.data.frame(split_oh[[1]])
test_oh=as.data.frame(split_oh[[2]])

# Define the set of covariates (without y and treat)
features=colnames(train)[2:(length(colnames(train))-2)]
features_oh=colnames(train_oh)[2:(length(colnames(train_oh))-2)]


# First of all, we create a copy of data,train and test set exclusively for two-models
data_tm=data
train_tm=train
test_tm=test

#We then separate treated and control in both groups
train_tm_control=subset(train_tm, treat==0)
train_tm_treatment=subset(train_tm, treat==1)
test_tm_control = subset(test_tm, treat==0)
test_tm_treatment = subset(test_tm, treat==1)

# Intuition
# logit_model_C<-glm(y ~ u_weekly_utilisation + u_rating_given + u_gender, family= binomial(link=logit), data=train_tm_control)  
# logit_model_T<-glm(y ~ u_weekly_utilisation + u_rating_given + u_gender, family= binomial(link=logit),  data=train_tm_treatment)
# data_tm$pred_C= logit_model_C %>% predict(data_tm, type = "response")
# data_tm$pred_T= logit_model_T %>% predict(data_tm, type = "response")
# data_tm$tau=data_tm$pred_T-data_tm$pred_C
# print('Estimated Probabilities when Customers are not treated')
# plot_ly(x=data_tm$u_weekly_utilisation, y=data_tm$u_rating_given, z=data_tm$pred_C,  type="scatter3d", mode="markers")
# print('Estimated Probabilities when Customers are treated')
# plot_ly(x=data_tm$u_weekly_utilisation, y=data_tm$u_rating_given, z=data_tm$pred_T,  type="scatter3d", mode="markers")
# print('Estimated TE computed as the difference between the abovementioned probs')
# plot_ly(x=data_tm$u_weekly_utilisation, y=data_tm$u_rating_given, z=data_tm$tau,  type="scatter3d", mode="markers")

# Defining the formula for the model
logitformula=as.formula(paste("y~", paste(features, collapse="+")))

logit_model_C<-glm(logitformula, family= binomial(link=logit), data=train_tm_control)
logit_model_T<-glm(logitformula, family= binomial(link=logit), data=train_tm_treatment)


# Computing the propability of resub for individuals on both the 
# the train and the test set

train_tm$pred_C= logit_model_C %>% predict(train_tm, type = "response")
train_tm$pred_T= logit_model_T %>% predict(train_tm, type = "response")

#computing the difference between these probabilities (tau): it is the estimated individual treatment effect 
train_tm$tau= train_tm$pred_T - train_tm$pred_C


test_tm$pred_C= logit_model_C %>% predict(test_tm, type = "response")
test_tm$pred_T= logit_model_T %>% predict(test_tm, type = "response")
test_tm$tau= test_tm$pred_T - test_tm$pred_C


# Model Evaluation on the test set

perf_tm=PerformanceUplift(data = test_tm, treat = "treat",
                          outcome = "y", prediction = "tau", equal.intervals = TRUE, nb.group = 10)

perf_tm
barplot.PerformanceUplift(perf_tm)

QiniArea(perf_tm)

# Plotting Qini curve and Qini Coeff on the test set - scrivere funzione che lo faccia 
# in automatico

df=data.frame(matrix(nrow=10, ncol=3))
df[,1]=perf_tm[[1]]
df[,2]=round(perf_tm[[6]],2)
df[,3]=round(perf_tm[[7]],2)
colnames(df)=c("Dec", "num.incr", "perc.incr")
firstrow=numeric(3)
df=rbind(firstrow,df)


##Plot Qini curves
qini_curve1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (abs) - TM Logit", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                         linetype="dashed", size=0.5)
qini_curve1

qini_curve2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (%) - TM Logit", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
                          linetype="dashed", size=0.5)
qini_curve2

#### 2.1.2 Model selection ####


# Here there are two options: implementing model selection as one would do
# for binary classification, considering model as separated and using the
# the best single models possible; or implementing model selection by
# so as to maximize the Qini area.



#Once the two models have been estimated, let's derive once gain the
# estimated treatment effects




# Evaluating Performance, Qini curve and Qini Coeff on the test set - 
# scrivere funzione che lo faccia in automatico

perf_tm_final = PerformanceUplift(
                                  data = test_tm, treat = "treat",
                                  outcome = "y", prediction = "tau", 
                                  equal.intervals = TRUE, nb.group = 10
                                  )

perf_tm_final
barplot.PerformanceUplift(perf_tm_final)

QiniArea(perf_tm_final)

# Plotting Qini curve and Qini Coeff on the test set - 
# scrivere funzione che lo faccia in automatico

df = data.frame(matrix(nrow=10, ncol=3))
df[,1] = perf_tm_final[[1]]
df[,2] = round(perf_tm_final[[6]], 2)
df[,3] = round(perf_tm_final[[7]], 2)
colnames(df) = c("Dec", "num.incr", "perc.incr")
firstrow = numeric(3)
df = rbind(firstrow, df)


##Plot Qini curves
qini_curve1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (abs) - TM Logit (opt)", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                         linetype="dashed", size=0.5)
qini_curve1

qini_curve2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (%) - TM Logit (Opt)", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
                          linetype="dashed", size=0.5)
qini_curve2




# ALESSIA XGBOOST 

# Let's experiment with a more complex Classifier: XGBoost -- codice preso online

#Setting Up the xgboost learner
xgb_learner <- makeLearner("classif.xgboost", predict.type = "prob", par.vals = list(
  objective = "binary:logistic", eval_metric = "error"))

#set parameter space
xgb_params <- makeParamSet(
  makeIntegerParam("nrounds", lower = 100, upper = 500), makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("eta", lower = .1, upper = .5), makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x))
ctrl <- makeTuneControlRandom(maxit = 15)  #nel codice era 15, ma per ora ho diminuito per far prima

#set resampling strategy
resample_desc <- makeResampleDesc("CV", iters = 4) #iters = CV with 4 iterations


#creating the model for treatment group:
task<-makeClassifTask(data=train_oh[ ,!(colnames(train_oh) == "treat")], target="y")
tuned_params <- tuneParams(learner = xgb_learner,task = task, resampling = resample_desc,
                           par.set = xgb_params,control = ctrl)
treatment_xgbmodel<- mlr::train(learner = setHyperPars(learner = xgb_learner,par.vals = tuned_params$x),task = task)

#creating the model for control group:
task<-makeClassifTask(data=test_oh[ ,!(colnames(test_oh) == "treat")],target="y")
tuned_params <- tuneParams(learner = xgb_learner,task = task,resampling = resample_desc,
                           par.set = xgb_params,control = ctrl)
control_xgbmodel<- mlr::train(learner = setHyperPars(learner = xgb_learner,par.vals = tuned_params$x),task = task)

#making treatment effect estimates on train and test data:
train_tm$pred_T_xgb<-predict(treatment_xgbmodel, newdata=train_oh[ ,!(colnames(train_oh) == "treat")])$data[[2]]
train_tm$pred_C_xgb<-predict(control_xgbmodel, newdata=train_oh[ ,!(colnames(train_oh) == "treat")])$data[[2]]
train_tm$tau_xgb<-train_tm$pred_T_xgb-train_tm$pred_C_xgb

test_tm$pred_T_xgb<-predict(treatment_xgbmodel,newdata=test_oh[ ,!(colnames(test_oh) == "treat")])$data[[2]]
test_tm$pred_C_xgb<-predict(control_xgbmodel,newdata=test_oh[ ,!(colnames(test_oh) == "treat")])$data[[2]]
test_tm$tau_xgb<-test_tm$pred_T_xgb-test_tm$pred_C_xgb

#Evaluating Performance

perf_xgb=PerformanceUplift(data = test_tm, treat = "treat",
                          outcome = "y", prediction = "tau_xgb", equal.intervals = TRUE, nb.group = 10)

perf_xgb
barplot.PerformanceUplift(perf_xgb)

QiniArea(perf_xgb) 

# Plotting Qini curve and Qini Coeff on the test set - scrivere funzione che lo faccia 
# in automatico

df=data.frame(matrix(nrow=10, ncol=3))
df[,1]=perf_xgb[[1]]
df[,2]=round(perf_xgb[[6]],2)
df[,3]=round(perf_xgb[[7]],2)
colnames(df)=c("Dec", "num.incr", "perc.incr")
firstrow=numeric(3)
df=rbind(firstrow,df)

##Plot Qini curves
qini_curve1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (abs) - TM XGB", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                         linetype="dashed", size=0.5)
qini_curve1

qini_curve2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve (%) - TM XGB", y="Incr. Num. of Retained Customers", x="Perc. of Customers Targeted")+
  xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
                          linetype="dashed", size=0.5)
qini_curve2










# Intuition
# logit_model_inter<-glm(y ~ u_weekly_utilisation + u_rating_given + treat + u_weekly_utilisation*treat + u_rating_given*treat , family= binomial(link=logit), data=data)
# data_interaction=data
# data_interaction$pred= logit_model_inter %>% predict(data_interaction, type = "response")
# print('Estimated Probabilities')
# plot_ly(x=data_interaction$u_weekly_utilisation, y=data_interaction$u_rating_given, z=data_interaction$pred,  type="scatter3d", mode="markers", color=data_interaction$treat)
# print('Estimated Probabilities for treated and non treated customers')

### 3.1 Basic Single Model ####

#Creating a copy of the train and test sets
train_interuplift=train
test_interuplift=test

#Estimating the model
intermodel<-InterUplift(train, treat='treat', outcome='y', predictors=features, input = "all")

print(intermodel)
summary(intermodel)



#Extracting predictions for train data
pred_intermodel= predict(intermodel, train_interuplift, treat='treat')
train_interuplift$pred_intermodel=pred_intermodel

#Eveluating the model performance
perf_intermodel=PerformanceUplift(data = train_interuplift, treat = "treat",
                                  outcome = "y", prediction = "pred_intermodel", equal.intervals = TRUE, nb.group = 10)


perf_intermodel
barplot.PerformanceUplift(perf_intermodel)

QiniArea(perf_intermodel) 

# Test set

#Extracting predictions for test  data
pred_intermodel_test= predict(intermodel, test_interuplift, treat='treat')
test_interuplift$pred_intermodel=pred_intermodel_test

#Eveluating the model performance on the test set
perf_intermodel=PerformanceUplift(data = test_interuplift, treat = "treat",
                                  outcome = "y", prediction = "pred_intermodel", equal.intervals = TRUE, nb.group = 10)


perf_intermodel
barplot.PerformanceUplift(perf_intermodel)

QiniArea(perf_intermodel) 

# Plotting Qini curve and Qini Coeff on the test set - scrivere funzione che lo faccia 
# in automatico

df=data.frame(matrix(nrow=10, ncol=3))
df[,1]=perf_intermodel[[1]]
df[,2]=round(perf_intermodel[[6]],2)
df[,3]=round(perf_intermodel[[7]],2)
colnames(df)=c("Dec", "num.incr", "perc.incr")
firstrow=numeric(3)
df=rbind(firstrow,df)


# Plot Qini curves
qini_curve_1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve - SM (Logit)", y="Incr. Num. of Retained Cust", x="Perc. of Customers Targeted")+
  scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                         linetype="dashed", size=0.5)
qini_curve_1


qini_curve_2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve - SM (Logit)", y="Incr. % of Retained Cust", x="Perc. of Customers Targeted")+
  xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
               linetype="dashed", size=0.5)
qini_curve_2

# Qini area

QiniArea(perf_intermodel) 

## 3.2 Model selection ####


# Understanding the function LassoPath
# formula=intermodel[[23]]
# my_path=LassoPath(train_interuplift, formula)
# my_path_mod=LassoPath_mod(train_interuplift, formula)

#Finding the optimal set of features
# La funzione BestFeatures() non funziona con variabili categoriche inserite come fattori, e dunque vanno messe one hot encoded come per l'xgb
# Penso che per il momento la funzione non metta una categoria come baseline e dunque sputa il set di warnings che si vedono sotto (relativi alla multicollinearità delle featuers)
# Capire come può essere risolta questa cosa (provare a rimuovere una categoria per ogni variabili dela dataset oh, oppure modificare il source code della funzione affinché lavori con variabili
# categoriche fattorizzate).

inter_opt_feat=BestFeatures(data=data_oh, treat='treat', outcome='y', predictors=features_oh, rank.precision = 2, 
             equal.intervals = FALSE, nb.group = 10, 
             validation = TRUE, p = 0.4)

# La funzione BestFeatures va a trovare un set di features ottimo per il modello, utilizzando
# la cross validation (semplce train vs test) e considerando il Qini coeff. Unico problema è che parte dall'intero dataset ed utilizza 
# train e test set creati da zero (fa nuovamente lo split internamente). Noi abbiamo bisogno che utilizzi
# il train e il test set che abbiamo già creato e utilizzato negli altri modelli.
# Bisogna dunque modificare la funzione affinché lavori in questo modo.

# Writing the corresponding formula for the model
formula_final_inter=as.formula(paste("y~", paste(inter_opt_feat, collapse="+")))



###INTERUPLIFT FORMULA: una versione leggermente modificata della funzione InterUplift()
# che permette di stimare un modello partendo da una formula prespecificata dall'utente

InterUplift.formula <- function(formula, treat, data, ...){
  
  # Formula interface to InterUplift.
  if (!inherits(formula, "formula"))
    stop("Method is only for formula objects")
  
  mf <- match.call(expand.dots = FALSE)
  args <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, args)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval.parent(mf)
  
  Terms <- attr(mf, "terms")
  Terms <- names(attr(Terms,"dataClasses")) 
  
  if (length(intersect(treat,colnames(data))) == 0)
    stop("InterUplift: data does not include the control/treatment variable treat).")    
  
  outcome <- Terms[1]
  predictors <- Terms[-1]
  fit <- InterUplift(data=data, treat=treat, outcome=outcome, predictors=predictors, input = "all", ...)
  
  
  cl <- match.call()
  cl[[1]] <- as.name("InterUplift")
  fit$call <- cl
  
  return(fit)
}

# Train
final_intermodel_train<-InterUplift.formula(formula=formula_final_inter, treat="treat", data=train_oh) 
print(final_intermodel_train)
summary(final_intermodel_train)


pred_intermodel_final= predict(final_intermodel_train, train_interuplift, treat='treat')
train_interuplift$pred_intermodel_final=pred_intermodel_final
perf_intermodel_final=PerformanceUplift(data = train_interuplift, treat = "treat",
                                  outcome = "y", prediction = "pred_intermodel_final", equal.intervals = TRUE, nb.group = 10)

perf_intermodel_final
barplot.PerformanceUplift(perf_intermodel_final)
QiniArea(perf_intermodel_final) 


# Test

final_intermodel_test<-InterUplift.formula(formula=formula_final_inter, treat="treat", data=test_interuplift) 
print(final_intermodel_test)
summary(final_intermodel_test)


pred_intermodel_final= predict(final_intermodel_test, test_interuplift, treat='treat')
test_interuplift$pred_intermodel_final=pred_intermodel_final
perf_intermodel_final=PerformanceUplift(data = test_interuplift, treat = "treat",
                                        outcome = "y", prediction = "pred_intermodel_final", equal.intervals = TRUE, nb.group = 10)
perf_intermodel_final
barplot.PerformanceUplift(perf_intermodel_final)
QiniArea(perf_intermodel_final)


# Holdout - Per ora non lo abbiamo

# Plotting Qini curve and Qini Coeff on the test set - scrivere funzione che lo faccia 
# in automatico

df=data.frame(matrix(nrow=10, ncol=3))
df[,1]=perf_intermodel_final[[1]]
df[,2]=round(perf_intperf_intermodel_finalermodel[[6]],2)
df[,3]=round(perf_intermodel_final[[7]],2)
colnames(df)=c("Dec", "num.incr", "perc.incr")
firstrow=numeric(3)
df=rbind(firstrow,df)


# Plot Qini curves
qini_curve_1<-ggplot(df, aes(x=Dec, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve - SM Logit (opt)", y="Incr. Num. of Retained Cust", x="Perc. of Customers Targeted")+
  scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                         linetype="dashed", size=0.5)
qini_curve_1


qini_curve_2<-ggplot(df, aes(x=Dec, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
  mytheme+labs(title="Qini Curve - SM (Logit)", y="Incr. % of Retained Cust", x="Perc. of Customers Targeted")+
  xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
               linetype="dashed", size=0.5)
qini_curve_2
