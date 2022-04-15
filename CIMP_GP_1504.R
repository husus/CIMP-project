rm(list = ls())
options(scipen=999)
set.seed(10)

# Packages for Data Manipulation 
library("dplyr")
library('tidyverse')
library("data.table")

# Stuff for Graphs and Plots
library("ggplot2")
library("ggpubr")
library("gplots")
library("plotly")
library("ggrepel")
library("RColorBrewer")


##I define a personalized theme for ggplot based on a default theme
mytheme<-theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

# General Tools for Regressions and Marginal Effects
library("car")
library("margins")

# Package for regression uplifting models
library("tools4uplift")

# Package for XGBoosting
library('xgboost')

# Package for HCF
library("grf")

# Miscellaneous
library("matrixStats")
library("reshape2")
library("Rcpp")
library('mltools')
library('glmnet')
library('caret')
library("mlr")


#Automatically Install Missing Packages
listOfPackages <- c("dplyr","tidyverse","data.table","ggplot2","ggpubr",
                    "gplots","plotly","ggrepel","RColorBrewer",
                    "reshape2","margins", "tools4uplift","xgboost","grf","ggplot2","matrixStats",
                    "Rcpp","car","mltools","glmnet",
                    "caret","mlr")
for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
    library(i)
  }
}


# total number of users under study - Increased from 1000 to 100000 (experiment)
num_users <- 10000

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


USERS

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
# data_oh = one_hot(as.data.table(data_cat))

dummy = dummyVars(" ~ .", data=data_cat)
newdata = data.frame(predict(dummy, newdata = data_cat))

#rimuovere il punto dal nome (sar? utile per successivi algoritmi)
for (k in 1:ncol(newdata)) {
  splitted_str=strsplit(colnames(newdata)[k], ".", fixed = TRUE)
  colnames(newdata)[k]=paste(splitted_str[[1]][1],splitted_str[[1]][2], sep = "", collapse = NULL)
}

data_oh=cbind(data$u_id, newdata, data_noncat)
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


#### TRADITIONAL A/B TESTING

basic_model_ab = lm(y ~ treat, data = data)
summary(basic_model_ab)


#Even when adding all the regressors

formula_abtest = as.formula(paste("y~ treat+", paste(features,collapse="+")))

adv_model_ab = lm(formula_abtest, data = data)
summary(adv_model_ab)

###############################################################################
### 3. SINGLE MODEL WITH INTERACTION ----
###############################################################################

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


InterModel.logit <- function ( formula, train, test, treat, outcome, lambda=NULL) {
  
  # Args:
  #   formula: formula for estimating the logit model on the train set
  #   train: train set
  #   test: test set
  #   treat: treatment
  #   outcome: our outcome wrt the treatment effect is evaluated
  #   
  # Returns:
  #   A list with the non penalized logit model estimated the train set,
  #  the penalized logit model estimated on the train set if lambda is provided,
  # and two arrays of estimated individual treatment effects for observations in the test set.
  
  logitmodel=glm(formula, train, family=binomial(link=logit))
  
  data1 <- test
  data1['treat'] <- 1
  pred_y1_ct1 <- predict.glm(logitmodel, newdata=data1, type="response")
  
  data0 <- test
  data0['treat'] <- 0
  pred_y1_ct0 <- predict.glm(logitmodel, newdata=data0, type="response")
  
  est_tau<- pred_y1_ct1 - pred_y1_ct0
  
  if( is.null(lambda)){
    output=list(logitmodel, est_tau)
  }
  else{
    
    X <- model.matrix(formula, train)
    Y <- model.frame(formula, train)[, 1]
    logitmodel_pen <- glmnet(X, Y, alpha = 1, family = "binomial",
                             lambda = lambda)
    
    X_test1 <- model.matrix(formula, data1)
    pred_y1_ct1_pen <- predict(logitmodel_pen, newx=X_test1, type='response')
    
    
    X_test0 <- model.matrix(formula, data0)
    pred_y1_ct0_pen <- predict(logitmodel_pen, newx=X_test0, type='response')
    
    est_tau_pen <- pred_y1_ct1_pen - pred_y1_ct0_pen
    
    output=list(logitmodel, est_tau, logitmodel_pen, est_tau_pen)
  }
  
  
  
  return(output)
  
}



#Defining the formula for our model
outcome='y'
treat='treat'

treat_formula <- c()
for (k in seq(1:length(features))) {
  treat_formula <- paste(treat_formula, paste(features[k], treat, sep = ":"), sep="+")
}

baseline_formula <- as.formula(paste(paste(outcome, "~", treat, "+"),paste(features,collapse="+"),treat_formula))

#Estimating the model
inter_baseline_output<-InterModel.logit(baseline_formula, train_interuplift, test_interuplift, treat='treat', outcome='y')

intermodel_baseline_model=inter_baseline_output[[1]]
print(intermodel_baseline_model)
summary(intermodel_baseline_model)


#Eveluating the model performance on Test set
test_interuplift$pred_intermodel_baseline=inter_baseline_output[[2]]

perf_intermodel_baseline=PerformanceUplift(data = test_interuplift, treat = "treat",
                                           outcome = "y", prediction = "pred_intermodel_baseline", equal.intervals = TRUE, nb.group = 10)

perf_intermodel_baseline
barplot.PerformanceUplift(perf_intermodel_baseline)

QiniArea(perf_intermodel_baseline) 
QiniArea(perf_intermodel_baseline, adjusted = TRUE) 

# Plotting Qini curve and Qini Coeff on the test set - scrivere funzione che lo faccia 
# in automatico

QiniPlot <- function (performance, modeltype) {
  
  #Plot qini curves (abs and %) starting from performance obejcts
  # of Tools4Uplift Package
  # Args
  # performance: performance object for estimating Qini Curves
  # modeltype: model type for filling up the title 
  
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

plot_list=QiniPlot(performance=perf_intermodel_baseline, modeltype = 'SM-Logit (Baseline)')

plot_list[[1]]
plot_list[[2]]


## 3.2 Model selection ####

my_path=LassoPath(train_interuplift, baseline_formula)

BestFeatures_mod <- function ( train, test, treat, outcome, predictors, rank.precision = 2, path, 
                               equal.intervals = FALSE, nb.group = 10) {
  # path=my_path
  # train=train_oh
  # test=test_oh
  # treat='treat'
  # outcome='y'
  # predictors=features_oh
  # rank.precision = 2
  # equal.intervals = FALSE
  # nb.group = 10
  
  path <- path[!duplicated(path[,"dimension"]), ]
  # Keep paths of dimension > 0
  path <- path[path[, "dimension"] > 0, ]
  lambda.qini <- c()
  for (k in 1:nrow(path)) {
    features <- path[k, -c(1, 2)]
    # Keep features with non zero estimates only
    features <- features[features != 0]
    
    # Fit the logistic regression model with selected features only
    form<-as.formula(paste("y~", paste(names(features), collapse="+")))
    lambda.model=glm(formula=form, data=train, family=binomial(link=logit))
    data1 <- test; data1[treat] <- 1
    pr.y1_ct1 <- predict.glm(lambda.model, newdata=data1, type="response")
    
    data0 <- test; data0[treat] <- 0
    pr.y1_ct0 <- predict.glm(lambda.model, newdata=data0, type="response")
    
    test$lambda.pred <- pr.y1_ct1 - pr.y1_ct0
    
    #test$lambda.pred <-predict.InterUplift(lambda.model, test, treat)
    
    lambda.perf <- PerformanceUplift(test, treat, outcome, 
                                     "lambda.pred", 
                                     rank.precision = rank.precision, 
                                     equal.intervals = equal.intervals, 
                                     nb.group = nb.group)
    if (length(lambda.perf[[1]]) == 1) { lambda.qini[k] <- 0}
    else {lambda.qini[k] <- QiniArea(lambda.perf)}
    
  }
  
  best.model <- cbind(path[, c(1, 2)], lambda.qini)
  
  if (max(best.model[,3]) == 0) { 
    warning("All models result in a Qini coefficient equal to 0. Please check LassoPath().")
  }
  # Take the model that maximizes the qini coefficient
  max.qini <- which.max(best.model[,3])
  best.model <- best.model[max.qini,]
  best.lambda<- best.model['lambda']
  best.dim<- best.model['dimension']
  
  
  # We also need to know which variables were selected
  best.features <- path[path[, "lambda"] == best.model["lambda"], -c(1, 2, 3)]
  best.features <- names(best.features[best.features != 0])
  class(best.features) <- "BestFeatures"
  output=list(best.features, best.lambda, best.dim)
  return(output)
}


# inter_opt_feat_new=BestFeatures_mod(train_oh, test_oh, treat='treat', outcome='y', predictors=features_oh, rank.precision = 2, path=my_path, 
#                           equal.intervals = TRUE, nb.group = 10)

best_feat_output=BestFeatures_mod(train_oh, test_oh, treat='treat', outcome='y', predictors=features_oh, rank.precision = 2, path=my_path,
                                  equal.intervals = TRUE, nb.group = 10)

#Extracting best features
best_features=best_feat_output[[1]]
best_lambda=best_feat_output[[2]]

# Using the best features for writing the formula for our final model
best_feat_formula=as.formula(paste("y~", paste(best_features, collapse="+")))

inter_opt_output=InterModel.logit(best_feat_formula, train_oh, test_oh, treat='treat', outcome='y', lambda = best_lambda)
intermodel_opt_model=inter_opt_output[[1]]
intermodel_opt_model_pen=inter_opt_output[[3]]
summary(intermodel_opt_model)
summary(intermodel_opt_model_pen)

test_interuplift$pred_intermodel_opt=inter_opt_output[[2]]
test_interuplift$pred_intermodel_opt_pen=inter_opt_output[[4]]

summary(test_interuplift$pred_intermodel_opt_pen)


perf_intermodel_opt <- PerformanceUplift(data=test_interuplift, treat='treat', outcome='y', 
                                         prediction="pred_intermodel_opt", 
                                         rank.precision = 2, 
                                         equal.intervals = TRUE, 
                                         nb.group = 10)

perf_intermodel_opt_pen <- PerformanceUplift(data=test_interuplift, treat='treat', outcome='y', 
                                             prediction="pred_intermodel_opt_pen", 
                                             rank.precision = 2, 
                                             equal.intervals = TRUE, 
                                             nb.group = 10)

perf_intermodel_opt
barplot.PerformanceUplift(perf_intermodel_opt)
QiniArea(perf_intermodel_opt) 
QiniArea(perf_intermodel_opt, adjusted = T) 

plot_list=QiniPlot(perf_intermodel_opt, modeltype = 'SM-Logit (Opt.)')
plot_list[[1]]
plot_list[[2]]




