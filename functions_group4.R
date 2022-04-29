# This module includes the functions we used for our group project and should
# be imported before running our code


#### FUNCTIONS FOR DATA GENERATION ####

#Function allowing to generate data from a truncated normal distribution
rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
  bounds <- pnorm(c(min, max), mean, sd)
  u <- runif(n, bounds[1], bounds[2])
  qnorm(u, mean, sd)
}




#### FUNCTIONS FOR EXPLORATORY DATA ANALYSIS

# Function for creating plot showing different resub rates across the 
# different categories of a categorical variable
ResubPlots = function(data, vars, target, treat){
  
  #Args:
  #   data: dataset for the analysis
  #   vars: list of categorical variables for which we need to 
  #   evaluate differences in resub rates
  #   target: name of target variable in the data
  #   treat: name of treatment variable in the data
  #   
  # Returns:
  #   List of lists containing various graphs
  
  plot_list=list()
  plot_uplift=list()
  plot_groups=list()
  
  # TODO aggiungere labels
  
  
  for(j in seq(1, length(vars))){
    
    values=unique(data[, vars[j]])
    data_plot=data.frame(matrix(nrow=length(values), ncol=4))
    colnames(data_plot)=c("category", "treated", "untreated", "uplift")
    k=1
    
    for (i in seq(1, length(values))){
      data_plot[i,1]=values[i]
      data_sub_t=data[data[,vars[j]]==values[i]&data[,treat]==1,]
      data_sub_c=data[data[,vars[j]]==values[i]&data[,treat]==0,]
      data_plot[i,2]=round(as.numeric(table(data_sub_t[,target])/length(data_sub_t[,target]))[2],4)
      data_plot[i,3]=round(as.numeric(table(data_sub_c[,target])/length(data_sub_c[,target]))[2],4)
      data_plot[i,4]=data_plot[i,2]-data_plot[i,3]
    }
    
    data_plot$category=factor(data_plot$category)
    pup=ggplot(data=data_plot, aes(x=category, y=uplift)) +
      geom_bar(stat='identity')+labs(title=sprintf("Differences in Uplift Across %s Categories", vars[j]), 
                                     y="Diff. in Resub Rate between Treated and Untreated", x=sprintf("%s Categories", vars[j]) )
    plot_uplift[[j]]=pup
    
    data.m <- reshape2::melt(data_plot, id.vars='category')
    pgr=ggplot(data.m, aes(category, value)) + geom_bar(aes(fill = variable), 
                                                        width = 0.4, position = position_dodge(width=0.5), stat="identity") +
      labs(title=sprintf("Differences in Resub Rates Across %s Categories", vars[j]), 
           y="Perc. of Resubscribing Users", x=sprintf("%s Categories", vars[j]) )
    #                                         + scale_x_discrete(labels = labels[[j]])
    
    
    plot_groups[[j]]=pgr
    
  }
  
  plot_list[[1]]=plot_uplift
  plot_list[[2]]=plot_groups
  
  return(plot_list)
}


#### FUNCTIONS FOR ESTIMATING MODELS ####


  

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
  

BestFeatures_mod <- function ( train, test, treat, outcome, predictors, rank.precision = 2, path, 
                               equal.intervals = FALSE, nb.group = 10) {
     #  This functions was adapted from the package Tools4Uplift (Belbahri, 2019).
     #  From a theortical standpoint there are no differences. Practically, 
     #  we have twisted a couple of things for a better fit with our needs (for example 
     #  starting with already split train and test instead of creating them inside 
     #  the function)
     #  Args:
     #  train: train set
     #  test: test set
     #  path: Lasso Path created by using the dedicated function
     #  All the other arguments are the same as in the original version of the 
     #  function
     #   
     # Returns:
     #  A list with the estimated logit model (on the train set) and an array of
     #  estimated individual treatment effects for observations in the test set.
  
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







#### FUNCTIONS FOR DIAGNOSTICS ####


QiniPlot <- function (performance, modeltype, ngroups=10) {
  
  # Plot qini curves (abs and %) starting from performance obejcts
  # of Tools4Uplift Package
  # Args
  # performance: performance object for estimating Qini Curves
  # ngorups: number of groups for computing performance
  # modeltype: model type for filling up the title 
  #    
  # Returns:
  # List containing two qini curves (abs and relative)
  
  
  df=data.frame(matrix(nrow=ngroups, ncol=3))
  df[,1]=performance[[1]]
  df[,2]=round(performance[[6]],2)
  df[,3]=round(performance[[7]],2)
  colnames(df)=c("perc_target", "num.incr", "perc.incr")
  firstrow=numeric(3)
  df=rbind(firstrow,df)
  
  
  
  # Plot Qini curves
  qini_curve_1<-ggplot(df, aes(x=perc_target, y=num.incr))+geom_point(color="blue")+geom_line(color="blue")+
    mytheme+labs(title=sprintf("Qini Curve (abs) - %s", modeltype), y="Incr. Numb. of Resub. Cust.", x="Perc. of Customers Targeted")+
    scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df[11,2], color="red", 
                                                           linetype="dashed", size=0.5)
  
  qini_curve_2<-ggplot(df, aes(x=perc_target, y=perc.incr))+geom_point(color="blue")+geom_line(color="blue")+
    mytheme+labs(title=sprintf("Qini Curve (perc.) - %s", modeltype), y="Incr. % of Resub. Cust.", x="Perc. of Customers Targeted")+
    xlim(0, 1)+geom_segment(x = 0, y=0, xend=1, yend=df[11,3], color="red", 
                            linetype="dashed", size=0.5)
  
  plot_list=list(qini_curve_1, qini_curve_2)
  
  return(plot_list)
}



MultiQiniPlots <- function (performance_models, names, ngroups=10) {
  
  #Plot multiple qini curves (abs and %) starting from a list of performance objects
  # of Tools4Uplift Package
  # Args
  # performance_models: list of performance objects
  # names: names of the models in the same order as they were listed
  # ngorups: number of groups for computing performance
  # Return
  # plot_list = a list of Qini Plots
  
  
  df = data.frame(matrix(nrow=ngroups, ncol=length(performance_models)+1))
  colnames(df)=c('perc_target', names) 
  for (j in seq(1:ngroups)){
    df[j,1]=round(j/ngroups, 2)}
  
  for (i in seq(1:length(performance_models))){
    performance = performance_models[[i]]
    df[,(1+i)]=round(performance[[6]],2)
    emptyrow = data.frame(matrix(0, nrow=1, ncol=length(performance_models)+1))
    colnames(emptyrow)=c('perc_target', names) 
    df_abs = rbind(emptyrow, df)
    
    
  }
  
  
  df_abs_reshaped = reshape2::melt(df_abs, id.vars="perc_target", measure.vars=names)
  
  multiple_qini_abs = ggplot(data=df_abs_reshaped, aes(x=perc_target, y=value, colour=variable, group=variable)) +
    geom_point() + 
    geom_line()+labs(title="Qini Curve - Comparison of Models", color='Model',
                     y="Incr. Numb. of Resub. Cust.", x="Perc. of Customer Base Targeted")+
    scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df_abs[ngroups,2], color="red", 
                                                           linetype="dashed", size=0.5)
  
  
  df_perc = data.frame(matrix(nrow=ngroups, ncol=length(performance_models)+1))
  colnames(df)=c('perc_target', names) 
  for (j in seq(1:ngroups)){
    df[j,1]=round(j/ngroups, 2)}
  
  for (i in seq(1:length(performance_models))){
    performance = performance_models[[i]]
    df[,(1+i)]=round(performance[[7]],2)
  }
  
  emptyrow = data.frame(matrix(0, nrow=1, ncol=length(performance_models)+1))
  colnames(emptyrow)=c('perc_target', names) 
  df_perc = rbind(emptyrow, df)
  
  
  df_perc_reshaped = reshape2::melt(df_perc, id.vars="perc_target", measure.vars=names)
  
  multiple_qini_perc = ggplot(data=df_perc_reshaped, aes(x=perc_target, y=value, colour=variable, group=variable)) +
    geom_point() + 
    geom_line()+labs(title="Qini Curve - Comparison of Models", color='Model',
                     y="Incr. Perc.  Numb. of Resub. Cust.", x="Perc. of Customer Base Targeted")+
    scale_x_continuous(breaks=seq(0, 1, 0.1))+geom_segment(x = 0, y=0, xend=1, yend=df_perc[ngroups,2], color="red", 
                                                           linetype="dashed", size=0.5)
  
  plot_list=list(multiple_qini_abs, multiple_qini_perc)
  
  return(plot_list)
  
}



MovingDifference<-function(performance_models, names, ngroups=10){
  
  # Plot the difference in y between treated and untreated units against the 
  # percentage of user base targeted starting from a list of performance objects
  # of Tools4Uplift Package
  # Args
  # performance_models: list of performance objects
  # names: names of the models in the same order as they were listed
  # ngrups: number of groups for computing performance
  # Return
  # plot = a plot showing how the estimated treatment effect changes 
  #       when we increase the % of population targeted
  
  df = data.frame(matrix(nrow=ngroups, ncol=length(performance_models)+1))
  colnames(df)=c('perc_target', names) 
  for (j in seq(1:ngroups)){
    df[j,1]=round(j/ngroups, 2)}
  
  for (i in seq(1:length(performance_models))){
    for(j in seq(1:ngroups)){
      df[j, i+1]=performance_models[[i]][[2]][j]/performance_models[[i]][[3]][j]-performance_models[[i]][[4]][j]/performance_models[[i]][[5]][j]
    }
  }
  
  df_perc_reshaped = reshape2::melt(df, id.vars="perc_target", measure.vars=names)
  
  plot  = ggplot(data=df_perc_reshaped, aes(x=perc_target, y=value*100, colour=variable, group=variable)) +
    geom_point() + geom_line() + labs(title="ATE vs % Customer Base Targeted", fill='Model', color='Model',
                                      y='ATE (pp)', x='Perc. of Customer Base Targeted')
  
  
  return(plot)
}









