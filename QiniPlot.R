QiniPlot <- function (performance, modeltype, ngroups=10) {
  
  #Plot qini curves (abs and %) starting from performance obejcts
  # of Tools4Uplift Package
  # Args
  # performance: performance object for estimating Qini Curves
  # ngorups: number of groups for computing performance
  # modeltype: model type for filling up the title 
  
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
  # modeltype: model type for filling up the title 
  
  #Creating a df for storing results (abs)
  
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
  
# Plot multiple qini curves (abs and %) starting from a list of performance objects
# of Tools4Uplift Package
# Args
# performance_models: list of performance objects
# names: names of the models in the same order as they were listed
# ngorups: number of groups for computing performance
# modeltype: model type for filling up the title 

#Creating a df for storing results (abs)

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

performance_models=list(perf_intermodel_opt, perf_intermodel_opt_pen, perf_intermodel_baseline)
myplot<-MovingDifference(performance_models, names=c('baseline', 'FSNP', 'FSP'), ngroups=10)
myplot

mylist<-MultiQiniPlots(performance_models, names=c('baseline', 'FSNP', 'FSP'), ngroups=10)
mylist[[2]]

