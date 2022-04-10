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



MultiQiniPlot <- function (models, names, ngroups=10) {
  
  #Plot multiple qini curves (abs and %) starting from a list of performance obejcts
  # of Tools4Uplift Package
  # Args
  # performance: list of performance objects
  # names: names of the models in the same order as they were listed
  # ngorups: number of groups for computing performance
  # modeltype: model type for filling up the title 
  
  #Creating a df for storing results (abs)
  
  df_abs = data.frame(matrix(nrow=ngroups, ncol=length(models)))
  colnames(df_abs)=c('perc_target', names) 
  for (i in seq(1:length(models)))
  performance = models[[i]]
  df_abs[,1]=performance[[1]]
  df_abs[,(1+i)]=round(performance[[6]],2)
  
  
  df_abs_reshaped = reshape2::melt(df_abs, id.vars="perc_target", measure.vars=names)
  
  multiple_qini_abs = ggplot(data=df_abs_reshaped, aes(x=perc_target, y=value, colour=variable, group=variable)) +
    geom_point() + 
    geom_line()
  
  
  #Creating a df for storing results (perc.)
  df_perc = data.frame(matrix(nrow=ngroups, ncol=length(models)))
  colnames(df_abs)=c('perc_target', names) 
  for (i in seq(1:length(models)))
    performance = models[[i]]
  df_perc[,1]=performance[[1]]
  df_perc[,(1+i)]=round(performance[[7]],2)
  
  
  df_perc_reshaped = reshape2::melt(df_abs, id.vars="perc_target", measure.vars=names)
  
  multiple_qini_perc = ggplot(data=df_perc_reshaped, aes(x=perc_target, y=value, colour=variable, group=variable)) +
    geom_point() + 
    geom_line()
  
  plot_list=list(multiple_qini_abs, multiple_qini_perc)
  
  return(plot_list)
  
}
  
  
