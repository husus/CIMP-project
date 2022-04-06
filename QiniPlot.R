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