# Scrivere una funziona che data una lista di features opera nel seguente modo: 
# Per ognuna delle categorie della variabile crea un dataset con le 
# frequenze relative e crea una plot


ResubPlots = function(data, vars, target, treat){

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
    data_plot[i,2]=round(as.numeric(table(data_sub_t[,target])/length(data_sub_t[,target]))[2],2)
    data_plot[i,3]=round(as.numeric(table(data_sub_c[,target])/length(data_sub_c[,target]))[2],2)
    data_plot[i,4]=data_plot[i,2]-data_plot[i,3]
  }
  
  data_plot$category=factor(data_plot$category)
  pup=ggplot(data=data_plot, aes(x=category, y=uplift)) +
    geom_bar(stat='identity')+labs(title=sprintf("Differences in Uplift Across %s Categories", vars[j]), 
                                   y="Diff. between RR between Treated and Untreated", x=sprintf("%s Categories", vars[j]) )
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


list_plot=ResubPlots(data=data, vars=c('u_age', 'u_occupation'), target='resub', treat = 'treated')

list_plot[[2]][[2]]