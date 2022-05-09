rm(list = ls())
options(scipen = 999)
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
# Package for Logit Model and Various Utils 
library("tools4uplift")

# Importing personal functions
source('./functions.R')

# Defining a personalized theme for ggplot based on a default theme
mytheme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))



#### Data Preparation ####

# Importing already generated dataset
finale <- read.csv("./full_data_logit.csv")
df_comparison <- read.csv("./comparison_data.csv")



kde <- ggplot(finale) + 
  geom_density(aes(x=tau_interlogit), color='black', fill="#E20812" , alpha=0.7) +
  mytheme + labs(fill="", title='KDE of Estiamted Individual Treatment Effects', x='Estimated ITE')

kde


barplot <- ggplot(data=finale, aes(x=tau_interlogit)) +
  geom_histogram(position = 'identity', alpha=0.7, bins=30,  fill="#E20812", col='black' ) +
  mytheme +
  labs(fill="", title='Barplot of Estimated Individual Treatment Effects', x='Estimated ITE')

barplot


perf_intermodel <- PerformanceUplift(data = df_comparison, treat = "treat",
                                              outcome = "y", prediction = "tau_interlogit", equal.intervals = TRUE, nb.group = 10,
                                              rank.precision = 2)
perf_xgb <- PerformanceUplift(data = df_comparison, treat = "treat",
                                     outcome = "y", prediction = "tau_xgb", equal.intervals = TRUE, nb.group = 10,
                                     rank.precision = 2)
perf_hcf <- PerformanceUplift(data = df_comparison, treat = "treat",
                                     outcome = "y", prediction = "tau_hcf", equal.intervals = TRUE, nb.group = 10,
                                     rank.precision = 2)

barplot.PerformanceUplift(perf_intermodel)


perf_list <- list(perf_intermodel, perf_xgb, perf_hcf)

for(perf in perf_list){
  print(QiniArea(perf))}



# Plotting Qini Curves
qini_plots <- MultiQiniPlots(perf_list, names=c('InterLogit', 'XGB', 'HCF'))
qini_plots[[1]]
qini_plots[[2]]

uplift_plot <- MovingDifference(perf_list,  names=c('InterLogit', 'XGB', 'HCF'))
uplift_plot



MovingDifference_mod <- function(performance_models, names, ngroups=10){
  
  # Plot the difference in y between treated and untreated units against the % of user
  # base targeted starting from a list of performance objects of Tools4Uplift package
  # Args:
  #  performance_models: list of performance objects
  #  names: names of the models in the same order as they were listed
  #  ngroups: number of groups for computing performance
  # Return:
  #  plot: a plot showing how the estimated treatment effect changes 
  #        when we increase the % of population targeted
  
  df <- data.frame(matrix(nrow=ngroups, ncol=length(performance_models)+1))
  colnames(df) <- c('perc_target', names) 
  for (j in seq(1:ngroups)){
    df[j,1] <- round(j/ngroups, 2)}
  
  for (i in seq(1:length(performance_models))){
    for(j in seq(1:ngroups)){
      df[j, i+1] <- performance_models[[i]][[2]][j]/performance_models[[i]][[3]][j]-performance_models[[i]][[4]][j]/performance_models[[i]][[5]][j]
    }
  }
  
  df_perc_reshaped <- reshape2::melt(df, id.vars="perc_target", measure.vars=names)
  
  plot <- ggplot(data=df_perc_reshaped, aes(x=perc_target, y=value*100, colour=variable, group=variable)) +
    geom_point() + geom_line(size=0.7, color="#E20812") + labs(title="Uplift vs % Customer Base Targeted", fill='Model', color='Model',
                                      y='Uplift (pp)', x='Perc. of Customer Base Targeted')+mytheme+theme(legend.position="none")+scale_x_continuous(breaks=seq(0, 1, 0.1))+
                                       geom_segment(x = 0.1, y=8.9, xend=1, yend=8.9, color="black",
                                         linetype="dashed", size=0.5)
    
  
  
  return(plot)
}

perf_list <- list(perf_intermodel)

uplift_plot <- MovingDifference_mod(perf_list, names=c( 'InterLogit'))
uplift_plot



ggplot(finale, aes(x=as.factor(u_other_sub), y=tau_interlogit)) + 
  geom_boxplot(fill="#E20812", alpha=0.7) + 
  xlab("Subscription to Other Platforms")+labs(title="ITE Across Different Subscription Behaviours")+mytheme+
  ylab('Individual Treatment Effect')

ggplot(finale, aes(x=as.factor(u_genre_pref), y=tau_interlogit)) + 
  geom_boxplot(fill="#E20812", alpha=0.7) + 
  xlab("Occupation Category")+labs(title="ITE Across Different Occupations")+mytheme+
  ylab('Individual Treatment Effect')


ggplot(finale, aes(x=u_sub_utilisation, y=tau_interlogit)) + 
  geom_point(col="#E20812", alpha=0.7)+
  geom_smooth(method=lm)



ggplot(finale, aes(x=as.factor(u_age) )) +
  geom_bar(aes(), alpha=0.7, fill="#E20812")+ 
  xlab("Age Categories")+labs(title="Distribution of Users' Age")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")

ggplot(finale, aes(x=as.factor(u_gender) )) +
  geom_bar(alpha=0.7, fill="#E20812")+scale_fill_brewer(palette = "YlOrRd", direction = -1)+ 
  xlab("Gender Categories")+labs(title="Distribution of Users' Gender")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")

ggplot(data=finale, aes(x=u_rating_given)) +
  geom_histogram(position = 'identity', alpha=0.7, bins=30,  fill="#E20812", col='black' ) +
  mytheme +
  labs(fill="", title='Distribution of Ratings', x='Average Rating Given')

ggplot(finale, aes(x=u_format_pref)) +
  geom_bar(alpha=0.7, fill="#E20812")+scale_fill_brewer(palette = "YlOrRd", direction = -1)+ 
  xlab("Format Categories")+labs(title="Distribution of Users' Favorite Formats")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")

ggplot(finale, aes(x=u_occupation)) +
  geom_bar(alpha=0.7, fill="#E20812")+scale_fill_brewer(palette = "YlOrRd", direction = -1)+ 
  xlab("Occupation Categories")+labs(title="Distribution of Users' Occupation")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")

ggplot(finale, aes(x=as.factor(u_plan))) +
  geom_bar(alpha=0.7, fill="#E20812")+scale_fill_brewer(palette = "YlOrRd", direction = -1)+ 
  xlab("Subscription Plan Categories")+labs(title="Distribution of Users' Subscription Plan")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")

ggplot(finale, aes(x=as.factor(u_other_sub))) +
  geom_bar(alpha=0.7, fill="#E20812")+scale_fill_brewer(palette = "YlOrRd", direction = -1)+ 
  xlab("Subscription to Competing Platforms")+labs(title="Distribution of Users' Subscription Behaviour")+mytheme+
  ylab('Number of Users')+mytheme+theme(legend.position="none")




ResubPlots <- function(data, vars, target, treat){
  
  #Args:
  #   data: dataset for the analysis
  #   vars: list of categorical variables for which we need to 
  #   evaluate differences in resub rates
  #   target: name of target variable in the data
  #   treat: name of treatment variable in the data
  #   
  # Return:
  #   List of lists containing various graphs
  
  plot_list<-list()
  plot_uplift<-list()
  plot_groups<-list()
  
  
  for(j in seq(1, length(vars))){
    
    values <- unique(data[, vars[j]])
    data_plot <- data.frame(matrix(nrow=length(values), ncol=4))
    colnames(data_plot) <- c("category", "treated", "untreated", "uplift")
    k<-1
    
    for (i in seq(1, length(values))){
      data_plot[i,1]=values[i]
      data_sub_t=data[data[,vars[j]]==values[i]&data[,treat]==1,]
      data_sub_c=data[data[,vars[j]]==values[i]&data[,treat]==0,]
      data_plot[i,2]=round(as.numeric(table(data_sub_t[,target])/length(data_sub_t[,target]))[2],4)
      data_plot[i,3]=round(as.numeric(table(data_sub_c[,target])/length(data_sub_c[,target]))[2],4)
      data_plot[i,4]=data_plot[i,2]-data_plot[i,3]
    }
    
    data_plot$category <- factor(data_plot$category)
    pup <- ggplot(data=data_plot, aes(x=category, y=uplift)) +
      geom_bar(stat='identity', col='black', fill='#E50914', alpha=0.7)+labs(title=sprintf("Uplift Across %s Categories", vars[j]), 
                                                                  y="Uplift (%)", x=sprintf("%s Categories", vars[j]) )+mytheme
    plot_uplift[[j]]=pup
    
    data.m <- reshape2::melt(data_plot, id.vars='category')
    pgr=ggplot(data.m, aes(category, value)) + geom_bar(aes(fill = variable), 
                                                        width = 0.4, position = position_dodge(width=0.5), 
                                                        stat="identity") +
      labs(title=sprintf("Uplift Across %s Categories", vars[j]), 
            y="Perc. of  Resub", x=sprintf("%s Categories", vars[j]) ) +
            mytheme + scale_fill_manual(values = c("#E50914", "#000000", "#564d4d"))
    
    plot_groups[[j]]=pgr
    
  }
  
  plot_list[[1]]=plot_uplift
  plot_list[[2]]=plot_groups
  
  return(plot_list)
}


list_plot <- ResubPlots(data=finale, vars=c('u_age','u_occupation', 'u_plan', 'u_genre_pref', 'u_other_sub'), target='y', treat = 'treat')
ggarrange(plotlist = list_plot[[1]])


df_plot <- df_comparison %>% select(tau_xgb, tau_hcf, tau_interlogit)
data_melt <- melt(df_plot)

ggplot(data=data_melt, aes(x=value, group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.7) +mytheme+
  labs(fill="Model", title='KDE of Estiamted Individual Treatment Effects - Various Models', x='Estimated ITE')


plot1 <- ggplot(df_comparison) + 
  geom_density(aes(x=tau_interlogit), color='black', fill="#E20812" , alpha=0.7) +
  mytheme + labs(fill="", title='KDE of Estiamted ITE - Interlogit', x='Estimated ITE')
  
plot2 <- ggplot(df_comparison) + 
  geom_density(aes(x=tau_xgb), color='black', fill="#E20812" , alpha=0.7) +
  mytheme + labs(fill="", title='KDE of Estiamted ITE - XGB TM', x='Estimated ITE')
  
plot3 <- ggplot(df_comparison) + 
  geom_density(aes(x=tau_hcf), color='black', fill="#E20812" , alpha=0.7) +
  mytheme + labs(fill="", title='KDE of Estiamted ITE - HCF', x='Estimated ITE')

plotta <- list(plot1, plot2, plot3)

ggarrange(plotlist=plotta)

deciles <- c(seq(0.10, 1, 0.1))
newdf <- data.frame(deciles, perf_intermodel[[8]])
colnames(newdf) <- c('decile', 'uplift')

uplift1 <- ggplot(newdf, aes(x=as.factor(decile), y=uplift)) +
  geom_bar(stat='identity', alpha=0.7, fill="#E20812", col='#564d4d')+ 
  xlab("Perc. of Customer Base Targeted")+labs(title="Observed Uplift per Decile - InterLogit")+mytheme+
  ylab('Uplift (pp)')+mytheme+theme(legend.position="none")+geom_segment(x = 0.1, y=8.9, xend=10, yend=8.9, color="black",
                                                                         linetype="dashed", size=0.5)
uplift1

newdf <- data.frame(deciles, perf_xgb[[8]])
colnames(newdf) <- c('decile', 'uplift')

uplift2 <- ggplot(newdf, aes(x=as.factor(decile), y=uplift)) +
  geom_bar(stat='identity', alpha=0.7, fill="#E20812", col='#564d4d')+ 
  xlab("Perc. of Customer Base Targeted")+labs(title="Observed Uplift per Decile - XGB TM")+mytheme+
  ylab('Uplift (pp)')+mytheme+theme(legend.position="none")


newdf <- data.frame(deciles, perf_hcf[[8]])
colnames(newdf) <- c('decile', 'uplift')

uplift3 <- ggplot(newdf, aes(x=as.factor(decile), y=uplift)) +
  geom_bar(stat='identity', alpha=0.7, fill="#E20812", col='#564d4d')+ 
  xlab("Perc. of Customer Base Targeted")+labs(title="Observed Uplift per Decile - HCF")+mytheme+
  ylab('Uplift (pp)')+mytheme+theme(legend.position="none")

plotta2 <- list(uplift1, uplift2, uplift3)

ggarrange(plotlist=plotta2)

