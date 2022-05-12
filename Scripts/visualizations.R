rm(list = ls())
options(scipen = 999)
set.seed(10)

# Packages for Data Manipulation
library("dplyr")
library("tidyverse")
library("data.table")

# Packages for Graphs and Plots
library("ggplot2")
library("ggpubr")
library("gplots")
library("plotly")
library("ggrepel")
library("RColorBrewer")
# Defining a personalized theme for ggplot based on a default theme
mytheme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Package for Uplift Modeling
library("tools4uplift")

# Importing personal functions
source('./functions.R')

# Importing already generated dataset
df_comparison <- read.csv("./comparison_data.csv") #it includes the test set (3000 users) with the estimated TEs from each model (Intelogit, XGB, HCF)
finale <- read.csv("./full_data_logit.csv") #it includes the whole dataset (10000 users) with all the features and the estimated TEs of our final model (Interlogit)


# Inspecting the distribution of some of the features we have created
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


# Exploratory Data Anlysis to Assess the Presence of Heterogeneity in Treatment Effect across different groups
list_plot <- ResubPlots(data=finale, vars=c('u_age','u_occupation', 'u_plan', 'u_genre_pref', 'u_other_sub'), target='y', treat = 'treat')
ggarrange(plotlist = list_plot[[1]])


# Checking the distribution of Estimated Treatment Effects derived from our final model (Interlogit)

kde <- ggplot(finale) + 
  geom_density(aes(x=tau_interlogit), color='black', fill="#E20812" , alpha=0.7) +
  mytheme + labs(fill="", title='KDE of Estiamted Individual Treatment Effects', x='Estimated ITE')

kde


barplot <- ggplot(data=finale, aes(x=tau_interlogit)) +
  geom_histogram(position = 'identity', alpha=0.7, bins=30,  fill="#E20812", col='black' ) +
  mytheme +
  labs(fill="", title='Barplot of Estimated Individual Treatment Effects', x='Estimated ITE')

barplot


# Comparing the distributions of estimated TEs across different models

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

multiple_densities <- list(plot1, plot2, plot3)

ggarrange(plotlist=multiple_densities)


# Building Qini Curves for our three models (Intelogit, XGB and HCF)


perf_intermodel <- PerformanceUplift(data = df_comparison, treat = "treat",
                                              outcome = "y", prediction = "tau_interlogit", equal.intervals = TRUE, nb.group = 10,
                                              rank.precision = 2)
perf_xgb <- PerformanceUplift(data = df_comparison, treat = "treat",
                                     outcome = "y", prediction = "tau_xgb", equal.intervals = TRUE, nb.group = 10,
                                     rank.precision = 2)
perf_hcf <- PerformanceUplift(data = df_comparison, treat = "treat",
                                     outcome = "y", prediction = "tau_hcf", equal.intervals = TRUE, nb.group = 10,
                                     rank.precision = 2)


# Plotting Qini Curves
qini_plots <- MultiQiniPlots(perf_list, names=c('InterLogit', 'XGB', 'HCF')) #personal function
qini_plots[[1]]
qini_plots[[2]]

uplift_plot <- MovingDifference(perf_list,  names=c('InterLogit', 'XGB', 'HCF')) #personal function
uplift_plot

# Checking Qini Coefficients
perf_list <- list(perf_intermodel, perf_xgb, perf_hcf)

for(perf in perf_list){
  print(QiniArea(perf))}


# Plotting the cumulative uplift as a function of % of customer base targeted
perf_list <- list(perf_intermodel)
uplift_plot <- MovingDifference(perf_list, names=c( 'InterLogit')) #personal function
uplift_plot



# Plotting the uplift per decile 

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

uplift_per_decile <- list(uplift1, uplift2, uplift3)

ggarrange(plotlist=uplift_per_decile)


# Inspecting the relationship between the estimated treatment effects and other features of our customers 

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
