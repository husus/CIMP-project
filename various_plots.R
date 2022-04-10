# Questo script produce dei graifici per l'anlisi finale

#Input: dataset con features e ITE stimati dai vari modelli prodotti


library(ggplot2)

# Defining a personalized theme
mytheme<-theme_minimal()+theme(plot.title = element_text(hjust = 0.5))



#Creating fake data just to make sure that plots are working properly
pred_twomodel_logit = rnorm(1000, 0, 1)
pred_twomodel_xgb = rnorm(1000, 0.2, 0.9)
pred_single_logit = rnorm(1000, 0.4, 0.7)
pred_HCF = rnorm(1000, -0.3, 1.3)
x1=rnorm(1000, 0, 1)
x2=rnorm(1000, 0, 1)
x3=factor(sample(size=1000, x=c(1,2,3,4), prob =   c(0.2, 0.4, 0.3, 0.1), replace=T))
x4=factor(sample(size=1000, x=c(1,2), prob =  c(0.7,0.3), replace=T))

data_fake = data.frame(pred_twomodel_logit, pred_twomodel_xgb, pred_single_logit, pred_HCF, x1, x2, x3, x4) 
data_fake_reorg = data.frame(x = c(pred_twomodel_logit, pred_twomodel_xgb, pred_single_logit, pred_HCF), 
                             x1 = c(x1, x1, x1, x1), 
                             x2 = c(x2, x2, x2, x2), x3 = c(x3, x3, x3, x3),
                             x4 = c(x4, x4, x4, x4),
                            type=rep(c("TM - Logit", "TM - Xgb", 'SM - Logit', 'HCF'), c(length(pred_twomodel_logit), length(pred_twomodel_xgb),
                                                                                         length(pred_single_logit), length(pred_HCF))))
data_fake_reorg$type=as.factor(data_fake_reorg$type)

#### HISTOGRAM and KDE of estimated ITE #####

# First plot: histograms of ITE estimated by different models

kde = ggplot(data_fake_reorg) + 
  geom_density(aes(x=x, fill=type), color='black', alpha=0.3) +
  mytheme + labs(fill="", title='KDE of Estiamted ITE', x='Estimated ITE')

kde


# TODO: capire come regolare il colore dei bordi senza che appaia in legenda

barplot = ggplot(data=data_fake_reorg, aes(x=x, fill=type)) +
  geom_histogram(position = 'identity', alpha=0.25, bins=30) +
  mytheme +
  labs(fill="", title='Distribution of Estiamted ITE', x='Estimated ITE')

barplot


#Compute correlation between predicted ITE

#### Plots for exploring how estimates vary with some features across models

reg1 <- ggplot(data_fake_reorg, aes(x=x1, y=x, shape=type))+
  geom_point(aes(col=type), alpha=0.2)+mytheme

reg1+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                 aes(color=type))+labs(fill="", title='Relations Between Est. ITE and X1', x='X1', y='Est. ITE') 


# TODO: Capire come mai non mi splitta i gruppi 

boxplot_1 <- ggplot(data_fake_reorg, aes(x3, x))+
  geom_boxplot(aes(fill=type), alpha=0.2)+mytheme+labs(fill="", title='Relations Between Est. ITE and X3', x='X3', y='Est. ITE') 

boxplot_1








