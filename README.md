# Uplift Modeling and Retention - The Simulated Case of a Video Streaming Company

## Contents of the Repo
In the <b> Scripts </b> folder you can find:
- `data_generation.R`: a script that generates our dataset of customers
- `data_modeling.R`: a script that estimates and compares various uplift models
- `visualizations.R`: a script that is used for some data visualization tasks 
- `functions.R`: a module containing several personalized functions used in the previous scripts. <br>

In the <b> Datasets </b> folder you can find: 
- `users.csv`: a dataset of 10,000 users, which is the output of our data generation process
- `comparison_data.csv`: a dataset of 3,000 users (i.e. our test set) containing the estimated individual treamtent effects from three different uplift models
- `full_data_logit.csv`: a dataset where an additional column containing the estimated individual treatment effect from our final model is added to `users.csv`. 

## Description of our Project
In this project, we applied some uplift modeling techniques to the fictitious case of a video streaming company which is assumed to A/B tests a campaign for improving retention. The intervention consists in a discount voucher applicable to the next resubscription fee and is assigned randomly to some users. From the company standpoint, it would be valuable to learn who are those users for whom being given a voucher significantly impacts on their probability of resubscription (the so-called <b> persuadables </b>) and to distinguish them from users who would resubscribe in any case to the service (the so-called <b> sure-things </b>). As a matter of fact, it would make possible to optimize on the implementation of the retention campaign in the long run.  Uplift models can be exploited to discriminate between these groups of customers, because they provide <b> estimates of the treatment effect at the individual level </b>. By doing so,  they overcome  the limitations  associated to the simple A/B test setting, which instead estimates the <i> average </i> treatment effect.

First of all, we created a dataset of 10,000 users which were assumed to be the target of the A/B test the company carried out to assess the effect of its retention campaing (`data_generation.R` ). For each user, we first generated individual features which described some demographics (e.g. age, occupaton) and some behaviours related to service (e.g. rating given, time spent watching videos). Then, the intervention was randomly assigned. Finally, in order to assign resubscription, we built a user-specific utility score. The final output of this stage was a dataframe containing 10,000 rows and 13 columns (`users.csv`). This is what data analyst at our fictitious video streaming company were expected to observe in their database at the end of the A/B test.

At this point, we put ourselves in their shoes and we applied some uplift modeling techniques in order to estimate individual treatment effects. Specifically, we tested a two model approach based on XGBoost, a logistic regression with interaction terms on the treatment (Interlogit), and an Honest Causal Forest (see `data_modeling.R`). After having compared the performance of these models, we analyzed the results of our final model with the aid of various visualizations (`visualizations.R`), trying to derive some useful insights for the business.
