# Uplift Modeling and Customer Retention - The Simulated Case of a Video Streaming Company

## Contents of the Repo
In the `Scripts` folder you can find:
- `data_generation.R`: a script that generates our dataset of currently active customers
- `data_modeling.R`: a script that estimates and compares various uplift models
- `visualizations.R`: a script that is used for some data visualization tasks 
- `functions.R`: a module containing several personalized functions imported and used in the scripts. <br>

In the `Datasets` folder you can find: 
- `users.csv`: a dataset of 10,000 users (total sample), which is the output of our data generation process from `data_generation.R`.
- `comparison_data.csv`: a dataset of 3,000 users (i.e. our test set) containing the estimated individual treamtent effects from three different uplift models
- `full_data_logit.csv`: a dataset where a column containing the estimated individual treatment effect given by our final chosen model is added to `users.csv`. 

## Project Description
In this project, we applied some uplift modeling techniques to the fictitious case of a video streaming company that wants to apply A/B testing to a campaign for improving customer retention. The intervention consists in a discount voucher applicable to the next subscription fee and it is assigned randomly to 50% of the total users under study. From the company standpoint, it would be valuable to learn who are those users for whom, being given a voucher, significantly improves their probability of resubscription (the so-called <b>persuadables</b>) and to distinguish these users from those who would resubscribe in any case to the service (the so-called <b>sure-things</b>). As a matter of fact, understanding this would make possible to optimize the implementation of the retention campaign in the long run.  Uplift models can be exploited to discriminate between these groups of customers, because they provide estimates of the treatment effect at the <i>individual level</i>. By doing so, they overcome the limitations associated to the simple A/B test setting, which instead estimates the <i>average</i> treatment effect.

First of all, we created a dataset of 10,000 users which were assumed to be the target of the A/B test that the company carried out to assess the effect of its retention campaing (`data_generation.R`). For each user, we first generated individual features which described some demographics (e.g. age, occupaton, etc.) and some behaviours related to the service (e.g. rating given, time spent watching videos, etc.). After that, the intervention was randomly assigned to half of them so that the individuals in the treatment group and those in the control group woulb be similar. Finally, we assessed users' resubscription based on a user-specific utility score. The final output of this stage was a dataframe containing 10,000 rows and 13 columns (`users.csv`). This is what data analysts at our fictitious video streaming company were expected to observe when asked to carry out the analysis.

At this point, we applied some uplift modeling techniques in order to estimate individual treatment effects. Specifically, we tested a two-model approach based on <b>XGBoost</b>, a logistic regression with interaction terms on the treatment variable (<b>Interlogit</b>), and an <b>Honest Causal Forest</b> (see `data_modeling.R`). After having compared the performance of these models, we analyzed the results of our final model with the aid of various visualizations (`visualizations.R`), trying to derive some useful insights for the business.

## References
* https://towardsdatascience.com/a-quick-uplift-modeling-introduction-6e14de32bfe0
* https://proceedings.mlr.press/v67/gutierrez17a/gutierrez17a.pdf
* https://towardsdatascience.com/causal-machine-learning-for-econometrics-causal-forests-5ab3aec825a7
* https://arxiv.org/pdf/1901.10867.pdf
* https://www.markhw.com/blog/causalforestintro





<sub>(Credits: This is part of the project for the course [Causal Inference for Marketing Policies](https://didattica.unibocconi.it/ts/tsn_anteprima.php?cod_ins=20757&anno=2022&ric_cdl=x&IdPag=6618) at Bocconi University) </sub>
