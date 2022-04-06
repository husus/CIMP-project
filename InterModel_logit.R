
InterModel.logit <- function ( formula, train, test, treat, outcome ) {
  
  # Args:
  #   formula: formula for estimating the logit model on the train set
  #   train: train set
  #   test: test set
  #   treat: treatment
  #   outcome: our outcome wrt the treatment effect is evaluated
  #   
  # Returns:
  #   A list with the estimated logit model (on the train set) and an array of
  # estimated individual treatment effects for observations in the test set.
  
  logitmodel=glm(formula, train, family=binomial(link=logit))
  
  data1 <- test
  data1['treat'] <- 1
  pred_y1_ct1 <- predict.glm(logitmodel, newdata=data1, type="response")
  
  data0 <- test
  data0['treat'] <- 0
  pred_y1_ct0 <- predict.glm(logitmodel, newdata=data0, type="response")
  
  est_tau<- pred_y1_ct1 - pred_y1_ct0
  output=list(logitmodel, est_tau)
  return(output)

}

