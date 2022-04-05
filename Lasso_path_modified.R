LassoPath_mod <- function (data, formula) {
  
  # LASSO path for penalized logistic regression.
  #
  # Args:
  #   data: a data frame containing the treatment, the outcome and the predictors.
  #   formula: an object of class "formula" (or one that can be coerced to that class): 
  #            a symbolic description of the model to be fitted.
  # Returns:
  #   A dataframe containing the coefficients values and the number of nonzeros coefficients.
  
  X <- model.matrix(formula, data)
  y <- model.frame(formula, data)[, 1]
  
  get_lambdas <- cv.glmnet(X, 
                           y, 
                           family="binomial",
                           type.measure="auc",
                           nfolds=3)
  
  glmnet.output <- glmnet(X, y, alpha = 1, family = "binomial", 
                          lambda = get_lambdas$lambda, 
                          standardize=TRUE, 
                          intercept = FALSE,
                          type.logistic = "modified.Newton")
  
  
  dimension <- glmnet.output$df
  coeff <- t(as.matrix(glmnet.output$beta))
  lambda <- glmnet.output$lambda
  path <- cbind(lambda, dimension, coeff)
  
  path_list=list(coeff, dimension, lambda)
  
  return(path_list)
}

# END FUN