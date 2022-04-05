BestFeatures_mod <- function ( train, test, treat, outcome, predictors, rank.precision = 2, path, 
                          equal.intervals = FALSE, nb.group = 10) {
# path=my_path
# train=train_oh
# test=test_oh
# treat='treat'
# outcome='y'
# predictors=features_oh
# rank.precision = 2
# equal.intervals = FALSE
# nb.group = 10

path <- path[!duplicated(path[,"dimension"]), ]
# Keep paths of dimension > 0
path <- path[path[, "dimension"] > 0, ]
lambda.qini <- c()
for (k in 1:nrow(path)) {
  features <- path[k, -c(1, 2)]
  # Keep features with non zero estimates only
  features <- features[features != 0]
  
  # Fit the logistic regression model with selected features only
  form=as.formula(paste("y~", paste(names(features), collapse="+")))
  lambda.model=glm(formula=form, data=train, family=binomial(link=logit))
  
  test$lambda.pred <- predict(lambda.model, test, treat)
    lambda.perf <- PerformanceUplift(test, treat, outcome, 
                                     "lambda.pred", 
                                     rank.precision = rank.precision, 
                                     equal.intervals = equal.intervals, 
                                     nb.group = nb.group)
    if (length(lambda.perf[[1]]) == 1) { lambda.qini[k] <- 0}
    else {lambda.qini[k] <- QiniArea(lambda.perf)}
    
  }

best.model <- cbind(path[, c(1, 2)], lambda.qini)

if (max(best.model[,3]) == 0) { 
  warning("All models result in a Qini coefficient equal to 0. Please check LassoPath().")
}
# Take the model that maximizes the qini coefficient
max.qini <- which.max(best.model[,3])
best.model <- best.model[max.qini,]

# We also need to know which variables were selected
best.features <- path[path[, "lambda"] == best.model["lambda"], -c(1, 2, 3)]
best.features <- names(best.features[best.features != 0])
class(best.features) <- "BestFeatures"
return(best.features)
}