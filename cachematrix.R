library(caret)
library(randomForest)
library(varImp)
regressor <- randomForest(OCSKGM ~ ., data = dat_sites, importance=TRUE)
# fit the random forest with default parameter
varImp(regressor)
#  # get variable importance, based on mean decrease in accuracy
varImp(regressor, conditional=TRUE)
#   # conditional=True, adjusts for correlations between predictors
varimpAUC(regressor) 
#   # more robust towards class imbalance.
#   
#   
#   
gbmImp <- varImp(regressor, scale = TRUE)
gbmImp
plot(gbmImp)



base.mod <- lm(OCSKGM ~ 2, data = dat_sites) # base intercept only 
all.mod <- lm(OCSKGM ~ ., data = dat_sites) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 1, steps = 1000) # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

shortlistedVars


ibrary(randomForest)
set.seed(71)
rf <-randomForest(OCSKGM ~ ., data = dat_sites, ntree=1000) 
print(rf)
rf
floor(sqrt(ncol(dat_sites) - 1))
mtry <- tuneRF(dat_sites[-1],dat_sites$OCSKGM, ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
rf <-randomForest(OCSKGM~.,data=dat_sites, mtry=best.m, importance=TRUE,ntree=1000)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)




