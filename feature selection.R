
##--feature selection step in 100 loops
Data_GTV <- TV_GTV[,3:1234]
Outcome <- TV_GTV$Outcomes

top_features_GTV <- matrix("", nrow = 100, ncol = 8)
AUCs_GTV <- c()

for (i in 1:100) {
  ##split in trainig and validation maintaining balance in outcomes
  set.seed(i)
  
  train_ind <-
    createDataPartition(as.factor(Outcome),
                        times = 1,
                        p = 0.7,
                        list = FALSE)
  
  OutcomeT <- Outcome[train_ind]
  OutcomeV <- Outcome[-train_ind]
  #--GTV
  DataT_GTV <- Data_GTV[train_ind, ]
  DataV_GTV <- Data_GTV[-train_ind, ]
  
  ## create correlation matrix and remove highly correlated features
  
  corMatrix_GTV =  cor(Data_GTV, y = NULL, use = "ev",method = "spearman")
  highly_correlated_columns_GTV = findCorrelation(
    corMatrix_GTV,
    cutoff = 0.8,
    verbose = FALSE,
    names = FALSE,
    exact = TRUE
  )
  DT_GTV <- DataT_GTV[, -highly_correlated_columns_GTV]
  DV_GTV <- DataV_GTV[, -highly_correlated_columns_GTV]
  
  ##TRAINING
  #first get all combinations of top 8 variables to find "optimal" number of predictors
  GTV_features_withoutcome <- cbind(OutcomeT, DT_GTV)
  data_balanced_GTV_features <- ROSE(OutcomeT ~ ., data=GTV_features_withoutcome)$data
  ctrl_GTV <-
    rfeControl(
      functions = treebagFuncs,
      method = "repeatedcv",
      repeats = 5,
      verbose = FALSE
    )
  rfProfile_GTV <- rfe(data_balanced_GTV_features[,-1],as.factor(data_balanced_GTV_features$OutcomeT), rfeControl = ctrl_GTV, size = 20)
  
  ## set best combo predictors vs plateau
  selectedData_GTV <- data_balanced_GTV_features[, rfProfile_GTV$optVariables[1:8]]
  ##if outcomes skewed, over and undersample (see https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/)
  RandomForest_GTV <-
    randomForest(
      y = as.factor(data_balanced_GTV_features$OutcomeT),
      x = as.data.frame(selectedData_GTV),
      na.action = na.exclude,
      mtry = 5
    )
  #varImpPlot(RandomForest)
  ForestPrediction_GTV <- predict(RandomForest_GTV, DV_GTV[, rfProfile_GTV$optVariables[1:8]], type = "prob")
  ForestRocTesting_GTV <- roc(OutcomeV, ForestPrediction_GTV[, 2], ci = TRUE)
  #ForestRocTraining
  
  top_features_GTV[i,] <-rfProfile_GTV$optVariables[1:8]
  AUCs_GTV[i] <- ForestRocTesting_GTV$auc