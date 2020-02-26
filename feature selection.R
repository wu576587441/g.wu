
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
}

#--features order
top_features_GTV<-as.vector(top_features_GTV, mode="any")
top_featuresfreq_GTV<-count(top_features_GTV)
top_topfeaturesfreq_GTV<-top_featuresfreq_GTV[order(top_featuresfreq_GTV[,"freq"], decreasing= T), ]
top_topfeaturesfreq_GTV

#-- More than 30
selected_30_GTV <- subset(top_topfeaturesfreq_GTV, freq >= 30,select = c(x))
GTV_top_features_name<- array(selected_30_GTV$x)
GTV_top_features <- Data_GTV[GTV_top_features_name]

#--feature correlation
corMatrix_top_GTV <-  cor(GTV_top_features, y = NULL, use = "ev",method = "spearman")
highly_correlated_columns_top_GTV <- findCorrelation(corMatrix_top_GTV,cutoff = 0.8,verbose = FALSE,names = FALSE,exact = TRUE)
#GTV_top_features <- GTV_top_features[, -highly_correlated_columns_top_GTV]

#--Brouta
library("Boruta")
set.seed(1234)
boruta_GTV <-Boruta (GTV_top_features,as.factor(TT_Outcome),pValue = 0.01,mcAdj = TRUE,maxRuns = 500,doTrace = 2,ntree =500,holdHistory = TRUE,getImp = getImpExtraZ)
plot(boruta_GTV)
attStats(boruta_GTV
