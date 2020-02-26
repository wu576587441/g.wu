#--Modeling
library(randomForest)
#--Training
set.seed(1234)
RandomForest_GTV_feature <- randomForest(y = as.factor(Outcome_balanced_GTV), x = as.data.frame(data_balanced_GTV[,-4]),ntree = 500, na.action = na.exclude)
plot(RandomForest_GTV_feature)
varImpPlot(RandomForest_GTV_feature)
ForestPrediction_GTV_feature<-predict(RandomForest_GTV_feature,type = "prob")[,2]
ForestRocGTV_feature<-roc(Outcome_balanced_GTV ,ForestPrediction_GTV_feature, ci=TRUE)
ForestRocGTV_feature
plot(ForestRocGTV_feature)
P_GTV_training<-predict(RandomForest_GTV_feature)
CM_GTV_training<-confusionMatrix(as.factor(P_GTV_training),as.factor(Outcome_balanced_GTV))
CM_GTV_training

#---IDI
library(PredictABEL)
Model_GTV_PTV_training <- ForestPrediction_GTV_PTV_feature
Model_GTV_training <- ForestPrediction_GTV_feature
vs_GTV_GTV_PTV_training <- reclassification(data = data_balanced_GGTV_IDI,cOutcome = 5,predrisk1 = Model_GTV_PTV_training,predrisk2 = Model_GTV_training,cutoff = c(0,0.5,1))

##--calibrationplot
set.seed(1234)
plotcalibration_s1 <- plotCalibration(data.frame(as.numeric(Roc_semantic$original.response), Roc_semantic$original.predictor), 1, Roc_semantic$original.predictor, groups= 4, plottitle='Calibration')
plotcalibration_s1

#--ROC.test
roc.test(Roc_semantic, ForestRocGTV_feature)