#######################


#    Models Set 2


# This file :

# 1) creates the second set of all the models (8), and generates all the RMSEs for comparison
# 2) fine tunes all the models and generates all the RMSEs for comparison


# Uses BOTH the non-specific train dataset and the specific train dataset combined together as 
# the train set and partitions it into train and test dataset


# FINAL OUTPUT of the 8 models

#  |method        |tuned |      RMSE|
#  |:-------------|:-----|---------:|
#  |Guess         |N     | 0.3665481|
#  |Average       |N     | 0.1691356|
#  |KNN_1         |N     | 0.0915582|
#  |KNN_2         |Y     | 0.0882236|
#  |Reg Tree      |N     | 0.0738796|
#  |Linear Reg.   |N     | 0.0734966|
#  |GamLoess      |N     | 0.0665017|
#  |SVM Poly      |N     | 0.0599199|
#  |Random Forest |N     | 0.0553471|






#rmse_all to be used as a summary of all rmses. Initialize 
rmse_c_all<-NULL




# Model Number 1 : Guess model (model_c_guess)  RMSE: rmse_c_guess

model_c_guess<-runif(length(testset$y))

rmse_c_guess<-create_rmse(model_c_guess, testset$y)
rmse_c_all<-data.frame(method="Guess", tuned="N", RMSE=rmse_c_guess)



# Model Number 2: Baseline Model : Average Model (model_c_avg)  RMSE: rmse_c_avg

y_c_mean <- mean(trainset$y)
model_c_avg<-rep(y_mean, length(testset$y))

rmse_c_avg<-create_rmse(model_c_avg, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method ="Average", tuned ="N", RMSE = rmse_c_avg))
rmse_c_all %>% knitr::kable()



# Model Number 3a : KNN regression model (model_c_knn)  RMSE: rmse_c_knn, with k=5

model_c_knn<-knn.reg(trainset[,-1], testset[,-1], trainset$y,k=5)
knn_c_y<-model_c_knn$pred

rmse_c_knn<-create_rmse(knn_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN_1", tuned="N", RMSE=rmse_c_knn))
rmse_c_all %>% knitr::kable()



# Model Number 3b : KNN regression model (model_knn2)  RMSE: rmse_knn2 using train function

model_knn2<-train(trainset[,-1], trainset$y, method="knn",
                  #trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(k = seq(3, 50, 2))) #can be changed to 50
model_knn2$bestTune
ggplot(model_knn2, highlight=TRUE)
knn2_y<-predict(model_knn2, testset[,-1])

rmse_knn2<-create_rmse(knn2_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN_2", tuned="Y", RMSE=rmse_knn2))
rmse_c_all %>% knitr::kable()


# Model Number 4 : Regression Tree model (model_c_regtree)  RMSE: rmse_c_regtree


library(tree)

model_c_regtree <- tree(trainset$y~., trainset)
regtree_c_y<-predict(model_c_regtree, testset)

rmse_c_regtree<-create_rmse(regtree_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="N", RMSE=rmse_c_regtree))
rmse_c_all %>% knitr::kable()



# Model Number 5 : Linear regression model (model_c_lm)  RMSE: rmse_c_lm

model_c_lm<-lm(trainset$y ~ ., data=trainset)
lm_c_y<-predict(model_c_lm, testset)

rmse_c_lm<-create_rmse(lm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Linear Reg.", tuned="N", RMSE=rmse_c_lm))
rmse_c_all %>% knitr::kable()





# Model Number 6 : GamLoess regression model (model_c_loess)  RMSE: rmse_c_loess

model_c_loess<-train(trainset[,-1], trainset$y, method="gamLoess") 
loess_c_y<-predict(model_c_loess, testset)

rmse_c_loess<-create_rmse(loess_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_c_loess))
rmse_c_all %>% knitr::kable()


# Model Number 7 : Support Vector Machine model (model_c_svm)  RMSE: rmse_c_svm)

library(kernlab)

model_c_svm<-train(trainset[,-1], trainset$y, method="svmPoly") 
svm_c_y<-predict(model_c_svm, testset[,-1])

rmse_c_svm<-create_rmse(svm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Poly", tuned="N", RMSE=rmse_c_svm))
rmse_c_all %>% knitr::kable()


# Model Number 8 : Random Forest Tree model (model_c_rforest)  RMSE: rmse_c_rforest

library(randomForest)
model_c_rforest <- randomForest(trainset$y~., trainset)
rforest_c_y<-predict(model_c_rforest, testset)

rmse_c_rforest<-create_rmse(rforest_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="N", RMSE=rmse_c_rforest))
rmse_c_all %>% knitr::kable()


# Checking to see the feature contributions

varImpPlot(model_c_rforest)



##############################################################3



# Fine tuning the models


# Tuning Model Number 5 : GamLoess Regression Model (model_c_loess2)  RMSE: rmse_c_loess2


model_c_loess2<-train(trainset[,-1], trainset$y, method="gamLoess", 
                      tuneGrid = data.frame(span = seq(0.05, 0.95, 0.05), degree = 1))
model_c_loess2$bestTune

ggplot(model_c_loess2, highlight=TRUE)
loess2_c_y<-predict(model_c_loess2, testset)

rmse_c_loess2<-create_rmse(loess2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="Y", RMSE=rmse_c_loess2))



# Tuning Model Number 6 : Regression Tree Model (model_c_tree2)  RMSE: rmse_c_tree2

model_c_tree2<-train(trainset[,-1], trainset$y, method="rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len=25))) 
model_c_tree2$bestTune

ggplot(model_c_tree2, highlight=TRUE)
tree2_c_y<-predict(model_c_tree2, testset)

rmse_c_tree2<-create_rmse(tree2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="Y", RMSE=rmse_c_tree2))




# Tuning Model Number 7 : Random Forest Tree model (model_rf)  RMSE: RMSE=rmse_c_rf


model_rf<-train(trainset[,-1], trainset$y, method="rf",
                #trControl = trainControl(method = "cv", number = 5),
                tuneGrid = data.frame(mtry = c(1,5,10,25,50,100,200))) #can be changed to 50
rf_c_y<-predict(model_rf, testset[,-1])

rmse_c_rf<-create_rmse(rf_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="Y", RMSE=rmse_c_rf))

rmse_c_all
