
combined_set<-data.frame(y=train_cross_valid_set_y, train_cross_valid_set_x)

set.seed(123)
t_index<-createDataPartition(y= combined_set$y, p=0.2, list = FALSE)
testset<-combined_set[t_index,]
trainset<-combined_set[-t_index,]

#rmse_all to be used as a summary of all rmses. Initialize 
rmse_c_all<-NULL

# A Guess model
model_c_guess<-runif(length(testset$y))
rmse_c_guess<-create_rmse(model_c_guess, testset$y)
rmse_c_all<-data.frame(method="Guess", tuned="N", RMSE=rmse_c_guess)

# An average model
y_c_mean <- mean(trainset$y)
model_c_avg<-rep(y_mean, length(testset$y))
rmse_c_avg<-create_rmse(model_c_avg, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Average", tuned="N", RMSE=rmse_c_avg))

# A linear regression model
model_c_lm<-lm(trainset$y ~ ., data=trainset)
lm_c_y<-predict(model_c_lm, testset)
rmse_c_lm<-create_rmse(lm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Linear Reg.", tuned="N/A", RMSE=rmse_c_lm))
rmse_c_all %>% knitr::kable()

# A KNN regression model, with k=5
model_c_knn<-knn.reg(trainset[,-1], testset[,-1], trainset$y,k=5)
knn_c_y<-model_c_knn$pred
rmse_c_knn<-create_rmse(knn_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN", tuned="N", RMSE=rmse_c_knn))
rmse_c_all %>% knitr::kable()

model_knn2<-train(trainset[,-1], trainset$y, method="knn",
                  #trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(k = seq(3, 50, 2))) #can be changed to 50
model_knn2$bestTune
ggplot(model_knn2, highlight=TRUE)
knn2_y<-predict(model_knn2, testset[,-1])
rmse_knn2<-create_rmse(knn2_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN", tuned="Y", RMSE=rmse_knn2))

# A GamLoess regression model
model_c_loess<-train(trainset[,-1], trainset$y, method="gamLoess") 
loess_c_y<-predict(model_c_loess, testset)
rmse_c_loess<-create_rmse(loess_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_c_loess))
rmse_c_all %>% knitr::kable()

model_c_regtree <- tree(trainset$y~., trainset)
regtree_c_y<-predict(model_c_regtree, testset)
rmse_c_regtree<-create_rmse(regtree_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="N", RMSE=rmse_c_regtree))

model_c_rforest <- randomForest(trainset$y~., trainset)
rforest_c_y<-predict(model_c_rforest, testset)
rmse_c_rforest<-create_rmse(rforest_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="N", RMSE=rmse_c_rforest))

model_c_svm<-train(trainset[,-1], trainset$y, method="svmPoly") 
svm_c_y<-predict(model_c_svm, testset[,-1])
rmse_c_svm<-create_rmse(svm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Poly", tuned="N", RMSE=rmse_c_svm))

model_rf<-train(trainset[,-1], trainset$y, method="rf",
                  #trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(mtry = c(1,5,10,25,50,100,200))) #can be changed to 50
rf_c_y<-predict(model_rf, testset[,-1])
rmse_c_rf<-create_rmse(rf_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="Y", RMSE=rmse_c_rf))



model_c_loess2<-train(trainset[,-1], trainset$y, method="gamLoess", 
                      tuneGrid = data.frame(span = seq(0.05, 0.95, 0.05), degree = 1))
model_c_loess2$bestTune
ggplot(model_c_loess2, highlight=TRUE)
loess2_c_y<-predict(model_c_loess2, testset)
rmse_c_loess2<-create_rmse(loess2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="Y", RMSE=rmse_c_loess2))


model_c_tree2<-train(trainset[,-1], trainset$y, method="rpart", 
                      tuneGrid = data.frame(cp = seq(0, 0.05, len=25))) 
model_c_tree2$bestTune
ggplot(model_c_tree2, highlight=TRUE)
tree2_c_y<-predict(model_c_tree2, testset)
rmse_c_tree2<-create_rmse(tree2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="Y", RMSE=rmse_c_tree2))
