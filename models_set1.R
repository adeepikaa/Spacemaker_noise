#######################

#    Models Set 1


# This file creates the first set of all the models (8), and generates all the RMSEs for comparison

# Uses ONLY the non-specific train dataset as the train set
# and the specific train dataset as the test set


# FINAL OUTPUT of the 8 models:

#  |method        |tuned |      RMSE|
#  |:-------------|:-----|---------:|
#  |Guess         |N     | 0.3247182|
#  |Random Forest |N     | 0.0864403|
#  |KNN_1         |N     | 0.0806087|
#  |KNN_2         |Y     | 0.0727067|
#  |Average       |N     | 0.0639357|
#  |Reg Tree      |N     | 0.0625724|
#  |Linear Reg    |N     | 0.0551626|
#  |GamLoess      |N     | 0.0537910|
#  |SVM Poly      |N     | 0.0509911|



# Function to calculate RMSE 

create_rmse<-function(x,y){
  rmse<-sqrt(sum((x-y)^2)/length(x))
  return(rmse)
}


#rmse_all to be used as a summary of all rmses. 

rmse_all<-NULL    #Initialize 



# Model Number 1 : Guess model (model_guess)  RMSE: rmse_guess


model_guess<-runif(length(validation_set_y))

rmse_guess<-create_rmse(model_guess, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Guess", tuned="N", RMSE=rmse_guess))
rmse_all %>% knitr::kable()


# Model Number 2 : Random Forest Tree model (model_rforest)  RMSE: rmse_rforest

library(randomForest)
model_rforest <- randomForest(train_set_y~., train_set)
rforest_y<-predict(model_rforest, validation_set_x)


rmse_rforest<-create_rmse(rforest_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Random Forest", tuned="N", RMSE=rmse_rforest))
rmse_all %>% knitr::kable()



# Model Number 3a : KNN regression model (model_knn)  RMSE: rmse_knn

model_knn<-knn.reg(train_set_x, validation_set_x, train_set_y, k=5)   # Choosing k =5
knn_y<-model_knn$pred

rmse_knn<-create_rmse(knn_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="KNN_1", tuned="N", RMSE=rmse_knn))
rmse_all %>% knitr::kable()


# Model Number 3b : KNN regression model (model_knn1)  RMSE: rmse_knn1 using train function

model_knn1<-train(train_set_x, train_set_y, method="knn",
                  #trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(k = seq(3, 100, 2))) #can be changed to 50
model_knn1$bestTune

ggplot(model_knn1, highlight=TRUE)
knn1_y<-predict(model_knn1, validation_set_x)

rmse_knn1<-create_rmse(knn1_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="KNN_2", tuned="Y", RMSE=rmse_knn1))
rmse_all %>% knitr::kable()


# Model Number 4: Baseline Model : Average Model (model_avg)  RMSE: rmse_avg

y_mean <- mean(train_set_y)
model_avg<-rep(y_mean, length(validation_set_y))

rmse_avg<-create_rmse(model_avg, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Average", tuned="N", RMSE=rmse_avg))  
rmse_all %>% knitr::kable()



# Model Number 5 : Regression Tree model (model_regtree)  RMSE: rmse_regtree


library(tree)

model_regtree <- tree(train_set_y~., train_set)
regtree_y<-predict(model_regtree, validation_set_x)
plot(model_regtree)

# This will show the values on the tree

text(model_regtree, pretty = 0)


rmse_regtree<-create_rmse(regtree_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Reg Tree", tuned="N", RMSE=rmse_regtree))
rmse_all %>% knitr::kable()


# Model Number 5 : Linear regression model (model_lm)  RMSE: rmse_lm

install.packages("FNN")
library(FNN)
library(caret)

model_lm<-lm(train_set_y ~ ., data=train_set)
lm_y<-predict(model_lm, validation_set_x)

rmse_lm<-create_rmse(lm_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Linear Reg", tuned="N", RMSE=rmse_lm))
rmse_all %>% knitr::kable()



# Model Number 5 : GamLoess regression model (model_loess)  RMSE: rmse_loess

install.packages("gam")
library(gam)
 
model_loess<-train(train_set_x, train_set_y, method="gamLoess") 
loess_y<-predict(model_loess, validation_set_x)

rmse_loess<-create_rmse(loess_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_loess))
rmse_all %>% knitr::kable()



# Model Number 7 : Support Vector Machine model (model_svm)  RMSE: rmse_svm)

library(kernlab)

model_svm<-train(train_set_x, train_set_y, method="svmPoly") 
svm_y<-predict(model_svm, validation_set_x)

rmse_svm<-create_rmse(svm_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="SVM Poly", tuned="N", RMSE=rmse_svm))
rmse_all %>% knitr::kable()





