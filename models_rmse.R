#######################


#    MODELLING PART 1


# This file creates the first set of all the models (4), and generates all the RMSEs for comparison

# Uses ONLY the non-specific train dataset as the train set
# and the specific train dataset as the test set


# FINAL OUTPUT of the 4 models

#  |method      |tuned |      RMSE|
#  |:-----------|:-----|---------:|
#  |Average     |N     | 0.0639357|
#  |Guess       |N     | 0.3498014|
#  |Linear Reg  |N     | 0.0551626|
#  |KNN         |N     | 0.0806087|
#  |GamLoess    |N     | 0.0537910|



# Function to calculate RMSE 

create_rmse<-function(x,y){
  rmse<-sqrt(sum((x-y)^2)/length(x))
  return(rmse)
}


#rmse_all to be used as a summary of all rmses. 

rmse_all<-NULL    #Initialize 



# Model Number 1: Baseline Model : Average Model (model_avg)  RMSE: rmse_avg

y_mean <- mean(train_set_y)
model_avg<-rep(y_mean, length(validation_set_y))

rmse_avg<-create_rmse(model_avg, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Average", tuned="N", RMSE=rmse_avg))  
rmse_all %>% knitr::kable()



# Model Number 2 : Guess model (model_guess)  RMSE: rmse_guess


model_guess<-runif(length(validation_set_y))

rmse_guess<-create_rmse(model_guess, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Guess", tuned="N", RMSE=rmse_guess))
rmse_all %>% knitr::kable()


# Model Number 3 : Linear regression model (model_lm)  RMSE: rmse_lm

install.packages("FNN")
library(FNN)
library(caret)

model_lm<-lm(train_set_y ~ ., data=train_set)
lm_y<-predict(model_lm, validation_set_x)

rmse_lm<-create_rmse(lm_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Linear Reg", tuned="N", RMSE=rmse_lm))
rmse_all %>% knitr::kable()




# Model Number 3 : KNN regression model (model_knn)  RMSE: rmse_knn

model_knn<-knn.reg(train_set_x, validation_set_x, train_set_y, k=5)   # Choosing k =5
knn_y<-model_knn$pred

rmse_knn<-create_rmse(knn_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="KNN", tuned="N", RMSE=rmse_knn))
rmse_all %>% knitr::kable()


# Model Number 4 : GamLoess regression model (model_loess)  RMSE: rmse_loess

install.packages("gam")
library(gam)
 
model_loess<-train(train_set_x, train_set_y, method="gamLoess") 
loess_y<-predict(model_loess, validation_set_x)

rmse_loess<-create_rmse(loess_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_loess))
rmse_all %>% knitr::kable()



