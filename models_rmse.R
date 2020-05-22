# Function to calculate RMSE 
create_rmse<-function(x,y){
  rmse<-sqrt(sum((x-y)^2)/length(x))
  return(rmse)
}
#rmse_all to be used as a summary of all rmses. Initialize 
rmse_all<-NULL

# A Guess model
model_guess<-runif(length(validation_set_y))
rmse_guess<-create_rmse(model_guess, validation_set_y)
rmse_all<-data.frame(method="Guess", tuned="N", RMSE=rmse_guess)

# An average model
y_mean <- mean(train_set_y)
model_avg<-rep(y_mean, length(validation_set_y))
rmse_avg<-create_rmse(model_avg, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Average", tuned="N", RMSE=rmse_avg))
rmse_all %>% knitr::kable()

library(FNN)
library(caret)

# A linear regression model
model_lm<-lm(train_set_y ~ ., data=train_set)
lm_y<-predict(model_lm, validation_set_x)
rmse_lm<-create_rmse(lm_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="Linear Reg.", tuned="N/A", RMSE=rmse_lm))
rmse_all %>% knitr::kable()

# A KNN regression model, with k=5
model_knn<-knn.reg(train_set_x, validation_set_x, train_set_y,k=5)
knn_y<-model_knn$pred
rmse_knn<-create_rmse(knn_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="KNN", tuned="N", RMSE=rmse_knn))
rmse_all %>% knitr::kable()

# A GamLoess regression model
model_loess<-train(train_set_x, train_set_y, method="gamLoess") 
loess_y<-predict(model_loess, validation_set_x)
rmse_loess<-create_rmse(loess_y, validation_set_y)
rmse_all<-rbind(rmse_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_loess))
rmse_all %>% knitr::kable()

