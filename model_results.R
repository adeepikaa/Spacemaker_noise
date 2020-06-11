# 1) creates the set of all the models and generates all the RMSEs for comparison
# 2) fine tunes all the models and generates all the RMSEs for comparison
# 3) includes cross validation where seemed fit


# Uses BOTH the non-specific train dataset and the specific train dataset combined together as 
# the train set and partitions it into train and test dataset



# 8 models: Average Baseline, Guess, KNN, SVM Radial, Regression Tree, Random Forest, Linear Regression and GamLoess                    



# Function to calculate RMSE 

create_rmse<-function(x,y){
  rmse<-sqrt(sum((x-y)^2)/length(x))
  return(rmse)
}


#rmse_all to be used as a summary of all rmses. Initialize 
rmse_c_all<-NULL


# Model Number 1 : Guess model (model_guess)  RMSE: rmse_c_guess

model_c_guess<-runif(length(testset$y))

rmse_c_guess<-create_rmse(model_c_guess, testset$y)

# create a table to store all model results for comparison
rmse_c_all<-data.frame(method="Guess", tuned="N/A", RMSE=rmse_c_guess)



# Model Number 2: Baseline Model : Average Model (model_c_avg)  RMSE: rmse_c_avg

y_c_mean <- mean(trainset$y)
model_c_avg<-rep(y_c_mean, length(testset$y))

rmse_c_avg<-create_rmse(model_c_avg, testset$y)

# each rbind statement appends the new RMSE result at the end
rmse_c_all<-rbind(rmse_c_all, data.frame(method ="Average", tuned ="N/A", RMSE = rmse_c_avg))

# kable() from knitr package makes the summary appear as an aligned table, which is easy to read.
rmse_c_all %>% knitr::kable()


# Model Number 3 : KNN regression model (model_knn)  RMSE: rmse_knn using train function

model_knn<-train(trainset[,-1], trainset$y, method="knn",
                 tuneGrid = data.frame(k = seq(3, 50, 2))) 
model_knn$bestTune
ggplot(model_knn, highlight=TRUE)
knn_y<-predict(model_knn, testset[,-1])
rmse_knn<-create_rmse(knn_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="KNN", tuned="Y", RMSE=rmse_knn))
rmse_c_all %>% knitr::kable()


# Model Number 4 : Linear regression model (model_c_lm)  RMSE: rmse_c_lm
model_c_lm<-lm(trainset$y ~ ., data=trainset)
lm_c_y<-predict(model_c_lm, testset)
rmse_c_lm<-create_rmse(lm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Linear Reg.", tuned="N/A", RMSE=rmse_c_lm))
rmse_c_all %>% knitr::kable()


# Model Number 5a : GamLoess regression model (model_c_loess)  RMSE: rmse_c_loess

model_c_loess<-train(trainset[,-1], trainset$y, method="gamLoess") 
loess_c_y<-predict(model_c_loess, testset)
rmse_c_loess<-create_rmse(loess_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="N", RMSE=rmse_c_loess))
rmse_c_all %>% knitr::kable()



# Tuned Model Number 5b : GamLoess regression model (model_c_loess) with fine tuning for hyper parameter span 
#                         RMSE: rmse_c_loess2

model_c_loess2<-train(trainset[,-1], trainset$y, method="gamLoess", 
                      tuneGrid = data.frame(span = seq(0.025, 0.75, 0.025), degree = 1),
                      trControl = trainControl(method = "repeatedcv", number = 10, repeats=3))
model_c_loess2$bestTune

ggplot(model_c_loess2, highlight=TRUE)
loess2_c_y<-predict(model_c_loess2, testset)
rmse_c_loess2<-create_rmse(loess2_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="GamLoess", tuned="Y", RMSE=rmse_c_loess2))
rmse_c_all %>% knitr::kable()


# Model Number 6a : Regression Tree model (model_c_regtree)  RMSE: rmse_c_regtree

set.seed(123)
model_c_regtree <- tree(trainset$y~., trainset)
regtree_c_y<-predict(model_c_regtree, testset)

rmse_c_regtree<-create_rmse(regtree_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="N", RMSE=rmse_c_regtree))
rmse_c_all %>% knitr::kable()

# Tuning Model Number 6b : Regression Tree Model (model_c_tree2)  with fine tuning for complex parameter cp
#                          RMSE: rmse_c_tree2


model_c_tree2<-train(trainset[,-1], trainset$y, method="rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.05, len=25)),
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats=3),) 
model_c_tree2$bestTune

ggplot(model_c_tree2, highlight=TRUE)

tree2_c_y<-predict(model_c_tree2, testset)

rmse_c_tree2<-create_rmse(tree2_c_y, testset$y)

rmse_c_all<-rbind(rmse_c_all, data.frame(method="Reg Tree", tuned="Y", RMSE=rmse_c_tree2))
rmse_c_all %>% knitr::kable()


# Model Number 7a : Support Vector Machine model (model_c_svm)  RMSE: rmse_c_svm)


model_c_svm<-train(trainset[,-1], trainset$y, method="svmRadial") 
svm_c_y<-predict(model_c_svm, testset[,-1])

rmse_c_svm<-create_rmse(svm_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Radial", tuned="N", RMSE=rmse_c_svm))
rmse_c_all %>% knitr::kable()


# Model Number 7b : Support Vector Machine model (model_c_svm) with fine tuning for hyper parameters: tunelength
#                   RMSE: rmse_c_svm2)


model_c_svm2<-train(trainset[,-1], trainset$y, method="svmRadial",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 10) 
svm2_c_y<-predict(model_c_svm2, testset[,-1])

rmse_c_svm2<-create_rmse(svm2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="SVM Radial", tuned="Y", RMSE=rmse_c_svm2))
rmse_c_all %>% knitr::kable()



# Model Number 8a : Random Forest Tree model (model_rf)  RMSE: RMSE=rmse_c_rforest

set.seed(123)
model_c_rforest <- randomForest(trainset$y~., trainset)
rforest_c_y<-predict(model_c_rforest, testset)

rmse_c_rforest<-create_rmse(rforest_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="N", RMSE=rmse_c_rforest))
rmse_c_all %>% knitr::kable()


# Variable Importance Plot
imp <- as.data.frame(varImpPlot(model_c_rforest))
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

jpeg('graphs/forest_varImp.jpg')
ggplot(imp, aes(x=reorder(varnames, -IncNodePurity), weight=IncNodePurity, fill=varnames)) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  ggtitle("Variable Importance of Random Forest")
dev.off()

# Model Number 8b : Random Forest Tree model (model_rf) with fine tuning for hyper parameters: mtry, ntree
#                   RMSE: RMSE=rmse_c_rforest2

plot(model_c_rforest)

t <- tuneRF(trainset[, -1], trainset[, 1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.01)

# Tuned model - lowest mtry = 6, ntree = 400
model_c_rforest2 <- randomForest(trainset$y~., trainset, ntree = 400, mtry = 6)

rforest2_c_y<-predict(model_c_rforest2, testset)

rmse_c_rforest2<-create_rmse(rforest2_c_y, testset$y)
rmse_c_all<-rbind(rmse_c_all, data.frame(method="Random Forest", tuned="Y", RMSE=rmse_c_rforest2))
rmse_c_all %>% knitr::kable()



####################################################
#  PART G: Model Performance on final Test Site Data
####################################################

# Using the Test site data to predict the fraction yellow zone
site_y<-predict(model_c_rforest2, test_set_x)

# Final RMSE results:
rmse_site<-create_rmse(site_y, test_set_y)
rmse_site

# Ensemble Model: Average model using Regression Tree, SVM and Random Forest
# predict fraction yellow zone with the 3 models
site_y_rf<-predict(model_c_rforest2, test_set_x)
site_y_tree<-predict(model_c_tree2, test_set_x)
site_y_svm<-predict(model_c_svm2, test_set_x)

# Compute new fraction yellow zone as average of the above three predictions
site_avg_y<-(site_y_rf+site_y_tree+site_y_svm)/3

# Final Ensemble Model results
rmse_site_avg<-create_rmse(site_avg_y, test_set_y)
rmse_site_avg