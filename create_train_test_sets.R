#######################


# This file creates the train and test datasets for the Spacemaker noise surrogate model


# Below is the complete list of all the 23 features 


# cov_rd, cov_bldg, cov_ratio, bldg_avg, bldg_median, bldg_max, bldg_min, bldg_diff, avg_dist_rd, min_dist_rd,
# ht_min_dist_rd, cov_buffer1, cov_buffer2, cov_buffer3, cov_buffer, ht_buffer1, ht_buffer2, ht_buffer3, 
# ht_buffer4, htmax_buffer1, htmax_buffer2, htmax_buffer3, htmax_buffer4


# Creating a vector of x values from 17 of them 

xvalues<-c("cov_rd", "cov_bldg", "cov_ratio", "avg_dist_rd", "min_dist_rd", "ht_min_dist_rd", 
           "cov_buffer1", "cov_buffer2", "cov_buffer3", "cov_buffer4", "ht_buffer1", "ht_buffer2", 
           "ht_buffer3", "ht_buffer4", "htmax_buffer1", "htmax_buffer2", "htmax_buffer3", "htmax_buffer4")



# Using the non-specific train and specific dataset to create the x values

train_set_x<-non_spec_final_set[,xvalues]

validation_set_x<-spec_final_set[,xvalues]


# Using the non-specific train and specific dataset to create the y values
train_set_y<-non_spec_final_set[,"fraction_yellow_zone"]

validation_set_y<-spec_final_set[,"fraction_yellow_zone"]



# Combining x and y values into a dataframe as a train dataset

train_set <- data.frame(train_set_y, train_set_x)
test_set <- data.frame(validation_set_y, validation_set_x)

# join train and validation set by column names (combining non-specific and specific train 
# dataset into one)

# Hypothesis is that joining the two and using it to train the model would give better results 
# when the model is finally tested on the test set


train_cross_valid_set_x<-rbind(train_set_x, validation_set_x)
train_cross_valid_set_y<-c(train_set_y, validation_set_y)




#install.packages("caret")
library(caret)

combined_set<-data.frame(y=train_cross_valid_set_y, train_cross_valid_set_x)

set.seed(123)

t_index<-createDataPartition(y= combined_set$y, p=0.2, list = FALSE)

testset<-combined_set[t_index,]
trainset<-combined_set[-t_index,]




#To be used only during final RMSE calulations
test_set_x<-test_final_set[,xvalues]
test_set_y<-test_final_set[,"fraction_yellow_zone"]
