#xvalues<-c("cov_rd", "cov_bldg", "cov_ratio", 
           # "bldg_avg", "bldg_median", "bldg_max", "bldg_min", "bldg_diff", 
           # "avg_dist_rd", "min_dist_rd", "ht_min_dist_rd", 
           # "cov_buffer1", "cov_buffer2", "cov_buffer3", "cov_buffer4",
           # "ht_buffer1", "ht_buffer2", "ht_buffer3", "ht_buffer4", 
           # "htmax_buffer1", "htmax_buffer2", "htmax_buffer3", "htmax_buffer4")

xvalues<-c("cov_rd", "cov_bldg", "cov_ratio", "avg_dist_rd", "min_dist_rd", "ht_min_dist_rd", 
           "cov_buffer1", "cov_buffer2", "cov_buffer3", "cov_buffer4", "ht_buffer1", "ht_buffer2", "ht_buffer3", "ht_buffer4",
           "htmax_buffer1", "htmax_buffer2", "htmax_buffer3", "htmax_buffer4")

train_set_x<-non_spec_final_set[,xvalues]
validation_set_x<-spec_final_set[,xvalues]
train_set_y<-non_spec_final_set[,"fraction_yellow_zone"]
validation_set_y<-spec_final_set[,"fraction_yellow_zone"]

train_set<-data.frame(train_set_y, train_set_x)

# join train and validation set by column names
train_cross_valid_set_x<-full_join(train_set_x, validation_set_x)
train_cross_valid_set_y<-rbind(train_set_y, validation_set_y)

#To be used only during final RMSE calulations
test_set_x<-test_final_set[,xvalues]
test_set_y<-test_final_set[,"fraction_yellow_zone"]
