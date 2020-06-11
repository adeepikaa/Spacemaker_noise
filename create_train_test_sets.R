# Below is the complete list of all the 23 features 


# cov_rd, cov_bldg, cov_ratio, bldg_avg, bldg_median, bldg_max, bldg_min, bldg_diff, avg_dist_rd, min_dist_rd,
# ht_min_dist_rd, cov_buffer1, cov_buffer2, cov_buffer3, cov_buffer, ht_buffer1, ht_buffer2, ht_buffer3, 
# ht_buffer4, htmax_buffer1, htmax_buffer2, htmax_buffer3, htmax_buffer4


# Creating a vector of x values from 10 of them 

xvalues<-c("cov_rd",
           "cov_bldg",
           "cov_ratio",
           "htmax_buffer1",
           "htmax_buffer3",
           "htmax_buffer4",
           "cov_buffer1",
           "cov_buffer3",
           "cov_buffer4",
           "avg_dist_rd")


set_x<-rbind(non_spec_final_set[,xvalues], spec_final_set[,xvalues])
set_y<-c(non_spec_final_set[,"fraction_yellow_zone"], spec_final_set[,"fraction_yellow_zone"])


combined_set<-data.frame(y=set_y, set_x)

set.seed(123)

t_index<-createDataPartition(y= combined_set$y, p=0.2, list = FALSE)

testset<-combined_set[t_index,]
trainset<-combined_set[-t_index,]


#To be used only during final RMSE calulations
test_set_x<-test_final_set[,xvalues]
test_set_y<-test_final_set[,"fraction_yellow_zone"]

rm(t_index, set_x, set_y, col, p, p1, i)


