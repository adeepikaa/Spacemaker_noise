library(ggplot2) ## ggplot
library(RcppCNPy) ## for npyload
library(Matrix) ## for nnzero
library(dplyr) ## to use %>% 

# Function to create features: coverage of road and coverage of building
create_coverage <- function(x) {
  input_path <- paste("data/", x, sep="") #create the path
  print(input_path)
  sample_input <- npyLoad(input_path, "integer") # open the file
  sample_fraction <- nnzero(sample_input)/length(sample_input) #calculate the coverage
  return(sample_fraction)
}

# Function to create building features as a whole
create_bldg_stats <- function(x) {
  input_path <- paste("data/", x, sep="")
  print(input_path)
  sample_input <- npyLoad(input_path, "integer")
  # calculate average building height(excludes the zeros)
  bldg_avg<-mean(sample_input[sample_input>0]) 
  # calculate median building height(excludes the zeros)
  bldg_median<-median(sample_input[sample_input>0]) 
  # calculate max building height(excludes the zeros)
  bldg_max<-max(sample_input) 
  # calculate min building height(excludes the zeros)
  bldg_min<-min(sample_input[sample_input>0]) 
  # calculate difference of max and min building height
  bldg_diff<-bldg_max-bldg_min
  # return the aggregates as a vector
  c(bldg_avg=bldg_avg, bldg_median=bldg_median, 
    bldg_max=bldg_max, bldg_min=bldg_min, bldg_diff=bldg_diff)
}

# Function to create fetaures for multiple datasets
create_stats_vec<- function(set, src, bldg) {
  set$cov_rd <- sapply(set[,src],  create_coverage) # get road coverage
  set$cov_bldg <- sapply(set[,bldg], create_coverage) # get building coverage
  set$cov_ratio<-as.numeric(set$cov_bldg)/as.numeric(set$cov_rd) # ratio of building coverage to road coverage
  set_stats<-sapply(set[,bldg], create_bldg_stats) # get building stats
  set_stats_t<-t(set_stats)
  set_final<-cbind(set, set_stats_t) #merge building stats 
  return(set_final) 
}

non_spec_final<-create_stats_vec(json_non_specific, "source_grid_path", "building_grid_path")
spec_final<-create_stats_vec(json_specific, "source_grid_path", "building_grid_path")
test_final<-create_stats_vec(json_test, "source_grid_path", "building_grid_path")

# 
non_spec_dist_stats<-mapply(create_dist_stats, json_non_specific$source_grid_path, json_non_specific$building_grid_path)
spec_dist_stats<-mapply(create_dist_stats, json_specific$source_grid_path, json_specific$building_grid_path)
test_dist_stats<-mapply(create_dist_stats, json_test$source_grid_path, json_test$building_grid_path)
non_spec_dist_stats_t<-t(non_spec_dist_stats)
spec_dist_stats_t<-t(spec_dist_stats)
test_dist_stats_t<-t(test_dist_stats)

non_spec_final_set<-cbind(non_spec_final, non_spec_dist_stats_t)
spec_final_set<-cbind(spec_final, spec_dist_stats_t)
test_final_set<-cbind(test_final, test_dist_stats_t)
head(test_final_set)

#rm(non_spec_final, spec_final, test_final, 
#   non_spec_dist_stats, spec_dist_stats, test_dist_stats, 
#   non_spec_dist_stats_t, spec_dist_stats_t, test_dist_stats_t)
