library(ggplot2) ## ggplot
library(RcppCNPy) ## for npyload
library(Matrix) ## for nnzero
library(dplyr) ## to use %>% 


create_coverage <- function(x) {
  input_path <- paste("data/", x, sep="")
  print(input_path)
  sample_input <- npyLoad(input_path, "integer")
  sample_fraction <- nnzero(sample_input)/length(sample_input)
  return(sample_fraction)
}
create_bldg_stats <- function(x) {
  input_path <- paste("data/", x, sep="")
  print(input_path)
  sample_input <- npyLoad(input_path, "integer")
  bldg_avg<-mean(sample_input[sample_input>0]) 
  bldg_median<-median(sample_input[sample_input>0]) 
  bldg_max<-max(sample_input) 
  bldg_min<-min(sample_input[sample_input>0]) 
  bldg_diff<-bldg_max-bldg_min
  c(bldg_avg=bldg_avg, bldg_median=bldg_median, 
    bldg_max=bldg_max, bldg_min=bldg_min, bldg_diff=bldg_diff)
}

create_stats_vec<- function(set, src, bldg) {
  set$cov_rd <- sapply(set[,src],  create_coverage)
  set$cov_bldg <- sapply(set[,bldg], create_coverage)
  set$cov_ratio<-as.numeric(set$cov_bldg)/as.numeric(set$cov_rd)
  set_stats<-sapply(set[,bldg], create_bldg_stats)
  set_stats_t<-t(set_stats)
  set_final<-cbind(set, set_stats_t)
  return(set_final)
}

non_spec_final<-create_stats_vec(json_non_specific, "source_grid_path", "building_grid_path")
spec_final<-create_stats_vec(json_specific, "source_grid_path", "building_grid_path")
test_final<-create_stats_vec(json_test, "source_grid_path", "building_grid_path")

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
