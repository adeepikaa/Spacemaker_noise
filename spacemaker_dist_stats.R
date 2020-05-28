#######################



# This file creates all the distance related features for the Spacemaker noise surrogate model



# Function to create distance features

# arguments get both *npy files src (source), bldg(building)

# for the purposes of the distance features, each building has been treated as a collection of 
# buildings at adjacent locations

create_dist_stats <- function(x,y) {
  src_path <- paste("data/", x, sep="")
  print(src_path)
  src_file <- npyLoad(src_path, "integer")
  bldg_path <- paste("data/", y, sep="")
  print(bldg_path)
  bldg_file <- npyLoad(bldg_path, "integer")
  #initialize arr (array) to save a list of locations that have noise source- to null
  arr<-NULL
  for (i in 1:51){
    for (j in 1:51) {
      if (src_file[i,j] > 0) {
        arr<-rbind(arr, c(i,j))
      }   
    }
  }
  
  
  
  
  # define distance as the distance array that has distances between one location of building to all noise locations
  
  distance <- NULL
  D <- matrix(0, ncol=ncol(bldg_file), nrow=nrow(bldg_file)) 
  
  # D is the matrix which has each building's minimum distance to road
  for (i in 1:51){
    for (j in 1:51) {
      if (bldg_file[i,j] > 0) {
        for (k in 1:nrow(arr)) {
          distance[k]<-sqrt((arr[k,1]-i)^2+(arr[k,2]-j)^2)    #calculate distance to the array of noise locations
        }
        D[i,j]<-min(distance)        #find minimum distance and save in D
      }
    }
  }
  
  
  avg_dist_rd<-mean(D[D>0])              # average of all minimum distance gives average distance from building to road
  min_dist_rd<-min(D[D>0])               # minimum distance from building to road
  ht_min_dist_rd<-min(bldg_file[D==min_dist_rd]) # height of closest building to road
  
  
  
  
  # get building coverage for 4 buffer zones to portray density from road
  
  bldg_cov_rd_12<-sum(D[D<=12])/length(bldg_file)           # Zone 1 for distance less than 12 units
  bldg_cov_rd_24<-sum(D[D>12 & D<=24])/length(bldg_file)    # Zone 2 for distance between 12 and 24 units
  bldg_cov_rd_36<-sum(D[D>24 & D<=36])/length(bldg_file)    # Zone 3 for distance between 24 and 36 units
  bldg_cov_rd_51<-sum(D[D>36])/length(bldg_file)            # Zone 4 for distance greater than 36 units
  
  
  
  # get average building heights within the for 4 different buffer zones 
  
  ht_avg_dist_rd_12<-ifelse(bldg_cov_rd_12!=0, mean(bldg_file[D>0 & D<=12]), 0)
  ht_avg_dist_rd_24<-ifelse(bldg_cov_rd_24!=0, mean(bldg_file[D>12 & D<=24]), 0)
  ht_avg_dist_rd_36<-ifelse(bldg_cov_rd_36!=0, mean(bldg_file[D>24 & D<=36]), 0)
  ht_avg_dist_rd_51<-ifelse(bldg_cov_rd_51!=0, mean(bldg_file[D>36]), 0)
  
  
  # get maximum building heights within the 4 different buffer zones 
  
  ht_max_dist_rd_12<-ifelse(bldg_cov_rd_12!=0, max(bldg_file[D>0 & D<=12]), 0)
  ht_max_dist_rd_24<-ifelse(bldg_cov_rd_24!=0, max(bldg_file[D>12 & D<=24]), 0)
  ht_max_dist_rd_36<-ifelse(bldg_cov_rd_36!=0, max(bldg_file[D>24 & D<=36]), 0)
  ht_max_dist_rd_51<-ifelse(bldg_cov_rd_51!=0, max(bldg_file[D>36]), 0)
  
  
  
  return(c(avg_dist_rd=avg_dist_rd, min_dist_rd=min_dist_rd, ht_min_dist_rd=ht_min_dist_rd, 
           cov_buffer1=bldg_cov_rd_12,     cov_buffer2=bldg_cov_rd_24, #cov_buffer* measures the building covereage in the different zones
           cov_buffer3=bldg_cov_rd_36,     cov_buffer4=bldg_cov_rd_51,
           ht_buffer1=ht_avg_dist_rd_12,   ht_buffer2=ht_avg_dist_rd_24, #ht_buffer* measures the average height of the building in different zones
           ht_buffer3=ht_avg_dist_rd_36,   ht_buffer4=ht_avg_dist_rd_51,
           htmax_buffer1=ht_max_dist_rd_12,   htmax_buffer2=ht_max_dist_rd_24, #htmax_buffer* measures the maximum height of the building in different zones
           htmax_buffer3=ht_max_dist_rd_36,   htmax_buffer4=ht_max_dist_rd_51)
         )
}


# Creating the distance features for non-specific, specific and test data

non_spec_dist_stats<-mapply(create_dist_stats, json_non_specific$source_grid_path, json_non_specific$building_grid_path)
spec_dist_stats<-mapply(create_dist_stats, json_specific$source_grid_path, json_specific$building_grid_path)
test_dist_stats<-mapply(create_dist_stats, json_test$source_grid_path, json_test$building_grid_path)



# Switching the columns and the rows

non_spec_dist_stats_t<-t(non_spec_dist_stats)
spec_dist_stats_t<-t(spec_dist_stats)
test_dist_stats_t<-t(test_dist_stats)
