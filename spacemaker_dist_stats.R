create_dist_stats <- function(x,y) {
  src_path <- paste("data/", x, sep="")
  print(src_path)
  src_file <- npyLoad(src_path, "integer")
  bldg_path <- paste("data/", y, sep="")
  print(bldg_path)
  bldg_file <- npyLoad(bldg_path, "integer")
  arr<-NULL
  for (i in 1:51){
    for (j in 1:51) {
      if (src_file[i,j] > 0) {
        arr<-rbind(arr, c(i,j))
      }   
    }
  }
  
  distance<-NULL
  bldg_cnt<-0
  D<-matrix(0, ncol=ncol(bldg_file), nrow=nrow(bldg_file))
  for (i in 1:51){
    for (j in 1:51) {
      if (bldg_file[i,j] > 0) {
        for (k in 1:nrow(arr)) {
          distance[k]<-sqrt((arr[k,1]-i)^2+(arr[k,2]-j)^2)
        }
        D[i,j]<-min(distance)
      }
    }
  }
  avg_dist_rd<-mean(D[D>0])
  min_dist_rd<-min(D[D>0])
  ht_min_dist_rd<-min(bldg_file[D==min_dist_rd])
  
  bldg_cov_rd_12<-sum(D[D<=12])/length(bldg_file)       # Zone 1 for distance less than 12 units
  bldg_cov_rd_24<-sum(D[D>12 & D<=24])/length(bldg_file) # Zone 2 for distance between 12 and 24 units
  bldg_cov_rd_36<-sum(D[D>24 & D<=36])/length(bldg_file) # Zone 3 for distance between 24 and 36 units
  bldg_cov_rd_51<-sum(D[D>36])/length(bldg_file)        # Zone 4 for distance greater than 36 units
  
  ht_avg_dist_rd_12<-ifelse(bldg_cov_rd_12!=0, mean(bldg_file[D>0 & D<=12]), 0)
  ht_avg_dist_rd_24<-ifelse(bldg_cov_rd_24!=0, mean(bldg_file[D>12 & D<=24]), 0)
  ht_avg_dist_rd_36<-ifelse(bldg_cov_rd_36!=0, mean(bldg_file[D>24 & D<=36]), 0)
  ht_avg_dist_rd_51<-ifelse(bldg_cov_rd_51!=0, mean(bldg_file[D>36]), 0)
  
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

