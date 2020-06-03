#######################

# Spacemaker Noise Surrogate Model

#######################

#####################
# Packages & Libraries
###################

# Installing and reading packages

# install.packages("jsonlite")
# install.packages("stringr")
# install.packages("tidyr")
# Installing and reading packages
# install.packages("ggplot2")       ## ggplot
# install.packages("RcppCNPy")      ## for npyload
# install.packages("Matrix")        ## for nnzero
# install.packages("dplyr")         ## to use %>% 
#install.packages("caret")
library(caret)

library(ggplot2)      
library(RcppCNPy)      
library(Matrix)        
library(dplyr)         

library(jsonlite)       ## for json read
library(stringr)        ## for str split
library(tidyr)          ## for spread function



######################################
#  PART A: Read Data
#####################################

#PART 1: 

#Reading the non-specific training data

json1<-fromJSON(txt="data/non_specific_training_data.json")


# Using the unlist function to change the list format and to add the data as rows
json1<-data.frame(number = unlist(json1))



# Also gives rownames that have all the strings attached by "."
# Using strsplit by . and transform into columns

json1$scenario<-lapply(strsplit(as.character(rownames(json1)),"\\."), "[", 1)
json1$id<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 2)
json1$path<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 3)
rownames(json1)<-NULL


# Use spread function from tidyr to change rows into columns by specific to each entry

json_non_specific<-spread(json1, path, number)
json_non_specific$fraction_yellow_zone <- as.numeric(as.character(json_non_specific$fraction_yellow_zone))


# PART 2: 

# Reading the specific training data

json2<-fromJSON(txt="data/specific_training_data.json")
json2<-data.frame(number = unlist(json2))
json2$id<-lapply(strsplit(as.character(rownames(json2)),"\\."), "[", 1)
json2$path<-lapply(strsplit(as.character(rownames(json2)), "\\."), "[", 2)
rownames(json2)<-NULL
json_specific<-spread(json2, path, number)
json_specific$fraction_yellow_zone <- as.numeric(as.character(json_specific$fraction_yellow_zone))


# PART 3: 

# Reading the test data

json3<-fromJSON(txt="data/test_data.json")
json3<-data.frame(number = unlist(json3))
json3$id<-lapply(strsplit(as.character(rownames(json3)),"\\."), "[", 1)
json3$path<-lapply(strsplit(as.character(rownames(json3)), "\\."), "[", 2)
rownames(json3)<-NULL
json_test<-spread(json3, path, number)
json_test$fraction_yellow_zone <- as.numeric(as.character(json_test$fraction_yellow_zone))


rm(json1, json2, json3)

#####################################
#  PART B: Data Exploration
#####################################
json_non_specific$scenario<-as.character(json_non_specific$scenario)
json_specific$scenario<-as.character(json_specific$scenario)

p<-json_non_specific%>%
  group_by(scenario)%>%
  summarize(scenario=first(scenario),avg_frac=mean(fraction_yellow_zone))%>%arrange(scenario)

plot(p$avg_frac, xlab="Scenario 1 to 9 in order", 
     ylab="Average Fraction Yellow Zone", 
     main="Fraction Yellow Zone across different Scenarios",
     col = "dark red", cex=1, ylim=c(0,1))

p1<-json_non_specific%>%
  filter(scenario %in% c("scenario_6", "scenario_9"))%>%
  summarize(scenario=scenario, fraction=fraction_yellow_zone)
p1$n<-c(1:500, 1:500)
#p1$scenario<-as.character(p1$scenario)
p1%>%
  ggplot(aes(n, fraction))+
  geom_point(aes(col=scenario))+
  xlab("Different Buildings")+
  ylab("Fraction Yellow Zone")+
  ggtitle("Fraction Yellow Zone for Scenarios 6 & 9")+
  theme(legend.position="bottom")

#    Site configurations

fraction_summary<- json_non_specific%>%
  group_by(scenario)%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac])
fraction_summary_spec<- json_specific%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac])

create_maps <- function(x, y, z) {
  input_path1 <- paste("data/", x, sep="")                       #create the path
  print(input_path1)
  srcdata <- npyLoad(input_path1, "integer") # open the file
  src_data<-ifelse(srcdata==85, 25, 0)
  
  input_path2 <- paste("data/", y, sep="")                       
  print(input_path2)
  max_bldg_data <- npyLoad(input_path2, "integer") 
  
  input_path3 <- paste("data/", z, sep="")                       
  print(input_path3)
  min_bldg_data <- npyLoad(input_path3, "integer") 
  
  image(src_data+max_bldg_data, main="Grid area for max fraction yellow zone")
  image(src_data+min_bldg_data, main="Grid area for min fraction yellow zone")
  return("Done")
}

mapply(create_maps, fraction_summary$src, fraction_summary$bldg_max_npy, fraction_summary$bldg_min_npy)
mapply(create_maps, fraction_summary_spec$src, fraction_summary_spec$bldg_max_npy, fraction_summary_spec$bldg_min_npy)




#####################################
#  PART C: Create  Features
#####################################


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
  ht_min_dist_rd<-mean(bldg_file[D==min_dist_rd]) # height of closest building to road
  
  
  
  
  # get building coverage for 4 buffer zones to portray density from road
  
  bldg_cov_rd_12<-length(D[D>0 & D<=12])/length(bldg_file)           # Zone 1 for distance less than 12 units
  bldg_cov_rd_24<-length(D[D>12 & D<=24])/length(bldg_file)    # Zone 2 for distance between 12 and 24 units
  bldg_cov_rd_36<-length(D[D>24 & D<=36])/length(bldg_file)    # Zone 3 for distance between 24 and 36 units
  bldg_cov_rd_51<-length(D[D>36])/length(bldg_file)            # Zone 4 for distance greater than 36 units
  
  
  
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

##########################################################################

# Function to create coverage of road and coverage of building

create_coverage <- function(x) {
  input_path <- paste("data/", x, sep="")                       #create the path
  print(input_path)
  sample_input <- npyLoad(input_path, "integer")                 # open the file
  sample_fraction <- nnzero(sample_input)/length(sample_input)   #calculate the coverage
  return(sample_fraction)
}



# Function to create building features as a whole

create_bldg_stats <- function(x) {
  input_path <- paste("data/", x, sep="")
  print(input_path)
  sample_input <- npyLoad(input_path, "integer")
  bldg_avg<-mean(sample_input[sample_input>0])            # calculate average building height(excludes the zeros)
  bldg_median<-median(sample_input[sample_input>0])       # calculate median building height(excludes the zeros)
  bldg_max<-max(sample_input)                             # calculate max building height(excludes the zeros)
  bldg_min<-min(sample_input[sample_input>0])             # calculate min building height(excludes the zeros)
  bldg_diff<-bldg_max-bldg_min                            # calculate difference of max and min building height
  c(bldg_avg=bldg_avg, bldg_median=bldg_median,           # return the aggregates as a vector
    bldg_max=bldg_max, bldg_min=bldg_min, bldg_diff=bldg_diff)
}




# Function to create fetaures for multiple datasets(non-specific, specific and test data)

create_stats_vec<- function(set, src, bldg) {
  set$cov_rd <- sapply(set[,src],  create_coverage)                # get road coverage
  set$cov_bldg <- sapply(set[,bldg], create_coverage)              # get building coverage
  set$cov_ratio<-as.numeric(set$cov_bldg)/as.numeric(set$cov_rd)   # ratio of building coverage to road coverage
  set_stats<-sapply(set[,bldg], create_bldg_stats)                 # get building stats
  set_stats_t<-t(set_stats)                                        # switching columns and rows
  set_final<-cbind(set, set_stats_t)                               #merge building stats 
  return(set_final) 
}



# Adding the features above to the non-specific, specific and test data

non_spec_final<-create_stats_vec(json_non_specific, "source_grid_path", "building_grid_path")
spec_final<-create_stats_vec(json_specific, "source_grid_path", "building_grid_path")
test_final<-create_stats_vec(json_test, "source_grid_path", "building_grid_path")



# Combining the distance features from the distance dile to non-specific, specific and test data

non_spec_final_set<-cbind(non_spec_final, non_spec_dist_stats_t)
spec_final_set<-cbind(spec_final, spec_dist_stats_t)
test_final_set<-cbind(test_final, test_dist_stats_t)


rm(non_spec_final, spec_final, test_final, 
   non_spec_dist_stats, spec_dist_stats, test_dist_stats, 
   non_spec_dist_stats_t, spec_dist_stats_t, test_dist_stats_t)

#########################################################################################



#####################################
#  PART D: Feature Exploration
#####################################


non_spec_final_set%>%
  ggplot(aes(cov_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Noise coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Noise Coverage")

non_spec_final_set%>%
  ggplot(aes(cov_bldg, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Building coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Building Coverage")

non_spec_final_set%>%
  ggplot(aes(avg_dist_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Average distance to road")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average Distance to Road")

spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, cov_bldg, col="Building"))+
  geom_point()+
  geom_point(aes(fraction_yellow_zone, cov_rd, col="Noise"))+
  ylab("Building/Noise coverage")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Coverage of Specific Site")+
  theme(legend.position = "bottom")

spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, avg_dist_rd))+
  geom_point(col="blue")+
  ylab("Average distance to road")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average distance to road of Specific Site")

##### Need to check the column names
cor_table<-cor(non_spec_final_set[,c(5:28)])
cor_table_frac<-as.data.frame(cor(non_spec_final_set[,c(6:28)], non_spec_final_set[,5]))
cor_table_frac$feature<-rownames(cor_table_frac)
rownames(cor_table_frac)<-NULL
colnames(cor_table_frac)<-c("cor_frac","feature")

cor_table_frac%>%
  ggplot(aes(factor(feature), cor_frac))+
  geom_point(col="dark red")+
  xlab("Features")+
  ylab("Correlation")+
  ggtitle("Correlation across different features")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor_table, col=col, symm=TRUE)

# Listing correlations by absolute value
cor_table_frac%>%arrange(desc(abs(cor_frac)))


#####################################
#  PART E: Create Train/Test Sets
#####################################


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

#rm(t_index, set_x, set_y, col, p, p1, fraction_summary, fraction_summary_spec, cor_table, cor_table_frac)
