#####################
# Packages & Libraries
###################

# Installing and reading packages

# install.packages("jsonlite")      ## Reading data
# install.packages("stringr")       ## Data wrangling
# install.packages("tidyr")         ## data clean up

# install.packages("ggplot2")       ## ggplot
# install.packages("RcppCNPy")      ## for npyload
# install.packages("Matrix")        ## for nnzero
# install.packages("dplyr")         ## to use %>% 
# install.packages("caret")         ## to partition datasets and train function
# install.packages("kernlab")
# install.packages("tree")
# install.packages("gam")

library(ggplot2)      
library(RcppCNPy)      
library(Matrix)        
library(dplyr)         
library(jsonlite)       ## for json read
library(stringr)        ## for str split
library(tidyr)          ## for spread function

library(caret)
library(kernlab)
library(randomForest)
library(tree)
library(gam)


# Unzip the data file. Code assumes zip file in located in the working directory.
unzip("data.zip")

# Create empty directory to save all plots
dir.create("graphs")



# Section 1 of 3: Reading the non-specific training data


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



# Section 2 of 3:  Reading the specific training data


json2<-fromJSON(txt="data/specific_training_data.json")
json2<-data.frame(number = unlist(json2))
json2$id<-lapply(strsplit(as.character(rownames(json2)),"\\."), "[", 1)
json2$path<-lapply(strsplit(as.character(rownames(json2)), "\\."), "[", 2)
rownames(json2)<-NULL
json_specific<-spread(json2, path, number)
json_specific$fraction_yellow_zone <- as.numeric(as.character(json_specific$fraction_yellow_zone))




# Section 3 of 3:  Reading the test data


json3<-fromJSON(txt="data/test_data.json")
json3<-data.frame(number = unlist(json3))
json3$id<-lapply(strsplit(as.character(rownames(json3)),"\\."), "[", 1)
json3$path<-lapply(strsplit(as.character(rownames(json3)), "\\."), "[", 2)
rownames(json3)<-NULL
json_test<-spread(json3, path, number)
json_test$fraction_yellow_zone <- as.numeric(as.character(json_test$fraction_yellow_zone))


rm(json1, json2, json3)



