library(jsonlite) ## for json read
library(stringr) ## for str split
library(tidyr) ## for spread function

#json1<-fromJSON(txt="~/Desktop/FSU/industry_project/data/non_specific_training_data.json")
json1<-fromJSON(txt="data/non_specific_training_data.json")
json1<-data.frame(number = unlist(json1))
json1$scenario<-lapply(strsplit(as.character(rownames(json1)),"\\."), "[", 1)
json1$id<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 2)
json1$path<-lapply(strsplit(as.character(rownames(json1)), "\\."), "[", 3)
rownames(json1)<-NULL
json_non_specific<-spread(json1, path, number)
json_non_specific$fraction_yellow_zone <- as.numeric(as.character(json_non_specific$fraction_yellow_zone))


json2<-fromJSON(txt="data/specific_training_data.json")
json2<-data.frame(number = unlist(json2))
json2$id<-lapply(strsplit(as.character(rownames(json2)),"\\."), "[", 1)
json2$path<-lapply(strsplit(as.character(rownames(json2)), "\\."), "[", 2)
rownames(json2)<-NULL
json_specific<-spread(json2, path, number)
json_specific$fraction_yellow_zone <- as.numeric(as.character(json_specific$fraction_yellow_zone))


json3<-fromJSON(txt="data/test_data.json")
json3<-data.frame(number = unlist(json3))
json3$id<-lapply(strsplit(as.character(rownames(json3)),"\\."), "[", 1)
json3$path<-lapply(strsplit(as.character(rownames(json3)), "\\."), "[", 2)
rownames(json3)<-NULL
json_test<-spread(json3, path, number)
json_test$fraction_yellow_zone <- as.numeric(as.character(json_test$fraction_yellow_zone))


