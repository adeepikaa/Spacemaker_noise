# Using the non-specific data to make plots of some of the 9 scenarios


json_non_specific$scenario<-as.character(json_non_specific$scenario)

p <- json_non_specific %>%
  group_by(scenario) %>%
  summarize(scenario=first(scenario),avg_frac = mean(fraction_yellow_zone), .groups='drop') %>%
  arrange(scenario)



# Plotting fraction yellow zone across different scenarios and saving the plots into a new directory 
# called graphs


p<-json_non_specific%>%
  group_by(scenario)%>%
  summarize(avg_frac=mean(fraction_yellow_zone), .groups='drop')%>%arrange(scenario)


jpeg('graphs/frac_scenarios.jpg')
ggplot(p,aes(scenario,avg_frac, fill = scenario))+
  geom_col()+
  xlab("")+
  ylab("Average Fraction Yellow Zone")+
  ggtitle("Fraction Yellow Zone Across Different Scenarios")+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1, vjust = 1))
dev.off()




# Plotting buildings against fraction yellow zone across scenarios 6 and 9 


p1<-json_non_specific%>%
  filter(scenario %in% c("scenario_6", "scenario_9"))%>%
  select(scenario, fraction_yellow_zone)
p1$n<-c(1:500, 1:500)



jpeg('graphs/frac_bldgs.jpg')

p1 %>%
  ggplot(aes(n, fraction)) +
  geom_point(aes(col=scenario)) +
  xlab("Different Buildings") +
  ylab("Fraction Yellow Zone") +
  ggtitle("Fraction Yellow Zone for Scenarios 6 & 9") +
  theme(legend.position="bottom")
dev.off()



#   Site configurations using the non-specific dataset

fraction_summary<- json_non_specific %>%
  group_by(scenario) %>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac],
            .groups='drop')

#   Site configurations using the specific dataset

fraction_summary_spec<- json_specific%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac], 
            .groups='drop')

#   Site configurations using the test dataset
fraction_summary_test<- json_test%>%
  summarise(max_frac=max(fraction_yellow_zone), min_frac=min(fraction_yellow_zone), 
            src=source_grid_path[fraction_yellow_zone==max_frac], 
            bldg_max_npy=building_grid_path[fraction_yellow_zone==max_frac], 
            bldg_min_npy=building_grid_path[fraction_yellow_zone==min_frac], 
            .groups='drop')




# Creating a function to create grid areas of minimum and maximum fraction yellow zones


create_maps <- function(x, y, z, i) {
  input_path1 <- paste("data/", x, sep="")         #create the path
  print(input_path1)
  srcdata <- npyLoad(input_path1, "integer")       # open the file
  src_data<-ifelse(srcdata==85, 25, 0)
  
  input_path2 <- paste("data/", y, sep="")                       
  print(input_path2)
  max_bldg_data <- npyLoad(input_path2, "integer") 
  
  input_path3 <- paste("data/", z, sep="")                       
  print(input_path3)
  min_bldg_data <- npyLoad(input_path3, "integer") 
  
  max_file_name<-paste("graphs/maxplot_", i, ".jpeg", sep="")
  jpeg(max_file_name)
  image(src_data+max_bldg_data, main="Grid area for max fraction yellow zone")
  dev.off()
  
  min_file_name<-paste("graphs/minplot_", i, ".jpeg", sep="")
  jpeg(min_file_name)
  image(src_data+min_bldg_data, main="Grid area for min fraction yellow zone")
  dev.off()
  
  return("Done")
}


# i is used to create the names of the images saved

mapply(create_maps, fraction_summary$src, fraction_summary$bldg_max_npy, fraction_summary$bldg_min_npy, fraction_summary$scenario)
i<-10
mapply(create_maps, fraction_summary_spec$src, fraction_summary_spec$bldg_max_npy, fraction_summary_spec$bldg_min_npy, i)
i<-11
mapply(create_maps, fraction_summary_test$src, fraction_summary_test$bldg_max_npy, fraction_summary_test$bldg_min_npy, i)


