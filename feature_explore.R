# Section 1 of 2: Plots

# Using the non-specific final dataset to create plots of noise coverage, building coverage  
# and average distance to road with fraction yellow zone 


jpeg('graphs/frac_covrd.jpg')
non_spec_final_set%>%
  ggplot(aes(cov_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Noise coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Noise Coverage")
dev.off()


jpeg('graphs/frac_covbldg.jpg')
non_spec_final_set%>%
  ggplot(aes(cov_bldg, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Building coverage")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Building Coverage")
dev.off()

jpeg('graphs/frac_dist.jpg')
non_spec_final_set%>%
  ggplot(aes(avg_dist_rd, fraction_yellow_zone, col=scenario))+
  geom_point()+
  xlab("Average distance to road")+
  ylab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average Distance to Road")
dev.off()

jpeg('graphs/frac_spec.jpg')
spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, cov_bldg, col="Building"))+
  geom_point()+
  geom_point(aes(fraction_yellow_zone, cov_rd, col="Noise"))+
  ylab("Building/Noise coverage")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Coverage of Specific Site")+
  theme(legend.position = "bottom")
dev.off()


jpeg('graphs/frac_spec_dist.jpg')
spec_final_set%>%
  ggplot(aes(fraction_yellow_zone, avg_dist_rd))+
  geom_point(col="blue")+
  ylab("Average distance to road")+
  xlab("Fraction ")+
  ggtitle("Fraction Yellow Zone Vs Average distance to road of Specific Site")
dev.off()



# # Section 2 of 2: Correlation table and plots


cor_table<-cor(non_spec_final_set[,c(5:28)])
cor_table_frac<-as.data.frame(cor(non_spec_final_set[,c(6:28)], non_spec_final_set[,5]))
cor_table_frac$feature<-rownames(cor_table_frac)
rownames(cor_table_frac)<-NULL
colnames(cor_table_frac)<-c("cor_frac","feature")


jpeg('graphs/cor_frac.jpg')
cor_table_frac%>%
  ggplot(aes(factor(feature),cor_frac, colour=cor_frac))+
  geom_point(size =4)+
  scale_color_gradient2(cor_table_frac$cor_frac, low= "blue", mid="white", high= "red")+
  theme(panel.background = element_rect(fill = "slategrey",
                                        colour = "slategrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")
  )+
  theme(legend.title = element_blank())+
  labs(fill = "Correlation")+
  xlab("Features")+
  ylab("Correlation")+
  ggtitle("Correlation Across Different Features")+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 10, angle = 45, hjust = 1))
dev.off()

col<- colorRampPalette(c("blue", "white", "red"))(20)
jpeg('graphs/cor_heat.jpg')
heatmap(cor_table, col=col, symm=TRUE)
dev.off()


# Listing correlations by absolute value

#cor_table_frac%>%arrange(desc(abs(cor_frac)))

