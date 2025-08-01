#Read in locality data
library(ggplot2)
library(tidyverse)
loc <-read.csv("Sample_locations.csv", header = T, sep = ",", stringsAsFactors = T) 
view(loc)

world <- map_data("world")
p <- loc %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y=lat, group=group), colour="black",fill="lightgrey", alpha=2) +
  coord_sf(xlim = c(-70,-60), ylim = c(42,50)) +
  geom_point(data=loc, aes(x=Long, y=Lat, fill = Population, shape = Location, size=7)) +
  scale_fill_manual(values = c("Common" = "green4", "Both" = "deepskyblue3", "Quebec" = "darkorchid")) +
  scale_shape_manual(values = c("Canal_Lake" = 21, "Cherry_Burton_Road" = 24, "Rainbow_Haven" = 23, "Baie_de_L_Isle_Verte" = 25, "Baddeck" = 22, "Overton" = 20))+
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude")

p + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position="none")

#Add labels and shape details elswhere 
