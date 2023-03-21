# plot dots on map modifeied from: # https://austinwehrwein.com/post/solareclipse/
rm(list=ls())

# install.packages(c("maps", "mapdata"))
# install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
# devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)

usa =    map_data("usa") # we already did this, but we can do it again
states = map_data("state")
library(tidyverse)
library(fuzzyjoin)
library(hrbrthemes)
library(raster)
library(rgdal)

# monitoring sites
infile = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS\\Map\\EclipseTimesMap.csv"
df = read.table(infile,header=TRUE,sep = ",")
df$Lat
labs <- data.frame (
  long = df$Lon,
  lat =  df$Lat,
  names = df$SerialNumber,
  site = df$Park,
  stringsAsFactors = FALSE
)  

# eclipse path
efile = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\ANALYSIS\\Map\\eclipse2017_shapefiles\\center17.shp"
path.m <- shapefile(efile) 
path.points<-fortify(path.m)

#SIMPLE MAP- ALL US
gg1 <- ggplot() + 
  geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3) +
  geom_line(data=path.points,aes(x=long,y=lat),color='#c52828',size=4,alpha=.25,show.legend = F)+
  geom_line(data=path.points,aes(x=long,y=lat),color='#c52828',size=1,alpha=.75,show.legend = F) +
  geom_point(data = labs, aes(x = long, y = lat), color = "black",  size = 1) +
  geom_point(data = labs, aes(x = long, y = lat), color = "orange", size = 1) +
  theme_ipsum(grid=F,plot_title_family = 'Slabo 27px',
              plot_title_face = 'bold',subtitle_size = 10,base_family = 'Roboto Condensed')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.ticks.y=element_blank())
  #xlim(-135,-65)+ylim(20,50)
gg1 


# SIMPLE MAP- WITH ALL SITES and names
gg2 = ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3) + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black",  size = 1) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 1) +
  geom_text( data = labs, aes(x = long +.2, y = lat,  label = site), angle = 0, 
             hjust = 0, color = "black", size = 2)+
  geom_line(data=path.points,aes(x=long,y=lat),color='#c52828',size=4,alpha=.25,show.legend = F)+
  geom_line(data=path.points,aes(x=long,y=lat),color='#c52828',size=1,alpha=.75,show.legend = F)
gg2

ggsave(g2g2, file="SimpleMAP4.png", width = 14, height = 10, units = "cm",dpi = 300,bg = "transparent")

#SAVE OUT PLOTS/MAPS
wrkdir2 = "F:\\RESEARCH\\NSNSD_EclipseSoundScape\\Map\\ANALYSIS"
setwd(wrkdir2)
ggsave(gg1, file="SimpleMAP4.png", width = 14, height = 10, units = "cm",dpi = 300,bg = "transparent")
