# estuary_plot.R#####
#
# Author: Mark Myer
#
# Purpose: To create figures and maps for estuaries and subestuaries in the continental United States
# Revision: March 20, 2019
# R Version 3.5.1 Feather Spray

library(raster)
library(rgdal)
library(mapdata)
library(ggmap)
library(gridExtra)

setwd("O:/Public/Myer/CyAN/")

#Import the estuary and subestuary shapefiles along with outlines for the US and states
ATL_est <- shapefile("./Output/Atlantic/Atlantic_estuary_shore_dist.shp")
ATL_sub <- shapefile("./Output/Atlantic/Subestuaries/Atlantic_subestuary_shore_dist.shp")

GOM_est<- shapefile("./Output/Gulf/Gulf_estuary_shore_dist.shp")
GOM_sub<- shapefile("./Output/Gulf/Subestuaries/Gulf_subestuary_shore_dist.shp")

PAC_est<- shapefile("./Output/Pacific/Pacific_estuary_shore_dist.shp")
PAC_sub<- shapefile("./Output/Pacific/Subestuaries/Pacific_subestuary_shore_dist.shp")

estuaries <- bind(ATL_est, GOM_est, PAC_est)
subestuaries <- bind(ATL_sub, GOM_sub, PAC_sub)
rm(ATL_est, GOM_est, PAC_est,ATL_sub, GOM_sub, PAC_sub)

usa <- map_data("usa")
states <- map_data("state")

#Plot all estuaries and subestuaries 
tiff(filename="./Output/Figure1.tiff", width=8, height=4, units="in", pointsize=12, res=300, compression="lzw")
ggplot(data = states) + 
  geom_polygon(aes(x=long, y=lat, group=group), fill = "grey", color = "black") + 
  labs(x="Longitude", y="Latitude") +
  coord_fixed(1.3) +
  geom_point(data=estuaries@data, aes(x=CENTR_LONG, y=CENTR_LAT, shape="Estuaries"), color="black", size=1.2) +
  geom_point(data=subestuaries@data, aes(x=CENTR_LONG, y=CENTR_LAT, shape="Subestuaries"), fill = "white", color="black", size=1.2) +
  scale_shape_manual(name="",values=c('Estuaries' = 16, "Subestuaries" = 21)) +
  geom_text(x=-77, y=32, label="ATL") +
  geom_text(x=-93, y=28, label="GOM") +
  geom_text(x=-123, y=34, label="PAC") +
  theme_bw() + theme(legend.position = c(0.9, 0.2), legend.background = element_rect(fill="NA"), 
                     panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
                     plot.margin = unit(c(0,1,0,1), "cm")) 
dev.off()

#Plot estuaries and subestuaries resolvable at various resolutions
#1 km 
m_1000 <- ggplot(data = states) + 
          geom_polygon(aes(x=long, y=lat, group=group), fill = "grey", color = "black") + 
          labs(x="Longitude", y="Latitude") +
          coord_fixed(1.3) +
          geom_point(data=subset(estuaries@data, maxwindow > 1000), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Estuaries"), color="black", size=1.2) +
          geom_point(data=subset(subestuaries@data, maxwindow > 1000), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Subestuaries"), fill = "white", color="black", size=1.2) +
          scale_shape_manual(name="",values=c('Estuaries' = 16, "Subestuaries" = 21)) +
          geom_text(x=-77, y=32, label="ATL") +
          geom_text(x=-93, y=28, label="GOM") +
          geom_text(x=-123, y=34, label="PAC") +
          geom_text(x=-126, y=50, label="A", fontface="bold", size = 6) +
          theme_bw() + theme(legend.position = c(0.9, 0.2), legend.background = element_rect(fill="NA"), 
                     panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
                     plot.margin = unit(c(0,1,0,1), "cm")) 
#300m
m_300 <- ggplot(data = states) + 
          geom_polygon(aes(x=long, y=lat, group=group), fill = "grey", color = "black") + 
          labs(x="Longitude", y="Latitude") +
          coord_fixed(1.3) +
          geom_point(data=subset(estuaries@data, maxwindow > 300), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Estuaries"), color="black", size=1.2) +
          geom_point(data=subset(subestuaries@data, maxwindow > 300), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Subestuaries"), fill = "white", color="black", size=1.2) +
          scale_shape_manual(name="",values=c('Estuaries' = 16, "Subestuaries" = 21)) +
          geom_text(x=-77, y=32, label="ATL") +
          geom_text(x=-93, y=28, label="GOM") +
          geom_text(x=-123, y=34, label="PAC") +
          geom_text(x=-126, y=50, label="B", fontface="bold", size = 6) +
          theme_bw() + theme(legend.position = c(0.9, 0.2), legend.background = element_rect(fill="NA"), 
                     panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
                     plot.margin = unit(c(0,1,0,1), "cm")) 

#30m
m_30 <- ggplot(data = states) + 
          geom_polygon(aes(x=long, y=lat, group=group), fill = "grey", color = "black") + 
          labs(x="Longitude", y="Latitude") +
          coord_fixed(1.3) +
          geom_point(data=subset(estuaries@data, maxwindow > 30), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Estuaries"), color="black", size=1.2) +
          geom_point(data=subset(subestuaries@data, maxwindow > 30), aes(x=CENTR_LONG, y=CENTR_LAT, shape="Subestuaries"), fill = "white", color="black", size=1.2) +
          scale_shape_manual(name="",values=c('Estuaries' = 16, "Subestuaries" = 21)) +
          geom_text(x=-77, y=32, label="ATL") +
          geom_text(x=-93, y=28, label="GOM") +
          geom_text(x=-123, y=34, label="PAC") +
          geom_text(x=-126, y=50, label="C", fontface="bold", size = 6) +
          theme_bw() + theme(legend.position = c(0.9, 0.2), legend.background = element_rect(fill="NA"), 
                             panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
                             plot.margin = unit(c(0,1,0,1), "cm")) 

tiff(filename="./Output/Figure3.tiff", width=8, height=12, units="in", pointsize=12, res=300, compression="lzw")
grid.arrange(m_1000, m_300, m_30, ncol=1)
dev.off()