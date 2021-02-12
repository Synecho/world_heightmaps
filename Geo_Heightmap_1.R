rm(list = ls(all=T))
library(sf)
library(raster)
library(dplyr)
library(spData)
#install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)
library(tmap)    
library(tmaptools)
library(leaflet) 
library(ggplot2) 

setwd("D:/STLs/Countries_Topography/Heightmaps")


##europe
t1 <- as.data.frame(ccodes() )
t1 <- filter(t1, continent == "Europe")
EU <- t1$ISO3
EU <- EU[! EU %in% c("ALA", "GIB", "MCO")]  



for (i in 1:length(EU)){
  if (i == 1)
  print(EU[i])
  getData('GADM',country=EU[i],level=1)
}  





##### Heightmap Generation
for (i in 1:length(EU)){
  
  print(paste("Generating heightmap of: ", as.character(EU[i,2])," (", as.character(EU[i,1]), ")", sep = ""))
  
  c = getData(name = "GADM", country = as.character(EU[i,1]), level=1)
  c_elev = getData(name = "alt", country = as.character(EU[i,1]), level=1)
  ##### Set counting, setting k
  n.sets = 1
  if(class(c_elev) == "list"){n.sets = length(c_elev)}
  
  for(k in 1:n.sets){
    max.a = max(values(c_elev[[k]]), na.rm = T)
    max.a = ifelse(max.a <= 2000, 2000, max.a)
    
    map = tm_shape(c, bbox = st_bbox(c_elev[[k]])) +
      tm_polygons(col = "white", border.col = "white", border.alpha = 0, alpha = 0) + 
      tm_shape(c_elev[[k]]) + 
      tm_raster(alpha = 1, legend.show = F, palette = c("#222222", "#EEEEEE"),
                breaks = seq(from = 0, to = max.a+(.4*max.a), by = ifelse(max.a <= 4000, 0.5, 1))) +
      tm_layout(frame = FALSE, bg.color = "black")
    
    tmap_save(tm = map,
              filename = paste(gsub(" ", " ", as.character(EU[i,2])), k, "heightmap_v3.png", sep = "_"),
              units = "px", width = 8192, height = 8192, dpi = 600)
    
  }
}


##### Cropping white line from botom of images
# copy all .png files to  seperate folder... that helps things 
library(magick)
setwd("D:/STLs/Countries_Topography/PNGs")
png.files = dir()

for(i in 1:length(png.files)){
  print(png.files[i])
  img = image_read(png.files[i])
  img.c = image_crop(img, "8000x8000")
  image_write(img.c, path = png.files[i], format = "png")
}
