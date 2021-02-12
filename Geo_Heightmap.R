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

#data("World")
World.list = getData('ISO3')
##### Excluding a few regions....
out = c(2,9:10,14,29,32,34:35,44,50,51,52,55,57,60,75,81,88,92,95,97,100,110,115,119,120,137,139:140,144,148,151,156,164,165,167,176,180,190,191,203,205,206,212,216,231,237,248)
World.list = World.list[-out,]

####################################################
#data DEU, CHE
World.list <- World.list[c(86),]



##### Heightmap Generation
for (i in 1:nrow(World.list)){
  
  print(paste("Generating heightmap of: ", as.character(World.list[i,2])," (", as.character(World.list[i,1]), ")", sep = ""))
  
  c = getData(name = "GADM", country = as.character(World.list[i,1]), level=3)
  c_elev = getData(name = "alt", country = as.character(World.list[i,1]), level=3)
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
                breaks = seq(from = 0, to = max.a+(.4*max.a), by = max.a/40)) +
      tm_layout(frame = FALSE, bg.color = "black")
    
    tmap_save(tm = map,
              filename = paste(gsub(" ", " ", as.character(World.list[i,2])), k, "heightmap_v3.svg", sep = "_"),
              units = "px", width = 8192, height = 8192, dpi = 600)
    
  }
}


##### Cropping white line from botom of images
# copy all .png files to seperate folder... that helps things 
library(magick)
setwd("D:/STLs/Countries_Topography/PNGs")
png.files = dir()

for(i in 1:length(png.files)){
  print(png.files[i])
  img = image_read(png.files[i])
  img.c = image_crop(img, "8000x8000")
  image_write(img.c, path = png.files[i], format = "png")
}
