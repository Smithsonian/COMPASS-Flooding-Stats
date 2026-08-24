

pp1 <- shapefile(paste0("data/GIS/DEM_analysis/SageLotPond", "/", "SageLotPond", "_flume_snapped.shp"))
pp2 <- shapefile(paste0("data/GIS/DEM_analysis/SageLotPond_alt", "/", "SageLotPond_alt", "_flume_snapped.shp"))

streams <- raster(paste0("data/GIS/DEM_analysis/SageLotPond", "/", "SageLotPond", "_streams.tif"),)

# leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
# 
#   addRasterImage(dem, colors = pal, opacity = 0.8) %>%
#   addRasterImage(streams, colors = "blue", opacity = 0.8) %>%
#   addCircleMarkers(color = "black", lat = pp@coords[,"y"], lng = pp@coords[,"x"])

lil_shed1 <- raster(paste0("data/GIS/DEM_analysis/SageLotPond", "/", "SageLotPond", "_flume_watershed.tif"))
lil_shed2 <- raster(paste0("data/GIS/DEM_analysis/SageLotPond_alt", "/", "SageLotPond_alt", "_flume_watershed.tif"))


library(leaflet)

leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
  addRasterImage(lil_shed1, colors = "pink", opacity = 0.5) %>% 
  addRasterImage(lil_shed2, colors = "lightblue", opacity = 0.5) %>% 
  addRasterImage(streams, colors = "darkblue", opacity = 0.8) %>% 
  addCircleMarkers(color = "black", lat = pp1@coords[,"y"], lng = pp1@coords[,"x"]) %>% 
  addCircleMarkers(color = "black", lat = pp2@coords[,"y"], lng = pp2@coords[,"x"])

# addRasterImage(streams, colors = "blue", opacity = 0.8)