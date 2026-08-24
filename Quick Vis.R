# Quick Vis
library(tidyverse)
library(leaflet)
library(viridisLite)
library(sf)


lateral_flux_stations <- read_csv("LateralFluxStations.csv")
print(lateral_flux_stations)
i <- 5

temp_site_id <- lateral_flux_stations$site_id[i]

# Load up watershed
watershed <- terra::rast(paste0("data/GIS/DEM_analysis/",
                                temp_site_id, "/", temp_site_id,
                                "_flume_watershed.tif"))

footprint <- terra::rast(paste0("data/GIS/DEM_analysis/",
                                temp_site_id, "/", temp_site_id,
                                "_footprint.tif"))
  
# Load up snapped pour point
pour_point <- read_sf(paste0("data/GIS/DEM_analysis/",
                             temp_site_id, "/", temp_site_id,
                             "_flume_snapped.shp"))


streams <- terra::rast(paste0("data/GIS/DEM_analysis/",
                              temp_site_id, "/", temp_site_id,
                              "_streams.tif"))

pal <- colorNumeric(
  palette = inferno(100),
  domain = as.vector(footprint$mean),    # replace with your vector of values
  na.color = "transparent"
)


leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
  addRasterImage(watershed, colors = "white", opacity = 0.8) %>% 
  addRasterImage(footprint, colors = pal, opacity = 0.8) %>% 
  addRasterImage(streams, colors = "blue", opacity = 0.8) %>% 
  addCircleMarkers(data = pour_point)
# addRasterImage(streams, colors = "blue", opacity = 0.8)