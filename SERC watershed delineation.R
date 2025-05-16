# Example watershed delineation for SERC

# from the following lesson: https://vt-hydroinformatics.github.io/rgeowatersheds.html
library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
library(leaflet)
library(terra)

whitebox::wbt_init()
theme_set(theme_classic())


dem <- raster("data/GIS/GCREW_elevation_3m.tif")

pal <- colorNumeric(palette = "PuOr", values(dem),
                    na.color = "transparent")

# leaflet() %>% addTiles() %>%
#   addRasterImage(dem, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(dem),
#             title = "Elv (m)")

# wbt_hillshade(dem = "data/GIS/GCREW_elevation.tif",
#               output = "data/GIS/GCREW_hillshade.tif",
#               azimuth = 115)
# 
# hillshade <- raster("data/GIS/GCREW_hillshade.tif")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(hillshade, opacity = 0.8)
# 
# Fill depressions
wbt_breach_depressions_least_cost(
  dem = "data/GIS/GCREW_elevation_3m.tif",
  output = "data/GIS/GCREW_breached.tif",
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = "data/GIS/GCREW_breached.tif",
  output = "data/GIS/GCREW_breached_filled.tif"
)

wbt_d8_flow_accumulation(input = "data/GIS/GCREW_breached_filled.tif",
                         output = "data/GIS/GCREW_D8FA.tif")

wbt_d8_pointer(dem = "data/GIS/GCREW_breached_filled.tif",
               output = "data/GIS/GCREW_pointer.tif")

ppoints <- tribble(
  ~Lon, ~Lat,
  -76.546502, 38.874862
)

ppointsSP <- SpatialPoints(ppoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

shapefile(ppointsSP, filename = "data/GIS/GCREW_flume.shp", overwrite = TRUE)

wbt_extract_streams(flow_accum = "data/GIS/GCREW_D8FA.tif",
                    output = "data/GIS/GCREW_streams.tif",
                    threshold = 100)

wbt_jenson_snap_pour_points(pour_pts = "data/GIS/GCREW_flume.shp",
                            streams = "data/GIS/GCREW_streams.tif",
                            output = "data/GIS/GCREW_flume_snapped.shp",
                            snap_dist = 0.0005) #careful with this! Know the units of your data

pp <- shapefile("data/GIS/GCREW_flume_snapped.shp")
streams <- raster("data/GIS/GCREW_streams.tif")

# leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
#   
#   addRasterImage(dem, colors = pal, opacity = 0.8) %>%
#   addRasterImage(streams, colors = "blue", opacity = 0.8) %>% 
#   addCircleMarkers(color = "black", lat = pp@coords[,"y"], lng = pp@coords[,"x"])

wbt_watershed(d8_pntr = "data/GIS/GCREW_pointer.tif",
              pour_pts = "data/GIS/GCREW_flume_snapped.shp",
              output = "data/GIS/GCREW_flume_watershed.tif")

lil_shed <- raster("data/GIS/GCREW_flume_watershed.tif")

leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
  addRasterImage(lil_shed, colors = "white", opacity = 0.8) %>% 
  addRasterImage(streams, colors = "blue", opacity = 0.8) %>% 
  addCircleMarkers(color = "black", lat = pp@coords[,"y"], lng = pp@coords[,"x"])
  # addRasterImage(streams, colors = "blue", opacity = 0.8)

