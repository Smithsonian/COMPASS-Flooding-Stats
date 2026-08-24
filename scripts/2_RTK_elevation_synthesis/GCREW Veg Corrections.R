# Veg correct GCREW
library(tidyverse)
library(sf)
library(sp)
library(terra)

# Upload GCREW DEM
dem_list <- list.files("/Volumes/LaCie/COMPASS_hydrology/dem", 
                       full.names = T, 
                       pattern ="*.tif$")

# naip_list <- list.files("data/GIS/NAIP/", recursive = T, full.names = T) 

error_points <- read_csv("elevation_errors_rtk.csv") %>% 
  filter(site_id == "GCW")

md_dem <- rast(dem_list[19])

# Upload ground data with errors
error_points <- error_points %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  # project
  st_transform(st_crs(md_dem)) 

# Upload veg classes
veg_classes <- read_sf("/Volumes/LaCie/COMPASS_hydrology/veg/GCREW/Communities_DissolvedNE.shp") %>% 
  st_make_valid() %>% 
  st_transform(st_crs(md_dem))  # Project

md_dem2 <- terra::crop(md_dem, ext(veg_classes))

# Spatial join between sf and points
error_point_veg_join <- error_points %>% 
  st_join(veg_classes) %>% 
  filter(complete.cases(spp, elevation_error)) %>% 
  group_by(spp) %>% 
  # Calculate mean error
  summarise(mean_error = mean(elevation_error),
            sd_error = sd(elevation_error),
            n = n()) %>% 
  st_drop_geometry()

# Filter
veg_classes_w_errors <-veg_classes %>% 
  # Left join
  left_join(error_point_veg_join) %>% 
  mutate(mean_error = case_when(spp == "BWALK"~NA,
                                spp == "FORE*"~0,
                                .default = mean_error
                                ))

bwalk_shape <- veg_classes %>% 
  left_join(error_point_veg_join) %>% 
  filter(spp == "BWALK") %>% 
  mutate(dummy_int = 1)

# Rasterize
correction_map <- rasterize(x = veg_classes_w_errors, y = md_dem2,
          field = "mean_error",
          background = 0,
          fun = "mean",
          na.rm = T
          )

bwal_rast <- rasterize(x = bwalk_shape, y = md_dem2,
                       field = "dummy_int",
                       background = NA,
                       fun = "max"
)

correction_map_fill <- focal(correction_map, w = 3, fun = "median", na.rm = TRUE)
correction_map[!is.na(bwal_rast)] <- correction_map_fill[!is.na(bwal_rast)]
correction_map[is.na(correction_map)] <- 0
# Subtract
pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")), values(correction_map),
  na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(correction_map, colors = pal)

writeRaster(correction_map, "/Volumes/LaCie/COMPASS_hydrology/VegCorrection/GcrewVegCorrect.tif")

# Clip out infrastruture

# Fill in gap
