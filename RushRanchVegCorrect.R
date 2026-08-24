# Veg correct RR
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
  filter(site_id == "Rush Ranch")

md_dem <- rast(dem_list[3])

# Upload ground data with errors
error_points <- error_points %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  # project
  st_transform(st_crs(md_dem)) 

# Upload veg classes
veg_classes <- read_sf('/Volumes/LaCie/COMPASS_hydrology/PreppedRasters/RushRanch/Vegetation_-_Suisun_Marsh_-_2024_[ds3221].shp') %>% 
  st_make_valid() %>% 
  st_transform(st_crs(md_dem)) %>% 
  filter(Habitat == "Tidal")

veg_classes <- veg_classes %>% 
  filter(! MapClass %in% c("Open Water Mapping Unit",
                           "Slough Mapping Unit"
                           ))

  # Project

md_dem2 <- terra::crop(md_dem, ext(veg_classes))

# Spatial join between sf and points
error_point_veg_join_1 <- error_points %>% 
  st_join(veg_classes) %>% 
  filter(complete.cases(MapClass, elevation_error))
  
error_point_veg_join <- error_point_veg_join_1 %>% 
  # Calculate mean error
  group_by(MapClass) %>% 
  summarise(mean_error = mean(elevation_error),
            sd_error = sd(elevation_error),
            n = n()) %>% 
  st_drop_geometry() %>% 
  arrange(mean_error)

# unsampled_types <- veg_classes %>% 
#   st_filter()
#   filter(! MapClass %in% error_point_veg_join$MapClass) %>% 
#   dplyr::select(MapClass) %>% 
#   st_drop_geometry() %>% 
#   distinct_all()

error_point_veg_join_1_plot <- error_point_veg_join_1 %>% 
  # bind_rows(unsampled_types) %>% 
  mutate(MapClass = factor(MapClass, levels = c(error_point_veg_join$MapClass)))

ggplot(error_point_veg_join_1_plot, aes(x = MapClass, y = elevation_error)) +
  geom_jitter() +
  geom_boxplot(fill = NA) +
  theme(axis.text.x = element_text(angle = 20, hjust=1))

lm1 <- lm(elevation_error~MapClass, data = error_point_veg_join_1)
summary(lm1)




# Filter
veg_classes_w_errors <-veg_classes %>% 
  # Left join
  left_join(error_point_veg_join) # %>% 
  # mutate(mean_error = case_when(spp == "BWALK"~NA,
  #                               spp == "FORE*"~0,
  #                               .default = mean_error
  # ))


cent <- st_coordinates(st_centroid(veg_classes_w_errors))

nb <- st_touches(veg_classes_w_errors)

for (i in which(is.na(veg_classes_w_errors$mean_error))) {
  
  neigh <- nb[[i]]
  vals <- veg_classes_w_errors$mean_error[neigh]
  
  keep <- !is.na(vals)
  
  if (any(keep)) {
    
    neigh <- neigh[keep]
    vals  <- vals[keep]
    
    d <- sqrt((cent[neigh,1] - cent[i,1])^2 +
                (cent[neigh,2] - cent[i,2])^2)
    
    w <- 1 / d
    
    veg_classes_w_errors$mean_error[i] <- weighted.mean(vals, w)
    
  }
}

pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")), domain = range(veg_classes_w_errors$mean_error, na.rm =T),
                                         na.color = "transparent")

veg_classes_w_errors_wgs <- veg_classes_w_errors %>% 
  st_transform("WGS84")

leaflet(veg_classes_w_errors) %>% 
  # addTiles() %>%
  addPolygons(color = ~pal(mean_error), fillOpacity = 0.9, popup = ~MapClass)

# bwalk_shape <- veg_classes %>% 
#   left_join(error_point_veg_join) %>% 
#   filter(spp == "BWALK") %>% 
#   mutate(dummy_int = 1)

# Rasterize
correction_map <- rasterize(x = veg_classes_w_errors, y = md_dem2,
                            field = "mean_error",
                            background = 0,
                            fun = "mean",
                            na.rm = T
)

# bwal_rast <- rasterize(x = bwalk_shape, y = md_dem2,
#                        field = "dummy_int",
#                        background = NA,
#                        fun = "max"
# )

# correction_map_fill <- focal(correction_map, w = 3, fun = "median", na.rm = TRUE)
# correction_map[!is.na(bwal_rast)] <- correction_map_fill[!is.na(bwal_rast)]
correction_map[is.na(correction_map)] <- 0
# # Subtract
# pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")), values(correction_map),
#                     na.color = "transparent")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(correction_map, colors = pal)
plot(correction_map)
writeRaster(correction_map, "/Volumes/LaCie/COMPASS_hydrology/VegCorrection/RushRanchVegCorrect.tif",
            overwrite=T)

# Clip out infrastruture

# Fill in gap