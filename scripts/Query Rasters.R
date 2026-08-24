# Read in rasters for NDVI and elevation
library(terra)
library(sf)
library(tidyverse)
library(sp)

# List files
elevation_points <- read_csv("synthesized_studies_geoid12b.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
                      crs = "WGS84")
# st_layers("data/GIS/wetlands/MD_geopackage_wetlands.gpkg")
# 
# MD_wetlands <- st_read("data/GIS/wetlands/MD_geopackage_wetlands.gpkg",
#                        "MD_Wetlands") %>% 
#   filter(WETLAND_TYPE %in% c("Estuarine and Marine Wetland", "Freshwater Emergent Wetland"))
# 
# VA_wetlands <- st_read("data/GIS/wetlands/VA_geopackage_wetlands.gpkg",
#                                       "VA_Wetlands") %>% 
#   filter(WETLAND_TYPE %in% c("Estuarine and Marine Wetland", "Freshwater Emergent Wetland"))

# Create terra 
dem_list <- list.files("/Volumes/LaCie/COMPASS_hydrology/dem", 
                       full.names = T, 
                       pattern ="*.tif$") 

# naip_list <- list.files("data/GIS/NAIP/", recursive = T, full.names = T) 

elevation_points_proj <- elevation_points %>% 
  st_transform(st_crs(rast(dem_list[1]))) 

dem_polys <- lapply(1:length(dem_list), function(i, rast_list = dem_list) { 
  f = rast_list[[i]]
  p <- as.polygons(ext(rast(f))) %>% 
    st_as_sf() %>% 
    mutate(raster_name = rev(str_split(f[[1]], "/")[[1]])[1],
           i = i)

}) 

# naip_polys <- lapply(1:4, function(i, rast_list = naip_list) { 
#   f = rast_list[[i]]
#   p <- as.polygons(ext(rast(f))) %>% 
#     st_as_sf() %>% 
#     mutate(raster_name = rev(str_split(f[[1]], "/")[[1]])[1],
#            i = i)
#   
# }) 

dem_polys_sf <- bind_rows(dem_polys) %>% 
    # st_as_sf() %>% 
    st_set_crs(st_crs(rast(dem_list[1]))) 


leaflet(dem_polys_sf) %>%  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(opacity = 0.4, label = ~raster_name)


# naip_polys_sf <- bind_rows(naip_polys) %>% 
#   # st_as_sf() %>% 
#   st_set_crs(st_crs(rast(dem_list[1]))) 

# CB_wetlands <- MD_wetlands %>% 
#   bind_rows(VA_wetlands) %>% 
#   st_transform(st_crs(rast(dem_list[1])))
  
which_polygons_match_1 <- elevation_points_proj %>%
    st_join(dem_polys_sf)

# which_polygons_match_2 <- elevation_points_proj %>% 
#   st_join(naip_polys_sf) 
  
unique_files <- unique(which_polygons_match_1$raster_name)

# datums_lidar <- read_csv("LiDAR year tidal datums.csv")
# (datums_lidar)  

dem_point_list <- list()
# mask_list <- list()
# wetland_list <- list()
# msl_list <- list(0.0733, (0.0121+0.0433)/2, 0.0581, 0.0289)
# mhw_list <- list(0.147, (0.272+0.252)/2, 0.340, 0.340)

# dem_masked_list <- list()
# dem_wetland_list <- list()


for (j in 1:length(unique_files)) { 
  temp_points <- which_polygons_match_1 %>% 
    dplyr::filter(raster_name == unique_files[j]) 
    
  temp_rast <- rast(dem_list[[temp_points$i[1]]])
  
  # msk <- ifel(temp_rast <= -9999, NA, 1)
  # mask_list[[j]] <- msk
  # 
  # wetland_shape <- CB_wetlands %>% st_crop(ext(temp_rast))
  # wetland_list[[j]] <- wetland_shape
  # 
  #   temp_rast1 <- mask(temp_rast, msk)
  
  # dem_masked_list[[j]] <- temp_rast1
  
  # temp_rast2 <- mask(temp_rast1, wetland_shape)
  
  # dem_wetland_list[[j]] <- temp_rast2

  extract_points <- terra::extract(temp_rast, temp_points) 
  # extract_z <- terra::extract(z_rast, temp_points) 
  
  temp_points_2 <- temp_points %>% 
    mutate(elevation_DEM = extract_points[,2]
           )
  
  dem_point_list[[j]] <- temp_points_2
}

# 
# unique_files_2 <- unique(which_polygons_match_2$raster_name) 
# 
# naip_point_list <- list()
# z_naip_list <- list()

# for (j in 1:length(unique_files_2)) { 
#   
#   temp_points <- which_polygons_match_2 %>% 
#     dplyr::filter(raster_name == unique_files_2[j]) 
#   
#   temp_rast <- rast(naip_list[[temp_points$i[1]]]) 
#   
#   msk <- mask_list[[j]]
#   wetland_shape <- wetland_list[[j]]
#   
#   temp_rast <- mask(temp_rast, msk)
#   temp_rast <- mask(temp_rast, wetland_shape)
#   
#   z_rast <- scale(temp_rast)
#   z_naip_list[[j]] <- z_rast
#   
#   extract_points <- terra::extract(temp_rast, temp_points) 
#   extract_z <- terra::extract(z_rast, temp_points) 
#   
#   temp_points_2 <- temp_points %>% 
#     mutate(naip_ndvi = extract_points$nd,
#            naip_z = extract_z$nd)
#   
#   naip_point_list[[j]] <- temp_points_2
# }

dem_points <- bind_rows(dem_point_list) %>% 
  rename(dem_raster = raster_name) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  filter(elevation_DEM>-99) %>% 
  mutate(site_id = ifelse(study_id == "Mariotti_et_al_2024",
                          "Barataria", site_id))

# naip_points <- bind_rows(naip_point_list) %>% 
#   rename(naip_raster = raster_name) %>% 
#   st_transform(crs = "WGS84") %>% 
#   mutate(latitude = st_coordinates(.)[,2],
#          longitude = st_coordinates(.)[,1],
#   ) %>% 
#   st_drop_geometry() %>% 
#   as_tibble()

all_points_together <- dem_points %>% 
  # left_join(naip_points) %>% 
  mutate(elevation_error = elevation_DEM-elevation_navd88_geoid12b) %>% 
  mutate(position = factor(position, levels = c("water", "wetland", "transition", "swamp", "upland")))

summary(all_points_together$elevation_DEM)

library(plotly)

write_csv(all_points_together, "elevation_errors_rtk.csv")
all_points_together <- read_csv("elevation_errors_rtk.csv")


data_exploration <- ggplot(all_points_together, aes(x = elevation_navd88_geoid12b,
                                                    y = elevation_DEM)) +
  geom_point(aes(shape = position, text = study_id)) +
  facet_wrap(.~site_id, scale = "free") +
  geom_abline(slope = 1, intercept = 0)
(data_exploration)
ggsave("RTK versus Measured Elevation All.jpg", 
       width = 8, 
       height=6)

data_exploration_for_steph <- all_points_together %>% 
  dplyr::filter(site_id %in% c("GCW", "GWI", "SWH")) %>% 
  mutate(site_id = recode(site_id, 
                          "GCW" = "Mesohaline",
                          "GWI" = "Polyhaline",
                          "SWH" = "Oligohaline"
  ),
  site_id = factor(site_id, levels = c("Oligohaline", "Mesohaline", "Polyhaline")))

ggplot(data_exploration_for_steph, aes(x = elevation_navd88_geoid12b,
                                       y = elevation_DEM)) +
  geom_point(aes(shape = position, text = study_id)) +
  facet_wrap(.~site_id, scale = "free") +
  geom_abline(slope = 1, intercept = 0) +
  theme_minimal() +
  xlab("Measured Elevation (m; NAVD88)") +
  ylab("Elevation from DEM (m; NAVD88)")

ggsave("RTK versus Measured Elevation.jpg", 
       width = 8, 
       height=2.75)

ggplotly(data_exploration)

# ggplot(all_points_together %>% filter(position == "wetland"), aes(x = elevation_DEM,
#                                 y = elevation_error)) +
#   geom_point(aes(shape = position, color = naip_z)) +
#   # facet_wrap(position~site_id) +
#   geom_hline(yintercept = 0)  +
#   scale_color_continuous(type = "viridis") +
#   geom_abline() +
#   geom_smooth(method = "lm", 
#               formula = "y ~ poly(x, 2)")


pal <- colorNumeric(
  palette = "viridis",
  domain = c(-0.1, 0.5)
  # domain = range(all_points_together$elevation_error, na.rm=T)
)


all_points_together_sf <- all_points_together %>% 
  st_as_sf(crs = "WGS84", coords = c("longitude", "latitude"))

leaflet(all_points_together_sf) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addCircleMarkers(color = ~pal(elevation_error),
                   radius = 5, label = ~paste0(study_id, ": ", elevation_error)) %>% 
  addLegend(pal = pal, values = ~elevation_error)

# ggplot(all_points_together %>% filter(position == "wetland"), aes(x = naip_ndvi,
#                                                                   y = elevation_error)) +
#   geom_point(aes(shape = position, color = elevation_DEM)) +
#   # facet_wrap(position~site_id) +
#   geom_hline(yintercept = 0) +
#   scale_color_continuous(type = "viridis") +
#   geom_smooth(method = "lm", 
#               formula = "y ~ poly(x, 2)")

# ggplot(all_points_together %>% filter(position == "wetland"), aes(x = zstar_DEM,
#                                                                   y = naip_z)) +
#   geom_point(aes(shape = position, color = elevation_error)) +
#   # facet_wrap(position~site_id) +
#   scale_color_continuous(type = "viridis") +
#   geom_smooth(method = "lm", formula = "y ~ poly(x, 2)")

ggplot(all_points_together, aes(x = position,
                                y = elevation_error)) +
  geom_jitter(aes(color = position)) +
  geom_boxplot(aes(color = position),  fill = NA, outliers = F) +
  facet_wrap(.~site_id, scale = "free") +
  geom_hline(yintercept = 0)




ggplot(all_points_together, aes(x = position,
                                y = elevation_error)) +
  geom_jitter(aes(color = position), alpha = 0.3) +
  geom_boxplot(aes(color = position), fill = NA,  outliers = F) +
  # facet_wrap(.~site_id, scale = "free") +
  geom_hline(yintercept = 0)

all_points_together_wetlands <- all_points_together %>% 
  filter(position == "wetland") %>% 
  filter(complete.cases(cover_class))

ggplot(all_points_together_wetlands, aes(x = cover_class,
                                y = elevation_error)) +
  geom_jitter(aes(color = cover_class)) +
  geom_boxplot(fill = NA, outliers = F) +
  facet_wrap(.~site_id, scale = "free") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none")

ggplot(all_points_together_wetlands, aes(x = cover_class,
                                         y = elevation_error)) +
  geom_jitter(aes(color = cover_class)) +
  # geom_boxplot(aes(color = cover_class),  fill = NA, outliers = F) +
  geom_boxplot(fill = NA, outliers = F) +
  facet_wrap(.~site_id, scale = "free") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "none") +
  xlab(NULL)
  
ggsave("All site errors w veg classes.jpg", width = 8, height = 5)

all_points_together_wetlands_more_than_1 <- all_points_together_wetlands %>% 
  group_by(cover_class) %>% 
  mutate(n_sites = n_distinct(site_id)) %>% 
  filter(n_sites>1) %>% 
  group_by(site_id) %>% 
  mutate(mean_site_error = mean(elevation_error),
         sd_site_error = sd(elevation_error)
         ) %>% 
  ungroup() %>% 
  mutate(normalized_site_error = (elevation_error-mean_site_error) / sd_site_error)

ggplot(all_points_together_wetlands_more_than_1, aes(x = site_id,
                                         y = elevation_error)) +
  geom_jitter(aes(color = cover_class)) +
  geom_boxplot(fill = NA, outliers = F) +
  facet_wrap(.~cover_class, scale = "free") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "none") 

