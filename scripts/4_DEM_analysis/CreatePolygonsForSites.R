# Create site bounds

library(tidyverse)
library(sf)

sites_with_DEM_years <- read_csv("sites_with_DEM_years.csv")

sites_with_DEM_years_w_buffers <- sites_with_DEM_years %>% 
  mutate(buffer_distance = case_when(
    site_id =="GCREW" ~ 750, 
    site_id =="GoodwinIsland" ~ 1750,
    site_id =="SweetHall" ~ 1750,
    site_id =="EdenLanding" ~ 3000,
    site_id =="Barataria"  ~ 1000,
    site_id =="SageLotPond" ~ 500,
    site_id =="RushRanch" ~ 2500
  ))

# https://github.com/r-spatial/sf/issues/1179
st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}


sites_with_DEM_years_tab <- sites_with_DEM_years_w_buffers %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  st_buffer(sites_with_DEM_years_w_buffers$buffer_distance) %>%
  mutate(
    bbox = st_geometry(.) %>% map(st_bbox),
    xmin = map_dbl(bbox, "xmin"),
    ymin = map_dbl(bbox, "ymin"),
    xmax = map_dbl(bbox, "xmax"),
    ymax = map_dbl(bbox, "ymax")
  ) %>%
  dplyr::select(-bbox) %>% 
  st_drop_geometry()

sites_with_DEM_years_sf <- sites_with_DEM_years_w_buffers %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  st_buffer(sites_with_DEM_years_w_buffers$buffer_distance)  %>% 
  st_bbox_by_feature() %>% 
  st_as_sf(crs = "WGS84")


sites_with_DEM_years_output <- sites_with_DEM_years_sf %>%  
  bind_cols(sites_with_DEM_years_tab)

names(sites_with_DEM_years_output) <- str_remove_all(names(sites_with_DEM_years_output), "_year")
  
st_write(sites_with_DEM_years_output, 
         "Site_Extents.shp",
         append=FALSE)



library(leaflet)

leaflet(sites_with_DEM_years_output) %>%  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(opacity = 0.4)

