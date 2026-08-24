# 
library(whitebox)
library(sf)
library(terra)
library(sp)
library(raster)

# Load up master file
lateral_flux_stations <- read_csv("LateralFluxStations.csv")

large_file_path <- "/Volumes/LaCie/COMPASS_hydrology/"


# pour_points <- read_sf("/Volumes/LaCie/COMPASS_hydrology/TidalCreeks/SiteLevelPourPoints.shp")

# Create clips
lateral_flux_stations_w_buffers <- lateral_flux_stations %>% 
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

lateral_flux_stations_w_buffers_sf <- lateral_flux_stations_w_buffers %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  st_buffer(lateral_flux_stations_w_buffers$buffer_distance)  %>% 
  st_bbox_by_feature() %>% 
  st_as_sf(crs = "WGS84") %>% 
  bind_cols(lateral_flux_stations)

whitebox::wbt_init()

for (i in 1:nrow(lateral_flux_stations_w_buffers_sf)) {

  dem_file_path <- paste0(large_file_path, "HydroPreppedDems/", lateral_flux_stations_w_buffers_sf$site_id[i], "_veg_corrected.tif")
  
  dem <- terra::rast(dem_file_path)
  
  dem <- project(dem, CRS("WGS84"))
  
  threshold_value <- -1
  
  # 3. Mask values below the threshold
  dem2 <- clamp(dem, threshold_value)
  
  temp_site <- lateral_flux_stations_w_buffers_sf$subsite_id[i]
  
  # If 
  temp_out_path <- paste0("data/GIS/DEM_analysis/", temp_site)
  
  if (! file.exists(temp_out_path)) {
    dir.create(temp_out_path, recursive = T)
  }

  
  
  raster::writeRaster(dem2, paste0(temp_out_path, "/", temp_site, "_elevation_3m_masked.tif"), 
                      overwrite = TRUE)
  
  
  wbt_breach_depressions_least_cost(
    dem = paste0(temp_out_path, "/", temp_site, "_elevation_3m_masked.tif"),
    output = paste0(temp_out_path, "/", temp_site, "_breached.tif"),
    dist = 3,
    fill = TRUE)
  
  wbt_fill_depressions_wang_and_liu(
    dem = paste0(temp_out_path, "/", temp_site, "_breached.tif"),
    output = paste0(temp_out_path, "/", temp_site, "_breached_filled.tif"),
  )
  
  wbt_d8_flow_accumulation(input = paste0(temp_out_path, "/", temp_site, "_breached_filled.tif"),
                           output =  paste0(temp_out_path, "/", temp_site, "_D8FA.tif"))
                           
  wbt_d8_pointer(dem =  paste0(temp_out_path, "/", temp_site, "_breached_filled.tif"),
                 output = paste0(temp_out_path, "/", temp_site, "_pointer.tif"))
  
  ppoints <- tribble(
    ~Lon, ~Lat,
    lateral_flux_stations_w_buffers_sf$longitude[i], lateral_flux_stations_w_buffers_sf$latitude[i]
  )
  
  ppointsSP <- SpatialPoints(ppoints, proj4string = CRS("WGS84"))
  
  shapefile(ppointsSP, filename =  paste0(temp_out_path, "/", temp_site, "_flume.shp"), overwrite = TRUE)
  
  wbt_extract_streams(flow_accum =  paste0(temp_out_path, "/", temp_site, "_D8FA.tif"),
                      output =  paste0(temp_out_path, "/", temp_site, "_streams.tif"),
                      threshold = 500)
  
  wbt_jenson_snap_pour_points(pour_pts = paste0(temp_out_path, "/", temp_site, "_flume.shp"),
                              streams = paste0(temp_out_path, "/", temp_site, "_streams.tif"),
                              output = paste0(temp_out_path, "/", temp_site, "_flume_snapped.shp"),
                              snap_dist = 0.001) #careful with this! Know the units of your data
  
  pp <- shapefile(paste0(temp_out_path, "/", temp_site, "_flume_snapped.shp"))
  streams <- raster(paste0(temp_out_path, "/", temp_site, "_streams.tif"),)
  
  # leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
  # 
  #   addRasterImage(dem, colors = pal, opacity = 0.8) %>%
  #   addRasterImage(streams, colors = "blue", opacity = 0.8) %>%
  #   addCircleMarkers(color = "black", lat = pp@coords[,"y"], lng = pp@coords[,"x"])
  
  wbt_watershed(d8_pntr = paste0(temp_out_path, "/", temp_site, "_pointer.tif"),
                pour_pts =  paste0(temp_out_path, "/", temp_site, "_flume_snapped.shp"),
                output = paste0(temp_out_path, "/", temp_site, "_flume_watershed.tif"))
  
  lil_shed <- raster(paste0(temp_out_path, "/", temp_site, "_flume_watershed.tif"))
  
  library(leaflet)
  
  leaflet() %>% addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
    addRasterImage(lil_shed, colors = "white", opacity = 0.8) %>% 
    addRasterImage(streams, colors = "blue", opacity = 0.8) %>% 
    addCircleMarkers(color = "black", lat = pp@coords[,"y"], lng = pp@coords[,"x"])
  # addRasterImage(streams, colors = "blue", opacity = 0.8)
  
}
