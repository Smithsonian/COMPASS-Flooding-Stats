library(tidyverse)
library(sf)
library(stringr)
library(sfnetworks)
library(terra)

large_file_path <- "/Volumes/LaCie/COMPASS_hydrology/"

# Load up master file
lateral_flux_stations <- read_csv("LateralFluxStations.csv") %>% 
  group_by( group_by(across(c(-subsite_id, latitude, longitude)))) %>% 
  summarise(longitude=mean(longitude), latitude=mean(latitude))
  

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

for (i in 1:nrow(lateral_flux_stations_w_buffers_sf)) {
  
  out_file_path <- paste0(large_file_path, "HydroPreppedDems/", lateral_flux_stations_w_buffers_sf$site_id[i], "_veg_corrected.tif")
  
  if (file.exists(out_file_path)) {
    print(paste0(lateral_flux_stations_w_buffers_sf$site_id[i], " done."))
    next
  } else {
    
    print(paste0("analysing ", lateral_flux_stations_w_buffers_sf$site_id[i], " ..."))
    
  }
  
  # Load up raster
  print(" ... loading raster")
  raster_path <- paste0(large_file_path, "dem/", lateral_flux_stations_w_buffers_sf$DEM[i])
  
  temp_rast <- terra::rast(raster_path)
  
  # temp_crs <- lateral_flux_stations_w_buffers_sf$EPSG[i]
  temp_crs <- crs(temp_rast)
    
  print(" ... projecting and cropping dem")

  # Project bounding box
  temp_bbox <- lateral_flux_stations_w_buffers_sf[i,] %>% 
    st_transform(crs = temp_crs)
  
  # proj_crs <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs"
  
  # Clip
  temp_rast_clip <- terra::crop(temp_rast, temp_bbox)
  
  # proj_bbox <- temp_bbox %>%
  #   st_transform(crs = proj_crs)
  
  # Create networks from files
  
  # Get shapefiles
  # stream_path <- paste0(large_file_path, "TidalCreeks/", lateral_flux_stations_w_buffers_sf$TidalCreeks[i])
  # temp_streams <- read_sf(stream_path) %>% 
  #   st_transform(temp_crs) # Project
  # 
  # # Get points
  # temp_points <-  pour_points %>% 
  #   st_transform(temp_crs) %>% # Project
  #   st_filter(temp_bbox) # Filter
  
  # Snap stream lines to local minimum elevation
  # Step 1: build network on ORIGINAL sloppy lines just to identify shared nodes
  # net <- as_sfnetwork(temp_streams, directed = FALSE)
  # nodes <- net |> activate("nodes") |> st_as_sf()
  # nodes$degree <- net |> activate("nodes") |> pull(centrality_degree())
  # 
  # # Step 2: snap each shared node ONCE, using a local search window (not a transect)
  # snap_node <- function(pt, dem, radius = 5) {
  #   buf <- st_buffer(pt, radius)
  #   cropped <- crop(dem, vect(buf), mask = TRUE)
  #   min_cell <- which.min(values(cropped))
  #   if (length(min_cell) == 0 || is.na(min_cell)) return(st_coordinates(pt))
  #   xyFromCell(cropped, min_cell)
  # }
  # 
  # node_coords <- st_coordinates(nodes)
  # snapped_coords <- t(sapply(seq_len(nrow(nodes)), function(i) {
  #   snap_node(st_sfc(st_point(node_coords[i,]), crs = st_crs(temp_streams)), temp_rast)
  # }))
  # 
  # nodes$x_snap <- snapped_coords[,1]
  # nodes$y_snap <- snapped_coords[,2]
  # 
  # edges <- net |> activate("edges") |> st_as_sf()
  # 
  # resnap_edge <- function(i) {
  #   geom <- st_geometry(edges)[[i]]
  #   coords <- st_coordinates(geom)[, 1:2]
  #   
  #   from_id <- edges$from[i]
  #   to_id   <- edges$to[i]
  #   
  #   # force endpoints to the pre-snapped shared node positions
  #   coords[1, ]   <- c(nodes$x_snap[from_id], nodes$y_snap[from_id])
  #   coords[nrow(coords), ] <- c(nodes$x_snap[to_id], nodes$y_snap[to_id])
  #   
  #   # snap interior vertices independently (your original transect approach is fine here,
  #   # since interior points by definition aren't shared with another edge)
  #   if (nrow(coords) > 2) {
  #     for (j in 2:(nrow(coords) - 1)) {
  #       i0 <- j - 1; i1 <- j + 1
  #       dx <- coords[i1,1] - coords[i0,1]
  #       dy <- coords[i1,2] - coords[i0,2]
  #       len <- sqrt(dx^2 + dy^2)
  #       if (len == 0) next
  #       perp <- c(-dy, dx) / len
  #       p0 <- coords[j,] - perp * 7.5
  #       p1 <- coords[j,] + perp * 7.5
  #       transect <- st_linestring(rbind(p0, p1)) |> st_sfc(crs = st_crs(lines)) |>
  #         st_segmentize(dfMaxLength = 0.5) |> st_cast("POINT") |> st_coordinates()
  #       elevs <- extract(dem, transect[,1:2])[,1]
  #       if (!all(is.na(elevs))) coords[j,] <- transect[which.min(elevs), 1:2]
  #     }
  #   }
  #   
  #   st_linestring(coords)
  # }
  # 
  # snapped_geoms <- lapply(seq_len(nrow(edges)), resnap_edge)
  # edges_snapped <- st_sf(st_drop_geometry(edges), geometry = st_sfc(snapped_geoms, crs = temp_crs))
  # 
  # # Calculate distances from pour points
  # net <- net |> activate("edges") |> mutate(length = st_length(geometry))
  # 
  # net_blended <- st_network_blend(net, temp_points, tolerance = 10)  # tune to how far 
  # 
  
  # Densify distances
  
  # Rasterize distances

  # Create burn depths

  # Load up nwi
  print(" ... working with nwi")
  nwi_path <- paste0(large_file_path, "nwi/", lateral_flux_stations_w_buffers_sf$NWI[i])
  state_name <- substr(rev(str_split(nwi_path, "\\/")[[1]])[1], start = 1, stop = 2)
  layer_name <- paste0(state_name, "_Wetlands")
  temp_nwi <- read_sf(nwi_path, layer = layer_name) %>% 
    st_make_valid() %>% 
    st_transform(crs = temp_crs)
  
  sf::sf_use_s2(F)
  
  # Project
  temp_nwi_clip <- temp_nwi %>% 
    st_crop(temp_bbox)
  
  sf::sf_use_s2(T)

  # Get OW from NWI
  ow_poly <- temp_nwi_clip %>% 
    filter(WETLAND_TYPE == "Estuarine and Marine Deepwater")
  
  est_poly <- temp_nwi_clip %>% 
    filter(WETLAND_TYPE == "Estuarine and Marine Wetland")
  
  # Get Wetland from NWI
  est_buffer <- est_poly %>% 
    st_buffer(30) %>% 
    st_intersection(ow_poly)
  
  est_and_ow <- temp_nwi_clip %>% 
    filter(WETLAND_TYPE %in% c("Estuarine and Marine Deepwater",
                               "Estuarine and Marine Wetland"))
  
  # If obstructions are present
  if (!is.na(lateral_flux_stations_w_buffers_sf$Obstructions[i])) {
    print(" ... removing obstructions")
    # load file, 
    obstructions_path <- paste0(large_file_path, "obstructions/", lateral_flux_stations_w_buffers_sf$Obstructions[i])
    obstructions_sf <- read_sf(obstructions_path) %>% 
      mutate(dummy_int = 1) %>% 
      st_transform(crs = temp_crs) # project
    
    # Raseterize
    obstructions_rast <- rasterize(x = obstructions_sf, y = temp_rast_clip,
             field = "dummy_int",
             background = 0,
             fun = "max")
    
    
    # Replace overlapping values with -99
    temp_rast_clip[obstructions_rast == 1] <- -99
    
  }
  
  print(" ... veg correcting dem")
  
  # Separate wetlands
  wetland_rast <- mask(temp_rast_clip, 
                       est_poly
                       )
  
  # Veg correct
  # If file provided
  if (!is.na(lateral_flux_stations_w_buffers_sf$VegCorrectRaster[i])) {
    # load file
    correction_path <- paste0(large_file_path, "VegCorrection/", lateral_flux_stations_w_buffers_sf$VegCorrectRaster[i])
    
    # project
    correction_rast <- terra::rast(correction_path) %>% 
      project(crs(temp_crs))
    
    correction_rast <- resample(correction_rast, wetland_rast)
    correction_rast[is.na(correction_rast)] <- 0
    # subtract correction from surface
    wetland_rast <- wetland_rast - correction_rast
    
  } else {
    
    haircut_value <- lateral_flux_stations_w_buffers_sf$VegCorrectNumber[i]
    wetland_rast <- wetland_rast - haircut_value
  }
  
  # Open water
  # mask by water buffer
  ow_rast <- mask(temp_rast_clip, 
                  est_buffer
  )
  
  # Upland do nothing
  upland_rast <- mask(temp_rast_clip,
                      est_and_ow,
                      inverse=T)
  
  mosaiced_output <- mosaic(wetland_rast, ow_rast, upland_rast)
  
  # mosaic
  # Replace -99 with arbitrarily low value
  # mosaiced_output <- clamp(mosaiced_output, lower=-1, upper=Inf)
  
  # Burn in streams
  print(" ... writing to file")
  
  writeRaster(mosaiced_output, 
              filename = out_file_path,
              overwrite = T)
  print(paste0(lateral_flux_stations_w_buffers_sf$site_id[i], " done."))
  
  
}




