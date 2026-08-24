# Iterate through sites
library(tidyverse)
library(sf)
library(maptiles)
library(terra)

lateral_flux_stations <- read_csv("LateralFluxStations.csv")  %>% 
  filter(subsite_id != "SageLotPond_alt")

emp_distributions <- read_csv("flood_probailities.csv")

output_table_list <- list()

for (i in 1:nrow(lateral_flux_stations)) {
  
  # Load up DEM
  temp_site_id <- lateral_flux_stations$subsite_id[i]
  
  print(paste0(temp_site_id, " analysing ... "))
  
  # Load up watershed
  dem <- terra::rast(paste0("data/GIS/DEM_analysis/",
                                  temp_site_id, "/", temp_site_id,
                                  "_elevation_3m_masked.tif"))
  
  # Load up watershed
  watershed <- terra::rast(paste0("data/GIS/DEM_analysis/",
                                  temp_site_id, "/", temp_site_id,
                                  "_flume_watershed.tif"))
  
  # Load up snapped pour point
  pour_point <- read_sf(paste0("data/GIS/DEM_analysis/",
                                temp_site_id, "/", temp_site_id,
                                "_flume_snapped.shp"))
  
  # Clip raster by watershed
  flume_dem <- terra::mask(dem, watershed)
  
  probability_table <- emp_distributions %>% 
    filter(site_id == lateral_flux_stations$site_id[i])
  
  
  temp_rast_list <- list()
  
  pixel_vect <- c()
  
  
  pb <- txtProgressBar(min = 1, max = nrow(probability_table), style = 3)

  
  # Iterate through probabilities
  for (j in 1:nrow(probability_table)) {
    
    # Classify everthing as below the flood line as 1 all else as zero.
    below_flood_line <- ifel(flume_dem<probability_table$flood_elevation[j],
                             1,
                             NA)
    
    # Segmentize
    segmented_floods <- terra::patches(below_flood_line)
    
    # Query which segment is at the pour point
    which_patch <- terra::extract(segmented_floods,
                           pour_point
                           )
    
    # Subset raster by what is at the point
    connected_floods <- below_flood_line
    connected_floods[segmented_floods != which_patch$patches[1]] <- NA
    
    # Add to list
    temp_rast_list[[j]] <- connected_floods
    
    areas <- cellSize(connected_floods)
    floodAreas <- mask(areas, connected_floods)
    
    number_of_pixels_m2 <- sum(as.vector(floodAreas$area), na.rm=T)
    
    pixel_vect <- c(pixel_vect, number_of_pixels_m2)
    
    # Print progress
    setTxtProgressBar(pb, j)
    
  } # End of loop
  close(pb)
  
  # Save_output 
  probability_table_out <- probability_table %>% 
    mutate(area_m2 = pixel_vect)
  
  output_table_list[[i]] <- probability_table_out
  
  # Define list as raster stack 
  temp_rast_stack <- terra::rast(temp_rast_list)
  temp_rast_stack[is.na(temp_rast_stack)] <- 0
  
  # Take pixel wise mean
  # probability surface = mean of binary layers = proportion of timesteps included
  prob_surface <- app(temp_rast_stack, fun = mean, na.rm = TRUE)
  prob_surface[prob_surface == 0] <- NA
  
  raster::writeRaster(prob_surface, paste0("data/GIS/DEM_analysis/",
                                           temp_site_id, "/", temp_site_id,
                                           "_footprint.tif"), 
                      overwrite = TRUE)
  
  # Write to file
  
  buffer_frac <- 0.25  # 15% padding around the combined extent
  
  e <- ext(c(terra::trim(prob_surface)))
  dx <- (e[2] - e[1]) * buffer_frac
  dy <- (e[4] - e[3]) * buffer_frac
  map_extent <- ext(e[1] - dx, e[2] + dx, e[3] - dy, e[4] + dy)
  
  basemap <- get_tiles(
    x        = map_extent,
    provider = "Esri.WorldImagery",
    crop     = TRUE,
    zoom     = 17  # increase for more detail (slower/larger download), decrease for speed
  )
  

  basemap_proj <- project(basemap, "EPSG:3857")
  prob_proj <- project(prob_surface, "EPSG:3857")
  pour_point_proj <- pour_point %>% st_transform("EPSG:3857")
  
  png(paste0("footprint_maps/", temp_site_id, "_footprint_map.png"), width = 2000, height = 2000, res = 300)
  plotRGB(basemap_proj, mar = c(2, 2, 2, 4))
  
  plot(
    prob_proj,
    add     = TRUE,
    col     = hcl.colors(100, "Inferno", rev = F),
    alpha   = 0.7,
    legend  = TRUE,
    plg     = list(title = "Inclusion\nprobability", cex = 1)
  )
  
  # sensor location
  points(
    pour_point_proj,
    pch = 24,        # triangle, good visibility against both imagery and heatmap
    cex = 1.6,
    bg  = "white",
    col = "black",
    lwd = 1.5
  )
  
  # scale bar (terra native, no extra package needed)
  sbar(
    d       = NULL,        # auto-scaled to plot extent
    type    = "bar",
    below   = "meters",
    cex     = 0.7
  )

  
  # title(main = "Sensor Footprint Probability", cex.main = 1.1)
  
  # legend(
  #   "topright",
  #   legend = c("Sensor location", "Max potential extent"),
  #   pch    = c(24, NA),
  #   lty    = c(NA, 1),
  #   lwd    = c(1.5, 2),
  #   col    = c("black", "cyan"),
  #   pt.bg  = c("white", NA),
  #   bty    = "n",
  #   cex    = 0.8,
  #   text.col = "white"
  # )
  
  dev.off()
  
  # ------------------------------------------------------------
  # Optional: interactive on-screen version (skip the png() wrapper)
  # ------------------------------------------------------------
  # plotRGB(basemap, mar = c(2, 2, 2, 4))
  # plot(prob_masked, add = TRUE, col = hcl.colors(100, "Inferno", rev = TRUE), alpha = 0.7)
  # sbar(type = "bar", below = "meters")
  
  
  
} # End of site iteration loop 



output_table <- bind_rows(output_table_list)
write_csv(output_table, "empirical_flood_heights_vs_areas.csv")

datum_vis <- read_csv("datums_output.csv") %>% 
  filter(Datum %in% c("HOT", "HAT", "MHHWS", "MHHW", "MLHW"))

ggplot(output_table, aes(x = flood_elevation, y = area_m2)) +
  geom_point() +
  geom_line() +
  geom_vline(data = datum_vis, aes(xintercept=meters, lty = Datum, color = Datum)) +
  facet_wrap(.~site_id, scale = "free") +
  ylab(expression("Area (m"^2*")"))  +
  xlab("Flood Elevation (m NAVD88)") +
  scale_y_continuous(labels = scales::comma)

ggsave("Flood Elevation Versus Bathtub Area.jpg")

# Iterate through sites

# Filter by site id

# Approx areas at tidal datum
output_list <- list()
for (i in 1:nrow(lateral_flux_stations)) {
  
  # Load up DEM
  temp_site_id <- lateral_flux_stations$site_id[i]
  
  hydrograph <- output_table %>% 
    filter(site_id == temp_site_id)
  
  site_datums <- datum_vis %>% 
    filter(site_id == temp_site_id)
  
  area_per_datum <- approx(x = hydrograph$flood_elevation,
                           y = hydrograph$area_m2,
                           xout = site_datums$meters,
                           rule = 2)
  
  site_datums$area_m2 <- area_per_datum$y
  output_list[[i]] <- site_datums
  
}

datum_areas <- bind_rows(output_list)
write_csv(datum_areas, "datum_areas_info_output.csv")
