# Housekeeping script to fuse all GEOID8 bin files
library(terra)
all_geoid_8_tiles <- list.files("data/GIS/GEOIDS/", 
                                pattern = "g2009",
                                full.names = T)

geoid_list <- list()
for (i in 1:length(all_geoid_8_tiles)) {
  
  geoid9 <- rast(all_geoid_8_tiles[i])
  e <- ext(geoid9)
  ext(geoid9) <- c(e$xmin - 360, e$xmax - 360, e$ymin, e$ymax)
  
  geoid_list[[i]] <- geoid9
}
geoid_sprc <- sprc(geoid_list)
merged_geoid <- merge(geoid_sprc)
writeRaster(merged_geoid, "data/GIS/GEOIDS/g2009u0.tif")
