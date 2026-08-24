# Table 

library(raster)

GCREW_rast <- raster("data/GIS/GCREW_flume_watershed.tif")
GCREW_pixels <- cellStats(!is.na(GCREW_rast), sum)

Goodwin_rast <- raster("data/GIS/GoodwinIsland/Goodwin_flume_watershed.tif")
Goodwin_pixels <- cellStats(!is.na(Goodwin_rast), sum)

Sweethall_rast <- raster("data/GIS/SweetHall/SweetHall_flume_watershed.tif")
Sweethall_pixels <- cellStats(!is.na(Sweethall_rast), sum)

output_table <- data.frame(site = c("GCREW", "Goodwin", "Sweethall"),
                           drained_area_m2 = c(GCREW_pixels, Goodwin_pixels, Sweethall_pixels) * 3)

View(output_table)
write_csv(output_table, "Drained_areas_for_latflux.csv")
