# Try out different NDWI thresholds
library(leaflet)
library(terra)
library(whitebox)

thresholds <- c(-0.1, 0, 0.1, 0.2, 0.3, 0.33)

naip_list <- list.files("/Volumes/LaCie/COMPASS_hydrology/naip", 
                       full.names = T, 
                       pattern ="*.tif$") 

vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

for (i in 1:length(naip_list)) {
  
  naip <- terra::rast(naip_list[i])
  img_title <- 
    str_remove_all(
      rev(str_split(naip_list[i], "\\/")[[1]])[1],  
      ".tif")
  
  ndwi <- vi(naip, k = "G", i = "N")

  pdf(paste0("temp/", img_title, "_ndwi_thresholds.pdf"), 
      width = 8.5, 
      height = 11,
      title = img_title)
  
  par(mfrow=c(3,2))
  
  for (j in 1:length(thresholds)) {
    
    m <- c(-Inf, thresholds[j], 0,
           thresholds[j], Inf, 1)
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    
    # water <- terra::clamp(ndwi, lower=thresholds[j])
    water <- classify(ndwi, rclmat)
    
    # # identify connected components (8-directional connectivity is usually right
    # # for channel networks, since diagonal connections matter at confluences)
    # water_patches <- patches(water, directions = 8, zeroAsNA = TRUE)
    # 
    # # count cells per patch ID
    # patch_freq <- freq(water_patches)
    # 
    # # find the patch ID with the maximum cell count
    # largest_patch_id <- patch_freq$value[which.max(patch_freq$count)]
    # 
    # # keep only that patch, everything else becomes NA
    # largest_water <- water_patches == largest_patch_id
    # largest_water <- mask(largest_water, largest_water, maskvalue = FALSE)
    # 
    plot(water, main=paste0("ndwi=", thresholds[j]),
         col = c("yellow", "darkblue"))
    
    # plot(largest_water, col = "darkred")
    
  }
  dev.off()
    
}
