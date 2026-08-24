# Query LiDAR dates
library(tidyverse)
library(xml2)

DEM_outputs <- "/Volumes/LaCie/COMPASS_hydrology/dem"

xml_files <- list.files(DEM_outputs)

sites <- read_csv("LateralFluxStations.csv")

unique_DEMs <- unique(sites$DEM)

unique_start_years <- c()
unique_end_years <- c()

for (i in 1:length(unique_DEMs)) {
  
  xml_data_front <- str_remove_all(unique_DEMs[i], "_GCS_3m_NAVDm.tif")

  
  metadata_file <- xml_files[grepl(xml_data_front, xml_files) & grepl("_metadata.xml", xml_files)]
  
  metadata <- read_xml(paste0(DEM_outputs, "/", metadata_file))
  
  begdate_raw <- xml_text(xml_find_all(metadata, "//rngdates/begdate"))
  
  unique_start_years <- c(unique_start_years, begdate_raw)
  
  enddate_raw <- xml_text(xml_find_all(metadata, "//rngdates/enddate"))
  
  unique_end_years <- c(unique_end_years, enddate_raw)
}

DEM_years <- data.frame(DEM = unique_DEMs, 
                        DEM_start_year=as.numeric(unique_start_years), 
                        DEM_end_year=as.numeric(unique_end_years)
                        )

sites_with_DEM_years <- sites %>% 
  left_join(DEM_years) %>% 
  mutate(DEM_mean_year = (DEM_start_year+DEM_end_year)/2)

write_csv(sites_with_DEM_years, "sites_with_DEM_years")
