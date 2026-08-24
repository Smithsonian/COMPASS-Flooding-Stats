# Iterate through stations 

library(tidyverse)
source("scripts/3_Water_Levels/getTidalDatums.R")

lateral_flux_stations <- read_csv("LateralFluxStations.csv") %>% 
  filter(subsite_id != "SageLotPond_alt")

datum_list <- list()
flood_prob_list <- list()

probs <- seq(0,1, by = 0.05)

for (i in 1:nrow(lateral_flux_stations)) {
  
  temp_site_id <- lateral_flux_stations$site_id[i]
  
  print(paste0(temp_site_id, " analysing ... "))
  
  bufferStartDate <- lubridate::ymd_hm(paste0(lateral_flux_stations$StartYear[i],
                                       "-01-01 00:00")
                                       ) - lubridate::days(15)

  bufferEndDate <- lubridate::ymd_hm(paste0(lateral_flux_stations$EndYear[i],
                                     "-12-31 24:59")) + lubridate::days(15)
  
  if (lateral_flux_stations$GaugeSource[i] == "NOAA") {
    
    bufferStartDate <- toString(format(bufferStartDate, "%Y-%m-%d %H:%M"))
    bufferEndDate <- toString(format(bufferEndDate, "%Y-%m-%d %H:%M"))
    
    wlTable <- download6minWlData(station_id=lateral_flux_stations$GaugeID[i], 
                                  startDate = bufferStartDate,
                                  endDate = bufferEndDate, 
                                  datum="NAVD")
    
  
  } else {
    
    if (lateral_flux_stations$GaugeSource[i] == "NERR") {
      
      wlTable <- read_csv(paste0("data/water_level/",
                                 lateral_flux_stations$GaugeSource[i], "/",
                                 lateral_flux_stations$WaterLevelFile[i]
      )) 
      
      wlTable <- wlTable %>% 
        rename(dateTime = SampleDateTime,
               waterLevel = NAVD88_elev) %>% 
        dplyr::select(dateTime, waterLevel) %>% 
        filter((dateTime >= bufferStartDate) & (dateTime <= bufferEndDate))
      
    } else if (lateral_flux_stations$GaugeSource[i] == "CRMS") {
      
      wlTable <- read_csv(paste0("data/water_level/",
                                 lateral_flux_stations$GaugeSource[i], "/",
                                 lateral_flux_stations$WaterLevelFile[i]
      ),
      locale = locale(encoding = "Latin1")) %>% 
        mutate(
          dateTime = mdy_hms(paste(`Date (mm/dd/yyyy)`, `Time (hh:mm:ss)`, `Time Zone`)),
          waterLevel = `Adjusted Water Elevation to Datum (ft)`*0.3048) %>% 
        dplyr::select(dateTime, waterLevel) %>% 
        filter((dateTime >= bufferStartDate) & (dateTime <= bufferEndDate))
                             
      
    } else {
      
      stop("No gauge data provided")
    }
    
      
    
    
  }

  
  wlTable <- wlTable %>% 
    group_by(dateTime) %>% 
    slice_sample(n = 1) %>% 
    ungroup()
  
  tidalDatums <- fitCustomTidalDatum(wlTable = wlTable, startDate = paste0(lateral_flux_stations$StartYear[i],
                                                                           "-01-01 00:00"), 
                                     endDate = paste0(lateral_flux_stations$EndYear[i],
                                                      "-12-31 24:59"),
                                     bufferStart = 15,
                                     bufferEnd = 15,
                                     graph=T,
                                     out_fig_name = lateral_flux_stations$GaugeID[i],
                                     gauge_data = "temp/"
                                     )
    
  sd_df <- data.frame(Datum = "sd", 
                      wl = sd(wlTable$waterLevel, na.rm = T),
                      n = length(wlTable$waterLevel[!is.na(wlTable$waterLevel)]))
  
  tidalDatums[[2]] <- tidalDatums[[2]] %>% 
    bind_rows(sd_df) %>% 
    mutate(station_id = lateral_flux_stations$GaugeID[i],
           startDate = lateral_flux_stations$StartYear[i],
           endDate = lateral_flux_stations$EndYear[i]) %>%
    rename(meters = wl) %>% 
    dplyr::select(station_id, startDate, endDate, Datum, meters, n)
  

  
  # Add to the master file
  # Rewrite master file
  
  return_datums <- tidalDatums[[2]] %>% 
    mutate(site_id = temp_site_id)
  
  classified_tides <-  tidalDatums[[1]] %>% 
    filter(HL == "H")
  
  datum_list[[i]] <- return_datums
  
  
  
  flood_prob_list[[i]] <-  tibble(
    flood_probability = probs,
    flood_elevation = quantile(classified_tides$observed, probs = probs, na.rm = TRUE)
  ) %>% 
    mutate(site_id = temp_site_id)
  
  
  print(paste0(temp_site_id, " done. "))
  
}

datum_save <- bind_rows(datum_list)
write_csv(datum_save, "datums_output.csv")
datum_save <- read_csv("datums_output.csv")

datum_vis <- datum_save %>% 
  filter(Datum %in% c("HOT", "HAT", "MHHWS", "MHHW", "MLHW", "MSL"))

flood_prob <- bind_rows(flood_prob_list)
write_csv(flood_prob, "flood_probailities.csv")

ggplot(flood_prob, aes(x = flood_elevation, y = flood_probability)) +
  geom_point() +
  geom_line() +
  geom_vline(data = datum_vis, aes(xintercept=meters, lty = Datum)) +
  facet_wrap(.~site_id)

