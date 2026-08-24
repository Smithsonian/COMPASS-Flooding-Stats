# Download 6 minute tide gauge data
# Crunch and return new datums

require(lubridate)
require(tidyverse)
require(httr)
require(XML)
require(TideHarmonics)
options(tidyverse.quiet = TRUE)
require(zoo)

#Our transformation function
scaleFUN <- function(x) sprintf("%.1f", x)

download6minWlData <- function(station_id=8575512, startDate = '2016-01-01 00:00',
                               endDate = '2016-12-31 23:59', datum="NAVD") {
  
  startDate <- ymd_hm(startDate)
  endDate <- ymd_hm(endDate)
  
  queryStart <- startDate
  queryEnd <- as_datetime(ifelse(endDate <= startDate+days(31), 
                                 endDate, startDate+days(31)))
  
  n_iterations <- round(as.numeric(difftime(endDate, startDate, units = "days"))/31+0.5)
  
  for (i in 1:n_iterations) {
    
    queryStartString <- toString(format(queryStart, "%Y%m%d %H:%M"))
    queryEndString <- toString(format(queryEnd, "%Y%m%d %H:%M"))
    
    xml_path <- paste("https://tidesandcurrents.noaa.gov/api/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=",
                      queryStartString, 
                      "&end_date=", queryEndString, 
                      "&datum=", datum, 
                      "&station=", toString(station_id), 
                      "&time_zone=GMT&units=metric&format=xml", sep="")
    
    xml_path <- gsub(" ", "%20", xml_path)
    tide_link <- httr::GET(xml_path)
    doc <- xmlParse(tide_link, useInternalNodes = TRUE) ### xmlParse()- is to parse the xml content, the parsed content is stored into doc
    xL <- xmlToList(doc) ###is to convert xml doc into List
    
    if (is.list(xL)) {
      if (exists("observations", where=xL) == T) {
        downloadData <- data.frame(matrix(unlist(xL$observations), ncol=5, byrow=T), 
                                   stringsAsFactors = F)
        names(downloadData) <- c("t", "v", "s", "f", "q")
        
        if (i == 1) {
          storeData <- downloadData
        } else {
          storeData <- storeData %>%
            bind_rows(downloadData)
        }
      } else {
        stop("Something went wrong with the data availability")
      }
    } else {
      stop("Something went wrong with the query")
    }
    
    queryStart <- queryEnd+minutes(6)
    
    if (i == n_iterations-1) {
      queryEnd <- endDate
    } else if (i < n_iterations-1) {
      queryEnd <- queryStart+days(31)
    }
  }
  
  outputData <- storeData %>%
    mutate(dateTime = ymd_hm(`t`),
           waterLevel = as.numeric(`v`)) %>%
    dplyr::select(dateTime, waterLevel)
  
  return(outputData)
}

fitCustomTidalDatum <- function(wlTable, startDate='2015-01-01 00:00',
                                bufferStart = 15,
                                bufferEnd = 15,
                                endDate='2015-12-31 24:59', graph=F, out_fig_name="temp",
                                gauge_data="biomass/agb_biomass_inundation/NOAA_water_levels") {
  
  
  
    # Fit harmonics
    fitHamonics <- TideHarmonics::ftide(x=wlTable$waterLevel,
                                        dto = wlTable$dateTime,
                                        nodal = F)
    
    # Amplitudes are usually in fit$Amp (named vector) or a coefficient data frame
    is_semidiurnal <- ifelse(fitHamonics$fval <= 3, 1, 0)
 
    bufferStartDate <- ymd_hm(startDate) - days(bufferStart)
    bufferStartDate <- toString(format(bufferStartDate, "%Y-%m-%d %H:%M"))

    bufferEndDate <- ymd_hm(endDate) + days(bufferEnd)
    bufferEndDate <- toString(format(bufferEndDate, "%Y-%m-%d %H:%M"))
    
    
    min_timestep <- min(
      diff(sort(wlTable$dateTime[!is.na(wlTable$dateTime)]))
    )
    
    predictFromHarmonics <- predict(fitHamonics, from = min(wlTable$dateTime), to = max(wlTable$dateTime),
                                    by = as.numeric(min_timestep, units = "mins")/60,
                                    msl = F) + mean(wlTable$waterLevel, na.rm=T)
    
    
    # detrend and add msl, IDK why but sometimes predict from harmonics is relative to 0, sometimes to MSL
    # predictFromHarmonics <- predictFromHarmonics - mean(predictFromHarmonics)
    # predictFromHarmonics <- predictFromHarmonics + fitHamonics$msl
    predicted_wls <- data.frame(dateTime = seq(min(wlTable$dateTime), max(wlTable$dateTime), by =  as.numeric(min_timestep, units = "mins")/60 * 3600),
                                predicted = predictFromHarmonics)
    
    allWaterLevels <- wlTable %>%
      full_join(predicted_wls, by="dateTime") %>% 
      rename(observed = waterLevel) %>% 
      arrange(dateTime)
    
    
    
    classifiedTides <- allWaterLevels %>%
      mutate(HL = ifelse(predicted > lag(predicted) & predicted > lead(predicted), "H",
                         ifelse(predicted < lag(predicted) & predicted < lead(predicted), "L", NA))) %>%
      filter(! is.na(HL))
    
    if (is_semidiurnal == 0) {
      
      classifiedTides <- classifiedTides %>% 
        mutate(
        
        classifiedTides = ifelse((HL == "H"), "HH",
                                 ifelse((HL == "L"), "LL", HL)),
        precedingTideRange = abs(predicted-lag(predicted)),
        year = year(dateTime),
        month = month(dateTime),
        day = day(dateTime)) %>%
        mutate(localMaxTR = suppressWarnings(rollmax(precedingTideRange, 28, "center", na.pad=T)),
               flagMax = ifelse(precedingTideRange == localMaxTR | localMaxTR == lead(precedingTideRange) | localMaxTR == lag(precedingTideRange), "monthlyMax", "not")) %>%
        mutate(classifiedTides = ifelse(flagMax=="monthlyMax" & classifiedTides == "HH", "HHS",
                                        ifelse(flagMax=="monthlyMax" & classifiedTides == "LL", "LLS", classifiedTides)),
               timeSpan = difftime(dateTime, lag(dateTime), units="hours")) %>%
        mutate(classifiedTides = recode(classifiedTides, "L"="HL", "H"="LH")) %>% 
        filter((dateTime >= ymd_hm(startDate, tz="UTC")) & (dateTime <= ymd_hm(endDate, tz="UTC")))
      
    } else {
      classifiedTides <- classifiedTides %>% mutate(
        
        classifiedTides = ifelse((HL == "H") & (predicted>lag(predicted,2)) & (predicted>lead(predicted, 2)), "HH",
                                 ifelse((HL == "L") & (predicted<lag(predicted,2)) & (predicted<lead(predicted, 2)), "LL", HL)),
        precedingTideRange = abs(predicted-lag(predicted)),
        year = year(dateTime),
        month = month(dateTime),
        day = day(dateTime)) %>%
        mutate(localMaxTR = suppressWarnings(rollmax(precedingTideRange, 57, "center", na.pad=T)),
               flagMax = ifelse(precedingTideRange == localMaxTR | localMaxTR == lead(precedingTideRange) | localMaxTR == lag(precedingTideRange), "monthlyMax", "not")) %>%
        mutate(classifiedTides = ifelse(flagMax=="monthlyMax" & classifiedTides == "HH", "HHS",
                                        ifelse(flagMax=="monthlyMax" & classifiedTides == "LL", "LLS", classifiedTides)),
               timeSpan = difftime(dateTime, lag(dateTime), units="hours")) %>%
        mutate(classifiedTides = recode(classifiedTides, "L"="HL", "H"="LH")) %>% 
        filter((dateTime >= ymd_hm(startDate, tz="UTC")) & (dateTime <= ymd_hm(endDate, tz="UTC")))
      
      
    }
    
    
    simpleHL <- classifiedTides %>%
      group_by(HL) %>%
      summarise(wl=mean(predicted),
                n=n()) %>%
      mutate(HL = recode(HL, "H"="MHW", "L"="MLW")) %>%
      rename(classifiedTides = HL)
    
    datum_summaries <- classifiedTides %>%
      group_by(classifiedTides) %>%
      summarise(wl=mean(predicted),
                n = n()) %>%
      bind_rows(simpleHL) %>% 
      filter(complete.cases(.)) %>%
      rename(Datum = classifiedTides) %>%
      mutate(Datum = recode(Datum, "LH"="MLHW", "HH"="MHHW", "HHS"="MHHWS", "HL"="MHLW", "LL"="MLLW", "LLS"="MLLWS")
             
             
             )
    
    wlTableSubset <- allWaterLevels %>% 
      filter((dateTime >= ymd_hm(startDate, tz="UTC")) & (dateTime <= ymd_hm(endDate, tz="UTC")))
    
    otherDatums1 <- as_tibble(data.frame(Datum = c("MSL", "HAT", "LAT"),
                              wl=c(mean(wlTableSubset$predicted,na.rm=T), max(wlTableSubset$predicted, na.rm=T), min(wlTableSubset$predicted, na.rm=T)),
                              n=c(nrow(wlTableSubset), 1, 1),
                              stringsAsFactors = F))
    
    datum_summaries <- datum_summaries %>% 
      bind_rows(otherDatums1) %>% 
      arrange(-wl)
    
    otherDatums2 <- as_tibble(data.frame(Datum = c("HOT", "LOT"), 
                                         wl=c(max(wlTableSubset$observed, na.rm=T), min(wlTableSubset$observed, na.rm=T)),
                                         n=c(1, 1),
                                         stringsAsFactors = F))
    
    datum_summaries <- datum_summaries %>% 
      bind_rows(otherDatums2) %>% 
      arrange(-wl)
    
  if (graph == T) {
    print("  ... graphing")
    # Coded datums, observed and predicted WL by fractional month
    tidesPlot <- classifiedTides %>%
      mutate(day_in_month = days_in_month(dateTime),
        f_day_of_month = ((day + (hour(dateTime)/24 + (minute(dateTime)/(60*24))))) /
               days_in_month(month)
             ) %>% 
      rename(wl = predicted)
    
    # Add predicted to wl table
    # Change waterLevel name to measured
    # Gather excluding datetime

    wlPlotting <- allWaterLevels %>% 
      dplyr::select(dateTime, observed, predicted) %>% 
      filter((dateTime >= ymd_hm(startDate, tz="UTC")) & (dateTime <= ymd_hm(endDate, tz="UTC"))) %>%
      # left_join(wlTable, by = "dateTime") %>%
      # mutate(anomaly = predicted - waterLevel) %>% 
      #select(-waterLevel) %>% 
      tidyr::gather(key = measuredOrModeled, value = wl, -dateTime) %>%
      arrange(dateTime, measuredOrModeled) %>%
      mutate(year = year(dateTime), 
        month = month(dateTime), 
             day_in_month = days_in_month(dateTime),
             f_day_of_month = ((day(dateTime) + (hour(dateTime)/24 + (minute(dateTime)/(60*24))))) /
               days_in_month(month)
             )
    
    wl_plot <- ggplot(data = wlPlotting, aes(x=f_day_of_month, y=wl)) +
      facet_wrap(year~month) +
      geom_line(aes(col = measuredOrModeled), alpha=0.66) +
      geom_point(data = tidesPlot, aes(shape = classifiedTides)) +
      ylab("Water Level (m)") +
      xlab("Month (fraction)") +
      theme_minimal() +
      scale_x_continuous(labels=scaleFUN) +
      theme(legend.title = element_blank())
    out_fig <- paste(gauge_data, "/", out_fig_name, ".pdf", sep = "")
    ggsave(out_fig, wl_plot, height = 8.5, width = 11)
  }
  
  return(list(classifiedTides, datum_summaries))
  
}

getPctInundationProfile <- function(wlTable) {
  targetElevations <- seq(round(min(wlTable$waterLevel, na.rm=T),2),
                          round(max(wlTable$waterLevel, na.rm=T),2),
                          0.01)
  
  fInundation <- c()
  for (i in 1:length(targetElevations)) {
    
    inundationEvents <- filter(wlTable, waterLevel >= targetElevations[i])
    fInundation <- c(fInundation, 
                     nrow(inundationEvents)/nrow(wlTable))
  }
  
  middle_df <- data.frame(elevation = targetElevations,
                          fractionInundation = fInundation)
  
  # Lowest inundation step should be 1
  lowest_df <- data.frame(elevation =  min(targetElevations) - 0.01,
                          fractionInundation = 1)
  
  # Highest should be 0
  highest_df <- data.frame(elevation =  max(targetElevations) + 0.01,
                          fractionInundation = 0)
  
  
  output_df <- bind_rows(lowest_df, middle_df, highest_df)
  return(output_df)
  
}

getDatumsForGauge <- function(station_id=9410660, 
                              startDate = '2015-01-01 00:00',
                              endDate = '2015-12-31 23:59', 
                              bufferStart = 15,
                              bufferEnd = 15,
                              datum="NAVD",
                              graph=F,
                              gauge_data = "biomass/agb_biomass_inundation/NOAA_water_levels") {
  
  print(paste("Analysing gauge ", as.character(station_id), " from ", 
              startDate, " to ", endDate, ".", sep=""))

  # We need a buffer start and end because, 1. We need to make sure we have the edges of the analysis period covered for
  # getting MHHWS and stuff.
  # Sometimes the longest period harmonic, the solar annual one, throws an error if you analyse over to small a period.
  bufferStartDate <- lubridate::ymd_hm(startDate) - lubridate::days(bufferStart)
  bufferStartDate <- toString(format(bufferStartDate, "%Y-%m-%d %H:%M"))

  bufferEndDate <- lubridate::ymd_hm(endDate) + lubridate::days(bufferEnd)
  bufferEndDate <- toString(format(bufferEndDate, "%Y-%m-%d %H:%M"))
  
  # First check to see if the data was already downloaded
  output_file <- paste(gauge_data, "/", as.character(station_id), ".csv", sep="")
  if (file.exists(output_file)) {
    
    # If it is load it
    station_file <- read_csv(output_file)
    
    # See if it has the right number of columns
    time_steps_needed <- seq(as.POSIXct(bufferStartDate, tz = "UTC"),
                             as.POSIXct(bufferEndDate, tz = "UTC"), by="6 min")
    
    if (all(time_steps_needed %in% station_file$dateTime)) {
      print("  already downloaded.") 
      wlTable <- station_file %>% 
        filter(dateTime >= ymd_hm(bufferStartDate, tz = "UTC") & dateTime <= ymd_hm(bufferEndDate, tz="UTC"))
    } else {
      # If not download, bind rows, arrange by date time and write to file
      # Run the download function
      print("  downloading and adding to file...")
      wlTable <- download6minWlData(station_id=station_id, 
                                    startDate = bufferStartDate,
                                    endDate = bufferEndDate, 
                                    datum=datum)
      
      # Filter out any obs that are in the file already
      station_file <- station_file %>%
        filter(!(dateTime %in% time_steps_needed))
      
      station_file <- station_file %>% 
        bind_rows(wlTable) %>% 
        arrange(dateTime) %>% 
        distinct()
      
      write_csv(station_file, output_file)
    }
  } else {
    # If not download, bind rows, arrange by date time and write to file
    # Run the download function
    print("  downloading and adding to file...")
    wlTable <- download6minWlData(station_id=station_id, 
                                  startDate = bufferStartDate,
                                  endDate = bufferEndDate, 
                                  datum=datum)
    
    # Filter out any obs that are in the file already
    # station_file <- station_file %>%
    #   filter(!(dateTime %in% time_steps_needed))
    
    station_file <- wlTable %>% 
      arrange(dateTime) %>% 
      distinct()
    
    write_csv(station_file, output_file)
  }
  
  print("  ... fitting custom tidal datum.")
  
  out_fig_name2 <- paste(as.character(station_id), " ", 
                         as.character(startDate),
                         " to ",
                         as.character(endDate),
                         # ".pdf", # DK commented this out on 2020-05-17 
                                   # because the file extension is added 
                                   # later on in the script
                         sep="")
  
  tidalDatums <- fitCustomTidalDatum(wlTable = wlTable, startDate = startDate, 
                                     endDate = endDate,
                                     bufferStart = bufferStart,
                                     bufferEnd = bufferEnd,
                                     graph = graph,
                                     out_fig_name = out_fig_name2)
  
  tidalDatums[[2]] <- tidalDatums[[2]] %>% 
    mutate(station_id = station_id,
           startDate = startDate,
           endDate = endDate) %>%
    rename(meters = wl) %>% 
    dplyr::select(station_id, startDate, endDate, Datum, meters, n)
  
  # Add to the master file
  # Rewrite master file
  
  return_datums <- tidalDatums[[2]]
  
  wlTable2 <- wlTable %>% 
    filter(dateTime >= ymd_hm(startDate) &
             dateTime <= ymd_hm(endDate))
  
  inundationProfile <- getPctInundationProfile(wlTable2)
  
  return(list(return_datums, inundationProfile))
  
}


