

bufferStartDate <- lubridate::ymd_hm("2015-11-19 00:00") - lubridate::days(15)

bufferEndDate <- lubridate::ymd_hm("2017-12-11 24:59") + lubridate::days(15)

bufferStartDate <- toString(format(bufferStartDate, "%Y-%m-%d %H:%M"))
bufferEndDate <- toString(format(bufferEndDate, "%Y-%m-%d %H:%M"))

wlTable <- download6minWlData(station_id=9415144, 
                              startDate = bufferStartDate,
                              endDate = bufferEndDate, 
                              datum="NAVD")

tidalDatums <- fitCustomTidalDatum(wlTable = wlTable, 
                                   startDate = "2015-11-19 00:00", 
                                   endDate = "2017-12-11 24:59",
                                   bufferStart = 15,
                                   bufferEnd = 15,
                                   graph=T,
                                   out_fig_name = "Port Chicago 2015 to 2017",
                                   gauge_data = "temp/"
)


observed_tides <- tidalDatums[[1]] %>% 
  group_by(classifiedTides) %>% 
  summarise(wl = mean(observed))
