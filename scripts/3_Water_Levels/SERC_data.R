# NAVD88 data from annapolis
serc_files <- list.files("data/water_level/GCREW/Dock/31434457",
                         pattern = "data_*",
                         full.names = T)

source("scripts/3_Water_Levels/getTidalDatums.R")

file_list <- list()
for (i in 1:length(serc_files)) {
  
  file_i <- read_csv(serc_files[i])
  file_list[[i]] <- file_i
  
}

serc_data <- bind_rows(file_list)

library(VulnToolkit)
annapolist_data <- VulnToolkit::noaa(begindate = "20190101", enddate = "20251231", station = '8575512',
                                     interval = '6 minute', met = F, datum = "NAVD",
                                     time = "GMT")

names(annapolist_data) <- c("dateTime", "waterLevel", "station")

annapolis_datum <- fitCustomTidalDatum(wlTable = annapolist_data,
                    startDate = "2019-01-01 00:00",
                    endDate = "2019-12-31 24:59"
                    )

write_csv(annapolist_data, "data/water_level/GCREW/annapolis_2019to2025_navd88.csv")
annapolist_data <- read_csv("data/water_level/GCREW/annapolis_2019to2025_navd88.csv")
library(tidyverse)

serc_data_edit <- serc_data %>% 
  rename(station = instrument_id,
         water_level = level_m_cbs
         ) %>% 
  select(station, timestamp, water_level, error_code_cs475a2) %>%
  mutate(datum = "station datum",
         timestamp = force_tz(timestamp, "EST"))

annapolis_data_edit <- annapolist_data %>% 
  rename(timestamp = time_GMT,
         water_level = `verified water level at 8575512 (meters rel. to NAVD)`) %>% 
  select(station, timestamp, water_level) %>% 
  mutate(datum = "NAVD88") %>% 
  mutate(station = as.character(station))

stations_together <- serc_data_edit %>% 
  bind_rows(annapolis_data_edit) %>% 
  mutate(month = as.numeric(month(timestamp)),
         year = as.numeric(year(timestamp))
         ) %>% 
  group_by(station) %>% 
  mutate(mean_wl = mean(water_level, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mean_corrected_wl = water_level - mean_wl) %>% 
  arrange(timestamp, station) %>% 
  mutate(station = recode(station, "8575512"="Annapolis",
                          "USA-MDA_waterlevel_SERC-dock"="SERC dock"))


stations_together_2020 <- stations_together %>% 
  filter(year == 2024)

annapolis_plot <- ggplot(stations_together_2020, aes(x = timestamp, y = mean_corrected_wl)) +
  geom_line(aes(color = station)) + 
  facet_wrap(.~month, scale = 'free_x')

library(plotly)
ggplotly(annapolis_plot)

stations_compared <- stations_together %>% 
  select(station, timestamp, water_level, month, year) %>% 
  pivot_wider(values_from = water_level,
              names_from = station
              )

wl_match <- lm(`SERC dock`~Annapolis, data = stations_compared)
summary(wl_match)

# Annapolis water levels
serc_data_summary <- stations_together %>% 
  mutate(water_level = ifelse(station == "SERC dock", water_level - 1.55, water_level),
         datum = "NAVD88") %>% 
  mutate(month = as.numeric(month(timestamp)),
         year = as.numeric(year(timestamp))) %>% 
  group_by(station, month, year) %>% 
  summarise(max_navd88_elev = max(water_level, na.rm = T),
                     n = n()) %>% 
  filter(max_navd88_elev != -Inf)

mean_high_elev <- serc_data_summary %>% 
           group_by(station, year) %>% 
           summarise(mean_max_navd88_elev = mean(max_navd88_elev, na.rm = T),
                     n = sum(n))

ggplot(serc_data_summary, aes(x = year+month/12, y = max_navd88_elev)) +
  geom_point(aes(color = station, size = n)) +
  geom_line(aes(color = station))

