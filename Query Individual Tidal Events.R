# 

library(tidyverse)
library(lubridate)
library(VulnToolkit)

# GCW: 2024-07-15 to 2024-07-19
# GWI: 2024-09-23 to 2024-09-27
# SWH: 2024-08-19 to 2024-08-23
areas_vs_floods <- read_csv("empirical_flood_heights_vs_areas.csv")

GWI_data <- read_csv("data/water_level/NERR/gi_2023-2025.csv") 

GWI_data <- GWI_data %>% 
  filter(SampleDateTime >= ymd("2024-09-22") &
           SampleDateTime <= ymd("2024-09-28") 
           )

GWI_HL <- HL(GWI_data$NAVD88_elev,
             GWI_data$SampleDateTime) %>% 
  filter(tide == "H") %>% 
  mutate(site_id = "GoodwinIsland")


SWM_data <- read_csv("data/water_level/NERR/sh_2023-2025.csv") 

SWM_data <- SWM_data %>% 
  filter(SampleDateTime >= ymd("2024-08-18") &
           SampleDateTime <= ymd("2024-08-24") 
  )

SWM_HL <- HL(SWM_data$NAVD88_elev,
             SWM_data$SampleDateTime) %>% 
  filter(tide == "H")  %>% 
  mutate(site_id = "SweetHall")



plot(SWM_data$NAVD88_elev)

gcw_data <- read_csv("data/water_level/GCREW/annapolis_2019to2025_navd88.csv") %>% 
  mutate(time_LST = with_tz(time_GMT, "America/New_York")) %>% 
  filter(time_LST >= ymd("2024-07-14") &
           time_LST <= ymd(" 2024-07-20") 
  ) %>% 
  rename(wl = `verified water level at 8575512 (meters rel. to NAVD)`)

gcw_HL <-  HL(gcw_data$wl,
              gcw_data$time_LST) %>% 
  filter(tide == "H")  %>% 
  mutate(site_id = "GCREW")

all_High_Tides <- bind_rows(GWI_HL, gcw_HL, SWM_HL)
all_hourly <- bind_rows()

sites <- c("GoodwinIsland", "GCREW", "SweetHall")
output_list <- list()
for (i in 1:length(sites)) {
  
  site_i <- sites[i]
  
  temp_areas <- areas_vs_floods %>% 
    filter(site_id == site_i)
  
  temp_floods <- all_High_Tides %>% 
    filter(site_id == site_i)
  
  new_floods <- approx(temp_areas$flood_elevation, 
                       temp_areas$area_m2,
                       xout = temp_floods$level
                       )
  
  temp_floods$area_m2 <- new_floods$y
  
  output_list[[i]] <- temp_floods
  
  
  
}

output_areas <- bind_rows(output_list)

write_csv(output_areas, "areas_per_tide_measured.csv")


# 
GWI_data_hourly <- GWI_data %>% 
  rename(time_LST = SampleDateTime,
         wl = NAVD88_elev)  %>% 
  mutate(site_id = "GoodwinIsland") %>% 
  dplyr::select(site_id, time_LST, wl)

SWM_data_hourly <- SWM_data %>% 
  rename(time_LST = SampleDateTime,
         wl = NAVD88_elev) %>% 
  mutate(site_id = "SweetHall") %>% 
  dplyr::select(site_id, time_LST, wl)

gcw_data_hourly <- gcw_data %>% 
  mutate(site_id = "GCREW") %>% 
  dplyr::select(site_id, time_LST, wl)

all_together_raw_ts <- bind_rows(GWI_data_hourly, SWM_data_hourly, gcw_data_hourly) %>% 
  arrange(site_id, time_LST)

all_together_hourly <- all_together_raw_ts %>% 
  mutate(year = year(time_LST),
         month = month(time_LST),
         day = day(time_LST),
         hour = hour(time_LST)
         ) %>% 
  group_by(site_id, year, month, day, hour) %>% 
  summarise(wl = mean(wl), 
            time_LST = mean(time_LST),
            n=n())

ggplot(all_together_hourly, aes(x=time_LST, y = wl)) +
  geom_point() +
  geom_line(data = all_together_raw_ts) +
  facet_wrap(.~site_id, scale = "free")


output_list <- list()
for (i in 1:length(sites)) {
  
  site_i <- sites[i]
  
  temp_areas <- areas_vs_floods %>% 
    filter(site_id == site_i)
  
  temp_floods <- all_together_hourly %>% 
    filter(site_id == site_i)
  
  new_floods <- approx(temp_areas$flood_elevation, 
                       temp_areas$area_m2,
                       xout = temp_floods$wl,
                       rule = 2
  )
  
  temp_floods$area_m2 <- new_floods$y
  
  output_list[[i]] <- temp_floods
  
}

output_areas <- bind_rows(output_list)

write_csv(output_areas, "areas_per_wl_hourly.csv")





