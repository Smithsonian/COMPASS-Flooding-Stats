# View VCOS data
library(tidyverse)
library(plotly)

goodwin_vcos <- read_csv("data/water_level/NERR/gi_2023-2025.csv")
head(goodwin_vcos)
tail(goodwin_vcos)

sweethall_vcos <- read_csv("data/water_level/NERR/sh_2023-2025.csv")
head(sweethall_vcos)
tail(sweethall_vcos)

nerr_vcos_2020 <- goodwin_vcos %>% 
  bind_rows(sweethall_vcos) %>% 
  filter(SampleDateTime >= ymd_hms("2024-01-01 0:0:0") &
           SampleDateTime <= ymd_hms("2024-12-31 23:59:59")
           ) %>% 
  mutate(month = as.numeric(month(SampleDateTime)),
         year = as.numeric(year(SampleDateTime))
         )

together_plots <- ggplot(nerr_vcos_2020, aes(x = SampleDateTime, y = NAVD88_elev)) +
  geom_line(aes(color = StationNameLong)) +
  facet_wrap(.~month, scale = "free_x")

ggplotly(together_plots)

neer_vcos <- goodwin_vcos %>% 
  bind_rows(sweethall_vcos) %>% 
  mutate(month = as.numeric(month(SampleDateTime)),
         year = as.numeric(year(SampleDateTime))
  )

nerr_vcos_summary <- neer_vcos %>% 
  group_by(StationNameLong, month, year) %>% 
  summarise(max_navd88_elev = max(NAVD88_elev, na.rm = T),
            n = n())

mean_high_elev <- nerr_vcos_summary %>% 
  group_by(StationNameLong, year) %>% 
  summarise(mean_max_navd88_elev = mean(max_navd88_elev, na.rm = T),
            n = sum(n))

ggplot(nerr_vcos_summary, aes(x = year+month/12, y = max_navd88_elev)) +
  geom_point(aes(color = StationNameLong)) +
  geom_line(aes(color = StationNameLong))




