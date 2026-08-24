# Point analysis
library(tidyverse)

error_points <- read_csv("elevation_errors_rtk.csv") %>% 
  filter(position == "wetland")






data_exploration <- ggplot(error_points, aes(x = elevation_navd88_geoid12b,
                               y = elevation_DEM)) +
  geom_point(aes(shape = position, text = study_id)) +
  facet_wrap(.~site_id, scale = "free") +
  geom_abline(slope = 1, intercept = 0)
(data_exploration)

error_summary <- error_points %>% 
  group_by(site_id) %>% 
  summarise(mean_error = mean(elevation_error, na.rm = T),
            error_lower = quantile(elevation_error, 0.25, na.rm = T),
            error_upper = quantile(elevation_error, 0.75, na.rm = T),
            n = n(),
            sd_error = sd(elevation_error,  na.rm = T)
            ) %>% 
  ungroup() %>% 
  mutate(se = sd_error / sqrt(n))
(error_summary)
View(error_summary)
