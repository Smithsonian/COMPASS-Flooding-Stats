# What do we need? 
library(tidyverse)
library(sf)

# For each data source
# study_id
# site_id
# point_id
# year
# month
# day
# latitude
# longitude
# elevation
# geoid
# position

# Elevation sources
compiled_study <- list()
# Original Holmquist

# Upland, wetland, mid, water

og_holmquist <- read_csv("data/elevation/source_studies/SERC_holmquist/original/GcrewSurvey/gcrew_rtk_gps_2016.csv") %>%
  filter(is.na(test_type)) %>% 
  mutate(position = case_when(positional_notes == "hiddenmarsh" ~ "transition", .default = "wetland")) %>% 
  group_by(survey_id, measurement_type, test_type, transect_id, plot_id, position) %>%
  summarise(elevation_sd = sd(elevation, na.rm=T), 
    elevation = mean(elevation, na.rm=T),
            easting=mean(easting, na.rm=T),
            northing=mean(northing, na.rm=T),
            n=n()) %>% 
  ungroup()

load_up_vegetation_data <- read_csv("data/elevation/source_studies/SERC_holmquist/original/GcrewSurvey/gcrew_braun_blanquet_2016.csv") %>%
  filter(!is.na(bb_cover)) %>%
  select(plot_id, bb_cover, cover_type) %>% 
  filter(! cover_type %in% c("Dead", "BareOrWater")) %>%
  filter(! cover_type %in% c("Unknown", "NA"),
         complete.cases(cover_type)) %>% 
  group_by(plot_id) %>%
  filter(! is.na(bb_cover) & ! is.na(cover_type)) %>%
  mutate(max_coverClass = max(bb_cover, na.rm=T)) %>%
  filter(bb_cover == max_coverClass) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  mutate(survey_id = "July and August 2016 20m Grid Survey",
         year = 2016,
         month = 9,
         day = 1)

tmon_2016 <- read_csv("data/elevation/source_studies/SERC_holmquist/original/Wigham_and_Megonigal_2020_data_release/plant_species_cover.csv") %>% 
  filter(year == 2016) %>% 
  rename(transect_id = transect, plot_id = plot, cover_type = species_code) %>% 
  group_by(transect_id, plot_id) %>%
  mutate(max_coverClass = max(fractional_cover, na.rm=T)) %>%
  filter(fractional_cover == max_coverClass) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  mutate(survey_id = "June 2016 TMON Survey") %>% 
  bind_rows(load_up_vegetation_data) %>% 
  mutate(transect_id = tolower(transect_id))
  
vegetation_w_data_elevation <- left_join(og_holmquist, tmon_2016)


# Add long term occupations of benchmarks
benchmarks_holmquist <- read_csv("data/elevation/source_studies/SERC_holmquist/original/GcrewSurvey/serc_benchmarks_total_station_2016.csv") %>% 
  filter(occupation_id %in% c("a001", "a003")) %>% 
  mutate(position = "upland") %>% 
  rename(point_id = occupation_id) %>% 
  select(point_id, northing, easting, elevation, position)

# Convert UTM 18N to lat lon
holmquist_formatted <- vegetation_w_data_elevation %>% 
  rename(point_id= plot_id) %>% 
  mutate(point_id = as.character(point_id)) %>% 
  bind_rows(benchmarks_holmquist) %>% 
  st_as_sf(coords = c("easting", "northing"),
           crs = 32618) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
         ) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(study_id = "Holmquist et al 2021",
         site_id = "GCW", 
         year = 2016,
         month = 8,
         day = 1,
         geoid = "GEOID12B",
         cover_type = recode(cover_type,
                             "TYAN"="TYLA")
         ) %>% 
  select(study_id,
         survey_id,
    site_id,
     point_id,
     year,
     month,
     day,
     latitude,
     longitude,
     elevation,
     geoid,
     position,
    cover_type)

compiled_study[[1]] <- holmquist_formatted

library(leaflet)
cols <- c(RColorBrewer::brewer.pal(n = 12, name = "Set3"))

pal <- colorFactor(
  palette = cols,
  domain = levels(holmquist_formatted$cover_type)
)

leaflet(holmquist_formatted) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addCircleMarkers(color = ~pal(cover_type),
                   radius = 5, label = ~paste0(study_id, ": ", cover_type)) %>% 
  addLegend(pal = pal, values = ~cover_type)


# Bring in Blue Methane data from Koontz
blue_methane <- read_csv("data/elevation/source_studies/SERC_holmquist/original/BlueMethane/All_SERC_Points.csv")
  
# Treat EMLID and RTN differently
blue_methane_emlid <- blue_methane %>% 
  # For EMLID data
  filter(RTK_Sensor == "Emlid Reach RS2") %>% 
  # Cut out top of well
  filter(!grepl("top of well", Description),
         Point_type != "Survey mark") %>% 
  # Add 9.2 cm offset correcting error from base station 
  mutate(elevation = Elevation_m + 0.092) %>% 
  st_as_sf(coords = c("Easting", "Northing"),
           crs = 32618) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(study_id = "Blue Methane Emlid",
         site_id = "GCW", 
         point_id = Full_name,
         year = Year,
         month = Month,
         day = Day,
         geoid = "GEOID12B",
         position = "wetland"
  ) %>% 
  select(study_id,
         site_id,
         point_id,
         year,
         month,
         day,
         latitude,
         longitude,
         elevation,
         geoid,
         position)

compiled_study[[2]] <- blue_methane_emlid

# For RTN data, convert from geoid18
blue_methane_RTN <- blue_methane %>% 
  # For EMLID data
  filter(RTK_Sensor == "Trimble R12") %>% 
  # These look like they were mistakenly flipped
  rename(x = Northing,
         y = Easting) %>% 
  # Cut out top of well
  # Filter out cap tops
  filter(! grepl("CAP", Full_name),
         ! grepl("TOP", Full_name),
         is.na(Note) | Note != "No precision info, do not use."
         ) %>% 
  st_as_sf(coords = c("x", "y"),
            crs = 26985) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(study_id = "Blue Methane Trimble R12",
         site_id = "GCW", 
         elevation = Elevation_m,
         point_id = Full_name,
         year = Year,
         month = Month,
         day = Day,
         geoid = "GEOID18",
         # Classify wetlands vs uplands vs transitions
         position = case_when(grepl("WOOD", point_id) ~ "upland",
                              point_id == "COMPASS UPLAND" ~ "upland",
                              point_id == "COMPASS TRANSITION" ~ "transition",
                              point_id == "COMPASS WETLAND" ~ "wetland",
                              point_id == "COMPASS WATER" ~ "water",
                              .default = "wetland")) %>% 
  select(study_id,
         site_id,
         point_id,
         year,
         month,
         day,
         latitude,
         longitude,
         elevation,
         geoid,
         position)

compiled_study[[3]] <- blue_methane_RTN

library(readxl)
# Add synoptic site info
compass_rtk <- read_xlsx("data/elevation/source_studies/COMPASS/original/COMPASS_CBSynoptic_AQ600_RTK_AllData.xlsx")

compass_veg <- read_csv("https://raw.githubusercontent.com/COMPASS-DOE/synoptic-discrete-data/refs/heads/main/Vegetation/Ground%20cover/Synoptic_CB/2023/Raw%20Data/COMPASS_Synoptic_CB_Veg_BBScore_2023.csv")

compass_veg_processed <- compass_veg %>% 
  rename(site_id = Site_Code) %>% 
  mutate(point_id = paste0(site_id, "_", Zone, "_VegPlot_", Rep)) %>% 
  filter(is.na(Other_Cover)) %>% 
  group_by(point_id) %>% 
  mutate(max_coverClass = max(Score, na.rm=T)) %>%
  filter(Score == max_coverClass) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  select(site_id, point_id, Species_Code) %>% 
  rename(cover_class = Species_Code)

# Example target forat 
# site_id = "SWH"
# point_id = SWH_WC_VegPlot_5


# filter out low precisions
compass_rtk_processed <- compass_rtk %>% 
  # filter(V_Prec_Obs <= 0.1) %>% 
  mutate(study_id = "COMPASS-RTK") %>% 
  rename(site_id = `Site Code`, 
         point_id =`Point ID`,
         year = Year,
         month = Month,
         day = Day,
         elevation = Elevation) %>% 
  mutate(geoid = "GEOID18",
         position = tolower(Zone)
         ) %>% 
  select(study_id,
         site_id,
         point_id,
         year,
         month,
         day,
         x,
         y,
         elevation,
         geoid,
         position)

# Separate virginia and maryland
compass_rtk_MD <- compass_rtk_processed %>% 
  filter(site_id %in% c("GCW", "MSM")) %>% 
  st_as_sf(coords = c("x", "y"),
           crs = 26985) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>%
  st_drop_geometry() %>% 
  as_tibble() %>%
  left_join(compass_veg_processed)



compass_rtk_VA <- compass_rtk_processed %>% 
  filter(! site_id %in% c("GCW", "MSM")) %>% 
  st_as_sf(coords = c("x", "y"),
           crs = 32618) %>% 
  st_transform(crs = "WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>%
  st_drop_geometry() %>% 
  as_tibble() %>%
  left_join(compass_veg_processed)


# Maryland is in state plane

# Virginia is in in UTM zone 18 N

compiled_study[[4]] <- compass_rtk_MD
compiled_study[[5]] <- compass_rtk_VA


# Bring in data from soil cores
cores <- read_csv("data/elevation/source_studies/CCN/soils/CCN_cores.csv")
species <- read_csv("data/elevation/source_studies/CCN/soils/CCN_species.csv") %>% 
  rename(cover_class = species_code)

ccn_compiled <- cores %>% 
  filter(complete.cases(elevation),
         admin_division %in% c("Maryland", "Virginia")
         ) %>% 
  left_join(species) %>% 
  filter(study_id %in% c("Langston_et_al_2022",
                         "Lerberg_et_al_2025",
                         "Messerschmidt_and_Kirwan_2020",
                         "Schieder_and_Kirwan_2019",
                         "Shaw_et_al_2020",
                         "Smith_and_Kirwan_2021",
                         "Weston_et_al_2023"
                         )) %>% 
  mutate(year = ifelse(study_id == "Smith_and_Kirwan_2021", 
                       2019,
                       year),
         month = ifelse(study_id == "Smith_and_Kirwan_2021", 
                       7,
                       month),
         day = ifelse(study_id == "Smith_and_Kirwan_2021", 
                        1,
                        day),
         geoid = ifelse(year < 2020, "GEOID12b",
                        "GEOID18"
                        )) %>% 
  filter(site_id %in% c("York", "Goodwin_Island", "Moneystump_Swamp",
                        "GCREW", "Goodwin Island", "Sweet_Hall_Marsh"
                        )) %>% 
  mutate(site_id = recode(site_id, "York"="GWI",
                          "Goodwin_Island" = "GWI",
                          "Moneystump_Swamp" = "MSM",
                          "GCREW" = "GCW",
                          "Goodwin Island" = "GWI",
                          "Sweet_Hall_Marsh" = "SWH"
                          )) %>% 
  mutate(position = "wetland") %>% 
  rename(point_id = core_id) %>% 
  filter(point_id != "YRK_5") %>% 
  select(study_id,
         site_id,
         point_id,
         year,
         month,
         day,
         latitude,
         longitude,
         elevation,
         geoid,
         position,
         cover_class) %>% 
  group_by(study_id, site_id, year, month, day, latitude, longitude,
           elevation, geoid, position, cover_class
           ) %>% 
  summarise(point_id = first(point_id)
            ) %>% 
  ungroup()

# Assume ones collected before 2020 used geoid12b and those that were after used geoid18
compiled_study[[6]] <- ccn_compiled

synthesized_studies <- bind_rows(compiled_study)

synthesized_studies_wgs <- synthesized_studies %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84")

write_csv(synthesized_studies, "data/elevation/synoptic_site_elevation_compiled.csv")

# Convert all 
library(terra)

geoid12b <- rast("data/GIS/GEOIDS/g2012bu0.bin")

e <- ext(geoid12b)
ext(geoid12b) <- c(e$xmin - 360, e$xmax - 360, e$ymin, e$ymax)
print(ext(geoid12b))
plot(geoid12b)

geoid18 <- rast("data/GIS/GEOIDS/g2018u0.bin")
e2 <- ext(geoid18)
ext(geoid18) <- c(e2$xmin - 360, e2$xmax - 360, e2$ymin, e2$ymax)
print(ext(geoid18))
plot(geoid18)

synthesized_studies_proj <- synthesized_studies %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  st_transform(crs(geoid12b))

points_12b <- extract(geoid12b, synthesized_studies_proj) 
points_18 <- extract(geoid18, synthesized_studies_proj) 

synthesized_studies_geoid12b <- synthesized_studies_proj %>% 
  mutate(geoid12b = points_12b[,2],
         geoid18 = points_18[,2]
         ) %>% 
  st_transform("WGS84") %>% 
  mutate(elevation_navd88_geoid12b = ifelse(geoid == "GEOID18",
                                            elevation + geoid18 - geoid12b,
                                            elevation
                                            ))

synthesized_studies_geoid12b_export <- synthesized_studies_geoid12b %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1],
  ) %>%
  st_drop_geometry() %>% 
  as_tibble()

write_csv(synthesized_studies_geoid12b_export, 
          "synthesized_studies_geoid12b.csv")

library(leaflet)


pal <- colorNumeric(
  palette = "viridis",
  # domain = range(synthesized_studies_geoid12b$elevation_navd88_geoid12b, na.rm=T)
  domain = c(0, 0.55)
  )

leaflet(synthesized_studies_geoid12b) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addCircleMarkers(color = ~pal(elevation_navd88_geoid12b),
                   radius = 5, label = ~paste0(study_id, ": ", cover_class)) %>% 
  addLegend(pal = pal, values = ~elevation_navd88_geoid12b)

