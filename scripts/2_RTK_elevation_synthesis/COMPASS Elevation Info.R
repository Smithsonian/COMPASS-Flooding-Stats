# What do we need? 
library(tidyverse)
library(sf)
library(sp)

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

tmon_species_data <- read_csv("data/elevation/source_studies/SERC_holmquist/original/Wigham_and_Megonigal_2020_data_release/plant_species_codes.csv") %>% 
  rename(cover_class = species_code) %>% 
  mutate(Genus_species = paste(genus, species)) %>% 
  dplyr::select(cover_class, Genus_species)

load_up_vegetation_data <- read_csv("data/elevation/source_studies/SERC_holmquist/original/GcrewSurvey/gcrew_braun_blanquet_2016.csv") %>%
  filter(!is.na(bb_cover)) %>%
  rename(cover_class = cover_type) %>% 
  dplyr::select(plot_id, bb_cover, cover_class) %>% 
  filter(! cover_class %in% c("Dead", "BareOrWater")) %>%
  filter(! cover_class %in% c("Unknown", "NA"),
         complete.cases(cover_class)) %>% 
  group_by(plot_id) %>%
  filter(! is.na(bb_cover) & ! is.na(cover_class)) %>%
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
  mutate(species_code = recode(species_code, "SPPA"="C4", "DISP"= "C4")) %>% 
  rename(transect_id = transect, plot_id = plot, cover_class = species_code) %>% 
  group_by(transect_id, plot_id, cover_class) %>%
  summarise(fractional_cover = sum(fractional_cover)) %>% 
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
  dplyr::select(point_id, northing, easting, elevation, position)

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
         cover_class = recode(cover_class,
                              "TYLA" = "TYAN")
         ) %>% 
  dplyr::select(study_id,
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
    cover_class) %>% 
  left_join(tmon_species_data) %>% 
  mutate(cover_class = ifelse(is.na(Genus_species), cover_class, Genus_species))


compiled_study[[1]] <- holmquist_formatted

library(leaflet)
cols <- c(RColorBrewer::brewer.pal(n = 12, name = "Set3"))

pal <- colorFactor(
  palette = cols,
  domain = levels(holmquist_formatted$cover_class)
)

leaflet(holmquist_formatted) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addCircleMarkers(color = ~pal(cover_class),
                   radius = 5, label = ~paste0(study_id, ": ", cover_class)) %>% 
  addLegend(pal = pal, values = ~cover_class)


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
  dplyr::select(study_id,
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
  dplyr::select(study_id,
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

compass_spp <- read_csv("https://raw.githubusercontent.com/COMPASS-DOE/synoptic-discrete-data/refs/heads/main/Vegetation/Ground%20cover/Synoptic_CB/COMPASS_CBSynoptic_SpeciesList_2025.csv", skip = 1) %>% 
  rename(cover_class = Species_Code) %>% 
  dplyr::select(cover_class, Genus_species) %>% 
  filter(complete.cases(cover_class))

compass_veg_processed <- compass_veg %>% 
  rename(site_id = Site_Code) %>% 
  mutate(point_id = paste0(site_id, "_", Zone, "_VegPlot_", Rep)) %>% 
  filter(is.na(Other_Cover)) %>% 
  group_by(point_id) %>% 
  mutate(max_coverClass = max(Score, na.rm=T)) %>%
  filter(Score == max_coverClass) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  dplyr::select(site_id, point_id, Species_Code) %>% 
  rename(cover_class = Species_Code) %>% 
  left_join(compass_spp) %>% 
  mutate(cover_class = ifelse(is.na(Genus_species), cover_class, Genus_species)) %>% 
  dplyr::select(-Genus_species)

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
  dplyr::select(study_id,
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
  left_join(compass_veg_processed) %>% 
  left_join(compass_spp) %>% 
  mutate(cover_class = recode(cover_class, "Spartina patens" = "C4",
                              "Distichlis spicata" = "C4")) %>% 
  group_by(study_id, site_id, point_id) %>% 
  slice_sample(n=1) %>% 
  dplyr::select(-Genus_species)


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
         geoid = ifelse(year < 2020, "GEOID12B",
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
  dplyr::select(study_id,
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
  ungroup() %>% 
  filter(cover_class != "Pinus") %>% 
  mutate(cover_class = recode(cover_class, "Spartina patens" = "C4",
                              "Distichlis spicata" = "C4",
                              "Distichlis" = "C4",
                              "Phragmites"="Phragmites australis")) %>% 
  
  group_by(study_id, site_id, point_id) %>% 
  slice_sample(n = 1)

# Assume ones collected before 2020 used geoid12b and those that were after used geoid18
compiled_study[[6]] <- ccn_compiled


# Sage Lot Pond
slp <- read_xlsx("data/elevation/source_studies/CapeCod_Eagle/original/Sage Lot Pond 2017_2018 RTK Surveys.xlsx")

slp_converted <- slp %>% 
  mutate(study_id = "Eagle_et_al",
         site_id = "SageLotPond",
         point_id = 1:n()) %>% 
  st_as_sf(coords = c("easting_m", "northing_m"),
           crs = CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +type=crs")) %>% 
  st_transform("WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]
         ) %>% 
  st_drop_geometry() %>%
  as_tibble() %>% 
  mutate(year = 2018,
         month = NA,
         day = NA,
         elevation=altitude_m,
         geoid = "GEOID12B",
         position = "wetland",
         point_id = as.character(point_id)
         ) %>% 
  dplyr::select(study_id,
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
  
compiled_study[[7]] <- slp_converted
  


# Barataria
barataria_elevation <- read_xlsx("data/elevation/source_studies/Mariotti_Barataria/1.1/data/0-data/BARATARIA_BASIN_final_SUBMITTEDtoNCEI.xlsx", sheet = 4)

barataria_plants <- read_xlsx("data/elevation/source_studies/Mariotti_Barataria/1.1/data/0-data/BARATARIA_BASIN_final_SUBMITTEDtoNCEI.xlsx", sheet = 5)


barataria_plant_dom <- barataria_plants %>% 
  filter(Genus_Species != "dead") %>% 
  group_by(SiteNum, Location) %>% 
  mutate(max_PrecCover = max(PercCover)) %>% 
  ungroup() %>% 
  filter(PercCover == max_PrecCover) %>% 
  group_by(SiteNum, Location) %>% 
  slice_sample(n = 1)

barataria_plants_analysed <- barataria_elevation %>% 
  left_join(barataria_plant_dom) %>%
  filter(complete.cases(Elevation_m_NAVD88)) %>% 
  mutate(study_id = "Mariotti_et_al_2024",
    site_id = as.character(SiteNum),
    point_id = paste(SiteNum, Location, Replicate_Profile_Num, sep="-"),
    year = year(ymd_hms(Time)),
    month = month(ymd_hms(Time)),
    day = day(ymd_hms(Time)),
    latitude = Latitude,
    longitude = Longitude,
    elevation = Elevation_m_NAVD88,
    geoid = "GEOID18",
    position = "wetland",
    cover_class = Genus_Species) %>% 
  dplyr::select(study_id,
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
  filter(complete.cases(cover_class)) %>% 
  mutate(cover_class = recode(cover_class,
                              "Distichlis spicata" = "C4",
                              "Sporobolus pumilus" = "C4",
                              "Sporobolus alterniflorus" = "Spartina alterniflora"
                              ))

compiled_study[[8]] <- barataria_plants_analysed 

# Eden Landing
eden_landing <- read_csv("data/elevation/source_studies/EdenLanding_Oikawa/original/elevation_data_20200826_RTKGPS.csv") %>% 
  mutate(study_id = "Shahan_et_al_2024",
         site_id = "Eden Landing",
         point_id = as.character(collar_num),
         latitude = Latitude_dd,
         longitude = Longitude_dd,
         elevation = Elevation_m,
         year = 2020,
         geoid = "GEOID18",
         position = ifelse(is.na(landcover_type), "upland", "wetland"),
         cover_class = recode(landcover_type,
                              "spartina"="Spartina foliosa",
                              "pickleweed"="Salicornia pacifica",
                              )
         ) %>% 
  dplyr::select(study_id,
         site_id,
         point_id,
         year,
         # month,
         # day,
         latitude,
         longitude,
         elevation,
         geoid,
         position,
         cover_class)

compiled_study[[9]] <- eden_landing 

library(readxl)

# Rush Ranch
# rr_cover <- readxl::read_xls("data/elevation/source_studies/SFbay_ShileBeers/original/channel vegetation data.xls",
#                        sheet = 2)
# rr_species <- readxl::read_xls("data/elevation/source_studies/SFbay_ShileBeers/original/channel vegetation data.xls",
#                                sheet = 8)
# 
# removeThese <-c("THATCH", "CHANNEL", "BARE")
# 
# rr_tall <- rr_cover %>%
#   select(-c(Notes)) %>%
#   gather(key="Code", value="ordinal_cover_class", Lela:Hebi) %>%
#   filter(complete.cases(.)) %>%
#   arrange(Point_ID, Code) %>%
#   filter(! (Code %in% removeThese)) %>%
#   mutate(site_id = "Rush Ranch") %>%
#   rename(point_id = Point_ID,
#          UTM_x = Easting_Lo,
#          UTM_y = Northing_L,
#          elevation = Ortho_Heig) %>%
#   mutate(Date_Time = mdy_hms(Date_Time),
#          year = year(Date_Time),
#          month = month(Date_Time),
#          day = day(Date_Time)) %>%
#   select(site_id, point_id, year, month, day,
#          UTM_x, UTM_y, elevation,
#          Code, ordinal_cover_class) %>%
#   mutate(Code = recode(Code, "Losc..41"="Losc", 
#                                           "Losc..27"="Losc", "Losc...41"="Losc", "Losc...27"="Losc")) %>% 
#   left_join(rr_species) %>% 
#   group_by(site_id, point_id) %>% 
#   mutate(max_cover_class = max(ordinal_cover_class)) %>% 
#   ungroup() %>% 
#   filter(max_cover_class == ordinal_cover_class) %>% 
#   group_by(site_id, point_id) %>% 
#   slice_sample(n = 1) %>% 
#   ungroup() %>% 
#   mutate(position = "wetland") %>% 
#   rename(cover_class = Species) %>% 
#   mutate(study_id = "Schile-Beers et al 2014 - Channel Vegetation") %>% 
#   select(study_id,
#     site_id,
#          point_id,
#          year,
#          month,
#          day,
#          UTM_x, UTM_y,
#          elevation,
#          position,
#          cover_class) %>% 
#   mutate(cover_class = str_remove_all(cover_class, "\\?")
#          )

rr2 <-  readxl::read_xlsx("data/elevation/source_studies/SFbay_ShileBeers/original/summary of RTK data.xlsx",
                         sheet = 3)
rr3 <- readxl::read_xlsx("data/elevation/source_studies/SFbay_ShileBeers/original/summary of RTK data.xlsx",
                        sheet = 4)

cc1 <- readxl::read_xlsx("data/elevation/source_studies/SFbay_ShileBeers/original/summary of RTK data.xlsx",
                         sheet = 2)

pr1 <- readxl::read_xlsx("data/elevation/source_studies/SFbay_ShileBeers/original/summary of RTK data.xlsx",
                         sheet = 1)

rr_2and3 <- rr2 %>% 
  bind_rows(rr3) %>% 
  rename(point_id = Point_ID,
         UTM_x = Easting_Lo,
         UTM_y = Northing_L,
         elevation = Ortho_Heig) %>%
  mutate(Date_Time = mdy_hms(Date_Time),
         year = year(Date_Time),
         month = month(Date_Time),
         day = day(Date_Time)) %>% 
  mutate(position = ifelse(Species == "not tidal",
                           "upland", "wetland"),
    cover_class = case_when(Species == "mudflat" ~ "mudflat",
                            Species == "S. acutus" ~ "Schoenoplectus acutus",
                            Species == "S. californicus" ~ "Schoenoplectus californicus",
                            Species == "Cordylanthus molis molis"  ~ "high marsh",
                            Species == "unknown but likely high marsh mixture (Sapa, Juba, Jaca, Trma)" ~ "high marsh",
                            Species == "S. americanus" ~ "Schoenoplectus americanus",
                            .default = NA
                         )) %>% 
  mutate(site_id = "Rush Ranch") %>%
  mutate(study_id = "Schile-Beers et al 2014 - RTK Transect") %>% 
  dplyr::select(site_id,
         study_id,
         point_id,
         year,
         month,
         day,
         UTM_x, UTM_y,
         elevation,
         position,
         cover_class) 


lsb_together <- rr_2and3 %>% 
  # rr_tall %>% 
  # bind_rows(rr_2and3) %>% 
  mutate(geoid="GEOID09") %>% 
  st_as_sf(coords = c("UTM_x", "UTM_y"), crs = CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +type=crs")) %>% 
  st_transform("WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]) %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  dplyr::select(study_id,
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
         cover_class)
  

compiled_study[[10]] <- lsb_together 

smo_data <- read_xlsx("data/elevation/source_studies/WaquoitBay_NERR/original/SMO Dataset_JWM (9.14.11).xlsx")

convert_dms <- function(x) {

  # Standardize symbols
  x <- x |>
    str_replace_all("°|o", "°") |>
    str_replace_all("[’']", "'") |>
    str_replace_all('[”"]', '"') |>
    str_squish()

  # Extract components
  m <- str_match(
    x,
    "^([NSEW])\\s*(\\d+)°\\s*(\\d+)'\\s*([0-9.]+)\"$"
  )

  dec <- as.numeric(m[,3]) +
    as.numeric(m[,4])/60 +
    as.numeric(m[,5])/3600

  dec[m[,2] %in% c("S", "W")] <-
    -dec[m[,2] %in% c("S", "W")]

  dec
}

smo_analysed <- smo_data %>%
  dplyr::select(Waterbody:Trash) %>%
  filter(complete.cases(Latitude, Longitude)) %>%
  pivot_longer(cols = `Total Cover`:Trash,
               names_to = "cover_class",
               values_to = "cover"
  ) %>%
  filter(complete.cases(cover)) %>%
  mutate(cover_class =  str_remove(cover_class, "\\.\\.\\..*$"),
         cover_class = str_remove(cover_class, "\\s*\\([^)]*\\)")) %>%
  filter(! cover_class %in% c("Total Cover", "Dead", "Wrack", "Water", "shells",
                              "Perriwinkle snails", "Trash", "Unknown")) %>%
  mutate(cover_class = recode(cover_class, "Bare"="mudflat",
                              "Distichlis spicata"="C4",
                              "Spartina patens" = "C4"
  )) %>%
  group_by(Waterbody, Section, Transect, `Plot #`, cover_class,PLOTNUMBER, Latitude, Longitude, `Date of Veg Survey`, `NAVD 88 from other source`) %>%
  summarise(cover = sum(cover)) %>%
  group_by(Waterbody, Section, Transect, `Plot #`) %>%
  mutate(max_cover = max(cover)) %>%
  ungroup() %>%
  filter(cover == max_cover) %>%
  group_by(Waterbody, Section, Transect, `Plot #`) %>%
  slice_sample(n=1) %>%
  mutate(latitude = convert_dms(Latitude),
         longitude = convert_dms(Longitude),
         study_id = "SMO_WQB_NERR",
         site_id = "SageLotPond",
         transect_id = paste(Section, Transect, sep = "-"),
         point_id = PLOTNUMBER,
         year = year(ymd(`Date of Veg Survey`)),
         month = month(ymd(`Date of Veg Survey`)),
         day = day(ymd(`Date of Veg Survey`)),
         elevation = as.numeric(`NAVD 88 from other source`),
         geoid = "GEOID09",
         position = "wetland"
         ) %>%
  dplyr::select(study_id,
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
         cover_class)

compiled_study[[11]] <- smo_analysed

sf_nerr <- read_csv("data/elevation/source_studies/SF_NERR/baydelta_plants_cover_height.csv",
                    guess_max = 10000)
  
names(sf_nerr) <- str_remove_all(names(sf_nerr), " or californicus")

sf_nerr_cleanup <- sf_nerr %>% 
  filter(Site == "Rush Ranch") %>% 
  pivot_longer(
    cols = -c(Site:`Litter Cover`),
    names_to = c("Species", ".value"),
    names_pattern = "^(\\S+ \\S+) (.+)$") %>% 
  filter(Cover > 0 & !is.na(Cover)) %>% 
  group_by(Site, Region, Date, Northing, Easting, Elevation, `Channel Distance`) %>% 
  mutate(Max_Cover = max(Cover)) %>% 
  ungroup() %>% 
  filter(Cover == Max_Cover) %>% 
  group_by(Site, Region, Date, Northing, Easting, Elevation, `Channel Distance`) %>% 
  slice_sample(n=1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("Easting", "Northing"), crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +type=crs")) %>% 
  st_transform("WGS84") %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(study_id = "Rankin et al 2024",
         site_id = "Rush Ranch",
         point_id = as.character(1:n()),
         year = year(mdy(Date)),
         month = month(mdy(Date)),
         day = day(mdy(Date)),
         elevation = Elevation,
         geoid = "GEOID12b",
         position = "wetland",
         cover_class = Species
         ) %>% 
  dplyr::select(study_id,
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
                cover_class)
  
compiled_study[[11]] <- sf_nerr_cleanup

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

geoid9 <- rast("data/GIS/GEOIDS/g2009u0.tif")
plot(geoid9)

synthesized_studies_proj <- synthesized_studies %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
  st_transform(crs(geoid12b))

points_12b <- terra::extract(geoid12b, synthesized_studies_proj) 
points_18 <- terra::extract(geoid18, synthesized_studies_proj) 
points_09 <- terra::extract(geoid9, synthesized_studies_proj) 

synthesized_studies_geoid12b <- synthesized_studies_proj %>% 
  mutate(geoid12b = points_12b[,2],
         geoid18 = points_18[,2],
         geoid09 = points_09[,2]
         ) %>% 
  st_transform("WGS84") %>% 
  mutate(elevation_navd88_geoid12b = case_when(geoid == "GEOID18" ~
                                            elevation + geoid18 - geoid12b,
                                            geoid == "GEOID09" ~ elevation + geoid09 - geoid12b,
                                            .default = elevation
                                            ))

synthesized_studies_geoid12b_export <- synthesized_studies_geoid12b %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]
  ) %>%
  st_drop_geometry() %>% 
  as_tibble()
  

write_csv(synthesized_studies_geoid12b_export, 
          "synthesized_studies_geoid12b.csv")

library(leaflet)


pal <- colorNumeric(
  palette = "viridis",
  # domain = range(synthesized_studies_geoid12b$elevation_navd88_geoid12b, na.rm=T)
  domain = c(0, 2)
  )

leaflet(synthesized_studies_geoid12b) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addCircleMarkers(color = ~pal(elevation_navd88_geoid12b),
                   radius = 5, label = ~paste0(study_id, ": ", elevation)) %>% 
  addLegend(pal = pal, values = ~elevation_navd88_geoid12b)

