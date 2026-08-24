# Upload
library(cowplot)
library(gridExtra)
library(tidyverse)
library(sf)
library(terra)
library(maptiles)

# Site ID 

# Create a 
lateral_flux_stations <- read_csv("LateralFluxStations.csv")

output_table <- read_csv("empirical_flood_heights_vs_areas.csv") %>% 
  filter(site_id %in% c("GCREW", "GoodwinIsland", "SweetHall")) %>% 
  mutate(site_id = recode(site_id, "GCREW" = "GCW", 
                          "GoodwinIsland" = "GWI",
                          "SweetHall" = "SWH"
                          )) %>% 
  mutate(site_id = recode(site_id, 
                          "GCW" = "Mesohaline",
                          "GWI" = "Polyhaline",
                          "SWH" = "Oligohaline"
  ),
  site_id = factor(site_id, levels = c("Oligohaline", "Mesohaline", "Polyhaline")))


datum_vis <- read_csv("datums_output.csv") %>% 
  filter(Datum %in% c("HOT", "HAT", "MHHWS", "MHHW", "MLHW")) %>% 
  filter(site_id %in% c("GCREW", "GoodwinIsland", "SweetHall")) %>% 
  mutate(Datum = factor(Datum, levels = c("HOT", "HAT", "MHHWS", "MHHW", "MLHW"))) %>% 
  mutate(site_id = recode(site_id, "GCREW" = "GCW", 
                          "GoodwinIsland" = "GWI",
                          "SweetHall" = "SWH"
  )) %>% 
  mutate(site_id = recode(site_id, 
                          "GCW" = "Mesohaline",
                          "GWI" = "Polyhaline",
                          "SWH" = "Oligohaline"
  ),
  site_id = factor(site_id, levels = c("Oligohaline", "Mesohaline", "Polyhaline")))


# Iterate through

unique_sites <- c("SweetHall", "GCREW", "GoodwinIsland")
name_abbrevs <-c("Oligohaline", "Mesohaline", "Polyhaline")

out_links <- list()

for (i in 1:length(unique_sites)) {
  
  temp_site_id <- unique_sites[i]
  
  prob_surface <- terra::rast(paste0("data/GIS/DEM_analysis/",
                                           temp_site_id, "/", temp_site_id,
                                           "_footprint.tif"))
  
  pour_point <- read_sf(paste0("data/GIS/DEM_analysis/",
                               temp_site_id, "/", temp_site_id,
                               "_flume_snapped.shp"))
  
  # Write to file
  
  buffer_frac <- 0.25  # 25% padding around the combined extent
  
  e <- ext(c(terra::trim(prob_surface)))
  dx <- (e[2] - e[1]) * buffer_frac
  dy <- (e[4] - e[3]) * buffer_frac
  map_extent <- ext(e[1] - dx, e[2] + dx, e[3] - dy, e[4] + dy)
  
  basemap <- get_tiles(
    x        = map_extent,
    provider = "Esri.WorldImagery",
    crop     = TRUE,
    zoom     = 17  # increase for more detail (slower/larger download), decrease for speed
  )
  
  basemap_proj <- project(basemap, "EPSG:3857")
  prob_proj <- project(prob_surface, "EPSG:3857")
  pour_point_proj <- pour_point %>% st_transform("EPSG:3857")
  
  out_links[[i]] <-  paste0("steph_maps/", temp_site_id, "_footprint_map.png")
  
  png(out_links[[i]], width = 1000, height = 1000, res = 300)
  
  if (i == 3) {
    these_mar <- c(0.1, 0.1, 2.5, 4)
  } else {
    these_mar <- c(0.1, 0.1, 2.5, 0.1)
  }
  
  plotRGB(basemap_proj, mar = these_mar)
  
  plot(
    prob_proj,
    add     = TRUE,
    col     = hcl.colors(100, "Inferno", rev = F),
    alpha   = 0.7,
    legend  = (i == 3),
    plg     = list(title = "Flood\nfrequency", cex = 1)
  )
  
  # sensor location
  points(
    pour_point_proj,
    pch = 24,        # triangle, good visibility against both imagery and heatmap
    cex = 1.6,
    bg  = "white",
    col = "black",
    lwd = 1.5
  )
  
  # scale bar (terra native, no extra package needed)
  sbar(
    d       = NULL,        # auto-scaled to plot extent
    type    = "bar",
    below   = "meters",
    cex     = 0.7
  )
  dev.off()
  
  
}


these_grobs <- list()

for (i in 1:length(unique_sites)) {
  
  this_plot <- ggdraw() + 
    draw_image(out_links[[i]]) +
    draw_label(name_abbrevs[i],
               y = 0.7, 
               color = "white",
               size = 10)
  
  these_grobs[[i]] <- this_plot
  
}


these_grobs[[4]] <- ggplot(output_table, aes(x = flood_elevation, y = area_m2)) +
  geom_point() +
  geom_line() +
  geom_vline(data = datum_vis, aes(xintercept=meters, lty = Datum, color = Datum)) +
  facet_wrap(.~site_id, scale = "free") +
  ylab(expression("Area (m"^2*")"))  +
  xlab("Flood Elevation (m NAVD88)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


grid.arrange(grobs = these_grobs,
            layout_matrix = matrix(c(1,2,3,4,4,4),
                                   byrow = T,
                                   nrow = 2
            )
)

output_img <- arrangeGrob(grobs = these_grobs,
             layout_matrix = matrix(c(1,2,3,4,4,4),
                                    byrow = T,
                                    nrow = 2
                                    )
             )

ggsave("Steph Compbo Maps and Hydrographs.jpg",  width = 8.5,
       height = 6, output_img)

