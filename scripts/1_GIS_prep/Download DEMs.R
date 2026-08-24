# download DEMS
options(timeout = 6000)

DEM_outputs <- "/Volumes/LaCie/COMPASS_hydrology/dem"

# NOAA

links_to_scrape <- c("https://coast.noaa.gov/slrdata/DEMs/MD/URLlist_MD.txt",
                    "https://coast.noaa.gov/slrdata/DEMs/CA/URLlist_CA.txt",
                    "https://coast.noaa.gov/slrdata/DEMs/VA/URLlist_VA.txt",
                    "https://coast.noaa.gov/slrdata/DEMs/MA/URLlist_MA.txt",
                    "https://coast.noaa.gov/slrdata/DEMs/LA/URLlist_LA.txt",
                    "https://coast.noaa.gov/slrdata/DEMs/NC/URLlist_NC.txt"
                    )

for (i in 1:length(links_to_scrape)) {
  
  
  these_links <- read_table(links_to_scrape[i], col_names = F)
  
  for (j in 1:nrow(these_links)) {
    
    output_file <- rev(str_split_1(these_links$X1[j], pattern = "/"))[1]
    output_path <- paste0(DEM_outputs, "/", output_file)
    
    if (file.exists(output_path)) {
     print(paste0(output_file, " done."))
    } else {
      tryCatch({download.file(these_links$X1[j],
                              output_path)},
               
               error = function(e) {
                 cat("Download failed:\n")
                 cat(e$message, "\n")
                 file.remove(output_path)
               }
      ) 
    }
  }
}
