################################################################
### Download and prepare osm data for accessibility analysis ###
################################################################

# load libraries
library(dplyr)
library(tidyr)
library(sf)
library(osmextract)


# set working directory MacOS
wd = "~/Dropbox (ScreenForBio)/Mwezi_B_Mugerwa/IMac/IZW/my_PhD/mugerwa_2019_02_chapter2/"
setwd(wd)

# CHANGE path-to-folder where you want to save the osm data
#  --> @Badru
# oe_download_directory <- "D:/Dateien/Uni/Eagle_Master/Hiwijob_IZW/GEE_Centralannamites_new/osm_badru"
oe_download_directory <- (paste0(wd,"/data-raw/geo-raw/shp/remoteness/YAS/"))


## Download ####################################################################################################

# TYPE in name of country/place where you want to extact the data
#  --> @Badru
its_details <- oe_match("Ecuador")

# More infos at https://cran.r-project.org/web/packages/osmextract/vignettes/osmextract.html
# (Object will return a named list of length two with the URL and the size (in bytes) of a .osm.pbf file
# representing a geographical zone stored by one of the supported providers)


# Download osm.pbf file to folder
its_pbf <- oe_download(
  file_url = its_details$url,
  file_size = its_details$file_size,
  #provider = "test",
  download_directory = oe_download_directory
)


# The .pbf files processed by GDAL are usually categorized into 5 layers,
# named points, lines, multilinestrings, multipolygons and other_relations.
# The .pbf files always contain all five layers:
st_layers(its_pbf, do_count = TRUE)

# The oe_vectortranslate() function can covert only one layer at a time.
# By default, the function will convert the lines layer, but you can change that using the parameter layer.
its_gpkg = oe_vectortranslate(its_pbf)
st_layers(its_gpkg, do_count = TRUE)
oe_get_keys(its_gpkg, layer = "lines")

# for points
its_gpkg_points = oe_vectortranslate(its_pbf, layer = "points")
st_layers(its_gpkg_points, do_count = TRUE)
oe_get_keys(its_gpkg_points, layer = "points")

# # Nevertheless, several layers with different names can be stored in the same .gpkg file.
# its_gpkg = oe_vectortranslate(its_pbf, layer = "multilinestrings")
# The corresponding gpkg file was already detected. Skip vectortranslate operations.
# st_layers(its_gpkg, do_count = TRUE)

## Postprocess #################################################################################################

# read gpkg into R for post processing
its_lines <- oe_read(its_gpkg)
its_points <- oe_read(its_gpkg_points)

plot(its_lines["highway"], lwd = 2, key.pos = NULL)

# select only interesting columns in R
# osm = its_lines2 %>% select(c("osm_id","name","highway"))
osm_places = its_points2 %>% select(c("osm_id","name","house"))

# remove NA's in highway column
osm2 <- osm[!is.na(osm$highway),]

# check all road categories in dataset
unique(osm2$highway)

# DEFINE all categories you don't want to include to analysis because they cannot be accessed by car/motorcycle
#  --> @Badru add/remove names if necessary
NAMES_list <- c("bridleway", "cycleway","footway", "path", "pedestrian", "steps", "track",
                paste0("track_grade", 1:5), "construction","planned","rest_area","corridor")

# remove all rows with categories listed above
osm3 <- osm2[ ! osm2$highway %in% NAMES_list, ]
osm3 <- osm2[ ! osm2$places %in% NAMES_list, ]

# check if all wanted categories are included
unique(osm3$highway)

# Plot selected roads
plot(osm3$geometry)


# Export dataset as shapefile to oe_download_directory
#  --> @Badru change name
st_write(osm3, paste0(oe_download_directory,"/geofabrik_ecuador-latest_edit.shp"))

