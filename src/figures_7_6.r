### Most commonly converted land-use categories by county and state

library(tidyverse)
library(sf)
library(stars)
library(terra)
library(tidyterra)

sf_use_s2(FALSE)

### Load data

nlcd_file <- list.files("L:/Project-SCC/NLCD_GIS_2001/", pattern = "img", full.names = TRUE)
nlcd_rast <- read_stars(nlcd_file)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > 2001) %>% # Drop one site built before NLCD data was collected
    st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # Match NLCD CRS

if (st_crs(nlcd_rast) != st_crs(uspvdb)) stop("CRS mismatch")

### Clip NLCD with USPVDB polygons

uspv_rast_list <- list()
for (i in 1:nrow(uspvdb)) {
    print(paste0("Cropping #",i))
    uspv_rast_list[[i]] <- st_crop(nlcd_rast, uspvdb[i,])
}
