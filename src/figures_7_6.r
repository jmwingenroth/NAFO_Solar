### Most commonly converted land-use categories by county and state

library(tidyverse)
library(sf)
library(stars)

sf_use_s2(FALSE)

### Load data

nlcd_file <- list.files("L:/Project-SCC/NLCD_GIS_2001/", pattern = "img", full.names = TRUE)
nlcd_rast <- read_stars(nlcd_file)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > 2001) %>% # Drop one site built before NLCD data was collected
    st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # Match NLCD CRS

if (st_crs(nlcd_rast) != st_crs(uspvdb)) stop("CRS mismatch")

### Clip NLCD with USPVDB polygons

uspv_stars <- list()
n <- nrow(uspvdb)

for (i in 1:n) {

    if (i%%10 == 0) print(paste0("Extracted ",i," of ",n))

    uspv_stars[[i]] <- uspvdb[i,] %>%
        st_crop(x = nlcd_rast) %>%
        st_as_stars()

}

### Join NLCD data to uspvdb

uspv_stars %>%
    lapply(as_tibble) %>%
    lapply(rename, land_cover = 3) %>%
    lapply(group_by, land_cover) %>%
    lapply(tally) %>%
    lapply(filter, !is.na(land_cover)) %>%
    lapply(pivot_wider, names_from = land_cover, values_from = n) %>%
    bind_rows()
