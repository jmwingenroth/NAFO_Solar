##### Most common land cover converted to solar facilities by county/state #####

### Load packages

library(tidyverse)
library(sf)
library(stars)
library(tigris)
library(spData)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

### Load data

nlcd_file <- list.files("L:/Project-SCC/NLCD_GIS_2001/", pattern = "img", full.names = TRUE)
nlcd_rast <- read_stars(nlcd_file)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > 2001) %>% # Drop one site built before NLCD data was collected
    filter(!p_state %in% c("HI", "AK")) %>% # Drop non-contiguous states
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

    if (i == n) print("Finished extracting NLCD values at USPVDB sites")

}

### Aggregate NLCD data to counties

uspv_lc <- uspv_stars %>%
    lapply(as_tibble) %>%
    lapply(rename, land_cover = 3) %>%
    lapply(group_by, land_cover) %>%
    lapply(tally) %>%
    lapply(filter, !is.na(land_cover)) %>%
    lapply(pivot_wider, names_from = land_cover, values_from = n) %>%
    bind_rows()

county_lc <- uspvdb %>%
    st_drop_geometry() %>%
    select(p_county, p_state, p_area) %>%
    bind_cols(uspv_lc) %>% # Order is conserved when clipping
    mutate(county_id = if_else(p_state == "CT", "CT combined", paste(p_state, p_county))) %>%
    group_by(p_state, county_id) %>%
    summarise(
        across(`Hay/Pasture`:`Open Water`, \(x) sum(x, na.rm = TRUE)*30^2), # Convert to m^2
        solar_area = sum(p_area)
    ) %>% 
    ungroup() %>%
    transmute(
        p_state,
        county_id,
        solar_area,
        Water = `Open Water`,
        Developed = rowSums(across(contains("Developed"))),
        Forest = rowSums(across(contains("Forest"))),
        Barren = rowSums(across(contains("Barren"))),
        Shrubland = rowSums(across(contains("Scrub"))),
        Herbaceous,
        Agricultural = `Hay/Pasture` + `Cultivated Crops`,
        Wetlands = rowSums(across(contains("Wetlands")))
    )

### Add in geographic data

county_raw <- tigris::counties()

county_tidy <-  county_raw %>%
    left_join(tigris::fips_codes, by = c("STATEFP" = "state_code", "COUNTYFP" = "county_code")) %>%
    mutate(county_id = if_else(state == "CT", "CT combined", paste(state, NAME))) %>%
    mutate(county_id = if_else(str_detect(county_id, "^NM.*Ana$"), "NM Dona Ana", county_id)) %>%
    group_by(state, county_id) %>%
    summarise(county_area = sum(ALAND))

state_key <- tibble(state.name, state.abb)

county_sf <- county_tidy %>%
    st_intersection(st_union(spData::us_states)) %>%
    left_join(county_lc, by = "county_id") 

state_tidy <- county_sf %>%
    st_drop_geometry() %>%
    group_by(state) %>%
    summarise(across(c(county_area, solar_area, Water:Wetlands), \(x) sum(x, na.rm = TRUE)))

state_sf <- spData::us_states %>%
    left_join(state_key, by = c("NAME" = "state.name")) %>%
    left_join(state_tidy, by = c("state.abb" = "state"))

### Create plots

p1 <- county_sf %>%
    mutate(
        solar_area = replace_na(solar_area, -1),
        cc_bins = cut(
            solar_area/county_area*1e6, # sq m -> sq km 
            breaks = c(-Inf,0,10^(0:4)), 
            labels = c("No facilities in USPVDB","0 to 1", "1 to 10", "10 to 100", "100 to 1,000", "1,000 to 10,000")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = cc_bins), color = alpha("black", .2)) +
    scale_fill_viridis_d(option = "mako", begin = .3, direction = -1) +
    theme_minimal() +
    labs(
        fill = "Square meters of solar facility footprint\nper square kilometer of land area" 
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("results/county_area_map.png", p1, width = 11, height = 7)

p2 <- state_sf %>%
    mutate(
        solar_area = replace_na(solar_area, -1),
        county_area = replace_na(county_area, 1),
        area_bins = cut(
            solar_area/county_area*1e6,
            breaks = c(-Inf,0,10^(0:4)), 
            labels = c("No facilities in USPVDB","0 to 1", "1 to 10", "10 to 100", "100 to 1,000", "1,000 to 10,000")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = area_bins), color = alpha("black", .2)) +
    scale_fill_viridis_d(option = "mako", begin = .475, direction = -1) +
    theme_minimal() +
    labs(
        fill = "Square meters of solar facility footprint\nper square kilometer of land area"
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("results/state_area_map.png", p2, width = 11, height = 7)
    

