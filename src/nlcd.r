### NOTE:
### Takes a while to run
###

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(stars)
library(FedData)
library(cowplot)
library(tigris)
library(spData)

sf_use_s2(FALSE)

### Parameters

nlcd_years <- c(2001, 2004, 2006, 2008, 2011, 2016, 2019, 2021) %>%
    sort() # Code below depends on ascending order

abbrev_key <- tibble(name = state.name, state.abb)

region_key <- tibble(name = spData::us_states$NAME, region = spData::us_states$REGION) %>%
    left_join(abbrev_key) %>%
    mutate(state.abb = if_else(name == "District of Columbia", "DC", state.abb)) %>%
    arrange(name)

### USPVDB data

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > min(nlcd_years) + 5) %>% # 5-year buffer for construction time, 2 sites dropped
    filter(!p_state %in% c("HI", "AK")) %>% # NLCD only covers CONUS
    mutate(nlcd_year = nlcd_years[findInterval(p_year - 5, nlcd_years)]) %>%
    st_transform(5070)

### Empty lists

nlcd_rasters_before <- list()
nlcd_rasters_cropped <- list()
uspvdb_land_cover <- list()

### Download NLCD data covering USPVDB polygons

start_time <- Sys.time()

for (i in 1:nrow(uspvdb)) {

    print(paste0("Downloading NLCD data: beginning rep ", i, " of ", nrow(uspvdb)))
    
    nlcd_rasters_before[[i]] <- try(
        get_nlcd(
            st_buffer(uspvdb[i,], dist = 90), 
            label = paste0("temp_b4_", i), 
            year = uspvdb$nlcd_year[i], 
            force.redo = TRUE
        )
    )

}

data_pull_successes <- lapply(nlcd_rasters_before, function(x) {
        typeof(x) == "S4"
    }) %>%
    unlist() %>%
    sum()

if(data_pull_successes != nrow(uspvdb)) {
    warning("NLCD queries failed for some USPVDB polygons. Check raster list for NULL values.")
}

### Extract NLCD data at USPVDB polygons and tally pixels by prior land cover category

for (i in 1:nrow(uspvdb)) {

    print(paste0("Analyzing NLCD data: beginning rep ", i, " of ", nrow(uspvdb)))

    nlcd_rasters_cropped[[i]] <- nlcd_rasters_before[[i]] %>%
        mask(
            resample(
                rast(st_rasterize(uspvdb[i,])), 
                nlcd_rasters_before[[i]]
            )
        )

    uspvdb_land_cover[[i]] <- nlcd_rasters_cropped[[i]] %>%
        freq() %>%
        filter(layer == 1)

}

end_time <- Sys.time()
print(end_time - start_time)

### Aggregate prior land cover data to units of area, county level, and NLCD class (broad category)

for (i in 1:nrow(uspvdb)) {
    uspvdb_land_cover[[i]]$state <- uspvdb$p_state[i]
    uspvdb_land_cover[[i]]$county <- uspvdb$p_county[i]
}

uspvdb_land_cover_agg <- uspvdb_land_cover %>%
    lapply(as_tibble) %>%
    bind_rows() %>%
    mutate(
        broad_category = fct_collapse(
            factor(value),
            Water = c(
                "Open Water", 
                "Perennial Ice/Snow"
            ),
            Developed = c(
                "Developed, Open Space",
                "Developed, Low Intensity",
                "Developed, Medium Intensity",
                "Developed High Intensity"
            ),
            Barren = "Barren Land (Rock/Sand/Clay)",
            Forest = c(
                "Deciduous Forest",
                "Evergreen Forest",
                "Mixed Forest"
            ),
            Shrubland = c(
                "Dwarf Scrub",
                "Shrub/Scrub"
            ),
            Herbaceous = c(
                "Grassland/Herbaceous",
                "Sedge/Herbaceous",
                "Lichens",
                "Moss"
            ),
            Agricultural = c(
                "Pasture/Hay",
                "Cultivated Crops"
            ),
            Wetlands = c(
                "Woody Wetlands",
                "Emergent Herbaceous Wetlands"
            )
        )
    ) %>%
    mutate(group = if_else(state == "CT", "CT Combined", paste(state, county))) %>%
    group_by(group, broad_category) %>%
    summarise(area = sum(count)*30^2) %>% # 30m x 30m pixels
    pivot_wider(names_from = broad_category, values_from = area) %>%
    ungroup() %>%
    mutate(across(everything(), function(x) replace_na(x, 0))) %>%
    mutate(
        total_area = rowSums(across(where(is.numeric)), na.rm = TRUE),
        forest_frac = Forest/total_area
    )

### Create maps

county_map <- tigris::counties()

connecticut <- spData::us_states %>% filter(NAME == "Connecticut") %>%
    transmute(group = "CT Combined")

county_forest_loss <- left_join(county_map, fips_codes, by = c("STATEFP" = "state_code", "COUNTYFP" = "county_code")) %>%
    filter(state %in% region_key$state.abb) %>%
    filter(state != "CT") %>%
    transmute(group = paste(state, NAME)) %>%
    mutate(group = if_else(str_detect(group, "^NM.*Ana$"), "NM Dona Ana", group)) %>% # Get rid of tilde :/
    bind_rows(connecticut) %>%
    left_join(uspvdb_land_cover_agg) %>%
    select(group, forest_frac, geometry)

forest_loss_map <- county_forest_loss %>%
    mutate(
        forest_frac = replace_na(forest_frac, -1),
        ff_bins = cut(
            forest_frac,
            breaks = c(-Inf, 0, .25, .5, .75, 1.1), # At least one county had 100% forest
            right = FALSE,
            labels = c("No facilities in USPVDB", "0% to 25%", "25% to 50%", "50% to 75%", "75% to 100%")
        )
    ) %>%
    st_intersection(st_union(spData::us_states)) %>%
    ggplot() +
    geom_sf(aes(fill = ff_bins), color = alpha("black", .2)) +
    scale_fill_viridis_d(option = "rocket", begin = .3, direction = -1) +
    theme_minimal() +
    labs(
        fill = "Percentage of solar facility footprint\noccupying previously forested land", 
        caption = "Note: Connecticut counties temporarily aggregated due to data-joining issues"
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("results/forest_loss_map.png", forest_loss_map, width = 11, height = 7)

spData::us_states %>%
    group_by(REGION) %>%
    summarise(geometry = st_union(geometry)) %>%
    ggplot() +
    geom_sf()
