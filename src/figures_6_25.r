library(tidyverse)
library(sf)
library(spData)
library(cowplot)
library(tigris)
library(tidycensus)

sf_use_s2(FALSE)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    mutate(acreage = p_area/4046.8564224) # Convert units


abbrev_key <- tibble(name = state.name, state.abb)

region_key <- tibble(name = spData::us_states$NAME, region = spData::us_states$REGION) %>%
    left_join(abbrev_key) %>%
    mutate(state.abb = if_else(name == "District of Columbia", "DC", state.abb)) %>%
    arrange(name)

uspvdb_new <- uspvdb %>%
    left_join(region_key, by = c("p_state" = "state.abb")) %>%
    filter(!p_state %in% c("AK", "HI")) %>% # CONUS
    mutate(
        cap_per_acre = p_cap_dc/acreage*1000 # (kW/acre)
    )

region_map <- uspvdb_new %>%
    st_centroid() %>%
    ggplot() +
    geom_sf(data = spData::us_states, show.legend = FALSE) +
    geom_sf(aes(size = acreage, fill = region, color = region))

region_map

# Plot histograms of acres and capacity per acre by region

fig_1_data <- uspvdb_new %>%
    select(p_name, region, acreage, cap_per_acre) %>%
    pivot_longer(acreage:cap_per_acre) %>%
    mutate(name = factor(name, labels = c("Acres", "Direct current capacity (kW) per acre")))

hists <- fig_1_data %>%
    ggplot(aes(x = value, fill = region)) +
    geom_histogram(color = "black", alpha = .5) +
    scale_x_log10() +
    facet_grid(region~name, scales = "free_x", switch = "both") +
    theme_bw() +
    labs(x = "", y = "# of facilities", fill = "Region") +
    theme(
        strip.text.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(fill = "white", color = "white")
    )

ggsave("results/hists.png", hists, width = 11, height = 7)

# Plot map of capacity (DC) by county

county_area <- uspvdb_new %>%
    st_drop_geometry() %>%
    mutate(group = if_else(p_state == "CT", "CT Combined", paste(p_state, p_county))) %>%
    group_by(group) %>%
    summarise(county_area = sum(p_area))

county_map <- tigris::counties()

county_area_sf <- left_join(county_map, fips_codes, by = c("STATEFP" = "state_code", "COUNTYFP" = "county_code")) %>%
    mutate(group = if_else(state == "CT", "CT Combined", paste(state, NAME))) %>% # Combine CT counties
    mutate(group = if_else(str_detect(group, "^NM.*Ana$"), "NM Dona Ana", group)) %>% # Get rid of tilde :/
    group_by(state, group) %>%
    summarise(geometry = st_union(geometry), land_area = sum(ALAND)) %>%
    left_join(county_area)

county_area_map <- county_area_sf %>%
    mutate(
        county_area = replace_na(county_area, 0),
        cc_bins = cut(
            county_area/land_area*1e6, # sq m -> sq km 
            breaks = c(-1,10^(0:4)), 
            labels = c("0 to 1", "1 to 10", "10 to 100", "100 to 1,000", "1,000 to 10,000"))
    ) %>%
    filter(state %in% region_key$state.abb) %>%
    st_intersection(st_union(spData::us_states)) %>%
    ggplot() +
    geom_sf(aes(fill = cc_bins), color = alpha("black", .2)) +
    scale_fill_viridis_d(option = "mako", begin = .3, direction = -1) +
    theme_minimal() +
    labs(
        fill = "Square meters of solar facility footprint\nper square kilometer of land area", 
        caption = "Note: Connecticut counties temporarily aggregated due to data-joining issues"
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("./results/county_area_map.png", county_area_map, width = 11, height = 7)
