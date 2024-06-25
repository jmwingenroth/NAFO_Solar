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
    left_join(abbrev_key)

uspvdb_new <- uspvdb %>%
    left_join(region_key, by = c("p_state" = "state.abb")) %>%
    filter(!p_state %in% c("AK", "HI")) %>% # CONUS
    mutate(
        region = if_else(p_state == "DC", "Norteast", region),
        cap_per_acre = p_cap_dc/acreage*1000 # (kW/acre)
    )

region_map <- uspvdb_new %>%
    st_centroid() %>%
    ggplot() +
    geom_sf(data = spData::us_states, show.legend = FALSE) +
    geom_sf(aes(size = acreage, fill = region, color = region))

region_map

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

ggsave("results/hists.png", hists)

