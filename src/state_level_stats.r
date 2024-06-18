library(tidyverse)
library(sf)

state_key <- tibble(state.name, state.abb) %>%
    bind_rows(tibble(state.name = "District of Columbia", state.abb = "DC"))

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    mutate(acreage = p_area/4046.8564224) # Convert units

uspvdb %>%
    st_drop_geometry() %>%
    group_by(p_state) %>%
    summarise(n = n(), acreage = sum(acreage), capacity = sum(p_cap_dc)) %>%
    left_join(state_key, by = c("p_state" = "state.abb")) %>%
    write_csv("results/state_level_stats.csv")
