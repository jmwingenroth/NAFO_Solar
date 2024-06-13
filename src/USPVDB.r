library(tidyverse)
library(sf)
library(spData)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson")

uspvdb %>%
    as.data.frame() %>%
    group_by(p_type) %>%
    summarise(ac = sum(p_cap_ac), dc = sum(p_cap_dc), area = sum(p_area)) %>%
    transmute(p_type, ac/sum(ac), dc/sum(dc), area/sum(area))
    # Almost 99% of sites are greenfield, i.e.,
    # "wildland, urbanized, cultivated, or reclaimed"
