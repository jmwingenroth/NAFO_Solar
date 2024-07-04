library(tidyverse)
library(sf)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson")

line_graph <- uspvdb %>%
    st_drop_geometry() %>%
    group_by(p_year) %>%
    # Units: annual area = km^2; area_efficiency = watts per m^2
    summarise(annual_area = sum(p_area/1e6), area_efficiency = mean(p_cap_dc/p_area*1e6)) %>%
    mutate(cumul_area = cumsum(annual_area)) %>%
    pivot_longer(annual_area:cumul_area) %>%
    ggplot(aes(x = p_year, y = value, color = name)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    scale_x_continuous(limits = c(2000,NA)) +
    theme_bw()

ggsave("results/line_graph.png", line_graph)
