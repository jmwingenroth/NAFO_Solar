library(tidyverse)
library(sf)

### Load data

uspvdb_raw <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    st_drop_geometry() # Don't need spatial polygons

aeo_raw <- read_csv("data/EIA_data.csv", skip = 4) 

### Tidy data

uspvdb_annual <- uspvdb_raw %>%
    group_by(p_year) %>%
    summarise(across(c(p_cap_dc, p_area), sum)) %>%
    mutate(
        across(p_cap_dc:p_area, cumsum, .names = "{.col}_cumul"),
        dataset = "Historical (USPVDB)"
    )

aeo_tidy <- aeo_raw %>%
    transmute(
        p_year = Year,
        p_cap_dc_cumul = `Renewable Energy: All Sectors: Net Summer Capacity: Solar GW`*1000, # GW -> MW
        dataset = "EIA AEO projection (reference case)"
    ) %>%
    filter(p_year > 2021) # Drop missing value

all_data <- bind_rows(uspvdb_annual, aeo_tidy) %>%
    arrange(p_year) %>%
    mutate(dataset = factor(
        dataset,
        levels = c("Historical (USPVDB)", "EIA AEO projection (reference case)")
    ))

### Create figures

p1 <- uspvdb_annual %>%
    filter(p_year > 2000) %>% # Dropping one outlier from 1986
    pivot_longer(p_cap_dc:p_area_cumul) %>%
    ggplot(aes(x = p_year, y = value, color = name)) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~name, scales = "free_y") +
    theme_bw()

ggsave("results/line_graph.png", p1)

p2 <- all_data %>%
    ggplot(aes(x = p_year, y = p_cap_dc_cumul/1e3, color = dataset)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = c("#04273c", "#ebd367")) +
    labs(
        x = "Year", 
        y = "Aggregate national solar\npower capacity (GW)",
        color = ""
    ) +
    scale_x_continuous(limits = c(2000, NA)) +
    theme(legend.position = "bottom")

ggsave("results/capacity_projection.png", width = 7, height = 4)
