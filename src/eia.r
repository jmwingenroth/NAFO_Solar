##### Make projection figures using USPVDB and EIA AEO datasets #####

library(tidyverse)
library(sf)
library(broom)
library(cowplot)

### Load data

uspvdb_raw <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    st_drop_geometry() %>% # Don't need spatial polygons
    filter(p_year > 2000) # Dropping one outlier from 1986

aeo_raw <- read_csv("data/EIA_data.csv", skip = 4) 

### Tidy data

uspvdb_annual <- uspvdb_raw %>%
    mutate(p_cap_dc = p_cap_dc*1e6) %>% # MW -> watts
    group_by(p_year) %>%
    summarise(across(c(p_cap_dc, p_area), sum)) %>%
    mutate(
        across(p_cap_dc:p_area, cumsum, .names = "{.col}_cumul"),
        dataset = "Historical (USPVDB)"
    )

aeo_tidy <- aeo_raw %>%
    transmute(
        p_year = Year,
        p_cap_dc_cumul = `Renewable Energy: All Sectors: Net Summer Capacity: Solar GW`*1e9, # GW -> watts
        dataset = "Projected (EIA AEO 2023, reference case)"
    ) %>%
    filter(p_year > 2021) # Drop missing value

all_data <- bind_rows(uspvdb_annual, aeo_tidy) %>%
    arrange(p_year) %>%
    mutate(
        dataset = factor(
            dataset,
            levels = c("Historical (USPVDB)", "Projected (EIA AEO 2023, reference case)")
        ),
        p_cap_dc_cumul = round(p_cap_dc_cumul) # Fix weird bug with AEO data
    )

### Analyze data

# Weighted least-squares regression to account for improvements in area efficiency

eff_coeff <- lm(I(p_cap_dc/p_area)*1e6 ~ p_year, data = filter(uspvdb_raw, p_year > 2000), weights = p_area) %>%
    tidy() %>%
    .$estimate

# Use regression coefficient to estimate future total area from cumulative capacity

all_data_lm <- all_data %>%
    mutate(
        p_cap_dc = if_else(
            is.na(p_cap_dc),
            p_cap_dc_cumul - lag(p_cap_dc_cumul),
            p_cap_dc
        ),
        p_area = if_else(
            is.na(p_area),
            p_cap_dc/(eff_coeff[1] + eff_coeff[2]*p_year),
            p_area
        ),
        p_area_cumul = cumsum(p_area)
    )

### Create figures

# Diagnostic plots

dir.create("misc")

proj_1 <- uspvdb_annual %>%
    pivot_longer(p_cap_dc:p_area_cumul) %>%
    ggplot(aes(x = p_year, y = value, color = name)) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~name, scales = "free_y") +
    theme_bw()

ggsave("misc/line_graph.png", proj_1)

# Solar capacity figure

proj_2 <- all_data %>%
    filter(p_year %in% 2006:2030) %>%
    ggplot(aes(x = p_year, y = p_cap_dc_cumul/1e9, color = dataset)) +
    geom_line(show.legend = FALSE) +
    geom_point(show.legend = FALSE) +
    theme_bw() +
    scale_color_manual(values = c("#04273c", "#88c4f4")) +
    labs(
        x = "Year", 
        y = "",
        title = "Aggregate national solar power capacity (in gigawatts)",
        color = ""
    ) +
    scale_x_continuous(limits = c(2004, 2031), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 500)) +
    theme(
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()
    )

# Land area figure

proj_3 <- all_data_lm %>%
    filter(p_year %in% 2006:2030) %>%
    ggplot(aes(x = p_year, y = p_area_cumul/sq_m_per_acre, color = dataset)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_color_manual(values = c("#04273c", "#88c4f4")) +
    labs(
        x = "Year", 
        y = "",
        title = "Total land area of utility-scale solar installations (in acres)",
        color = ""
    ) +
    scale_x_continuous(limits = c(2004, 2031), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 2e6), labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(
        legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()
    )

proj_fig <- plot_grid(
    proj_2, NULL, proj_3, 
    ncol = 1, 
    align = "hv", 
    labels = c("a)", "", "b)"), 
    rel_heights = c(1, -.1, 1)
)

ggsave("results/projections.svg", proj_fig, width = 7, height = 7)

