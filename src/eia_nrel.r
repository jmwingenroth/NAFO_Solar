##### Make projection figure using DOE/NREL SFS and EIA AEO datasets #####

library(tidyverse)
library(sf)
library(broom)
library(cowplot)
library(ggtext)

### Load data

aeo_raw <- read_csv("data/Renewable_Energy_All_Sectors_Net_Summer_Capacity_Solar.csv", skip = 4) 

nrel_raw <- read_csv("data/Solar_Futures_Study_DOE_NREL.csv")

### Tidy data

proj_tidy <- aeo_raw %>%
    rename(aeo_ref = 2, aeo_high = 3, aeo_low = 4) %>%
    mutate(across(aeo_ref:aeo_low, \(x) x/watt_dc_per_ac)) %>%     # Convert from DC to AC
    full_join(nrel_raw) %>%
    rename(nrel_ref = 5, nrel_decarb = 6, nrel_decarb_e = 7) %>%   # Convert to acres
    mutate(across(aeo_ref:nrel_decarb_e, \(x) x*1e3*acre_per_MWac)) %>%
    arrange(Year)

### Create projection figure

p1 <- proj_tidy %>%
    pivot_longer(aeo_ref:nrel_decarb_e) %>%
    bind_rows(tibble(
        Year = c(2020,2020),
        name = c("nrel_dummy", "aeo_dummy"),
        value = c(5e6, 5e6)
    )) %>%
    mutate(name = factor(name, 
        levels = c(
            "nrel_dummy",
            "nrel_ref",
            "nrel_decarb",
            "nrel_decarb_e",

            "aeo_dummy",
            "aeo_ref",
            "aeo_high",
            "aeo_low"
        ),
        labels = c(
            "**SFS (2021) Scenarios:**",
            "Reference Case     ",
            "Decarbonization     ",
            "Decarbonization plus Electrification     ",

            "**AEO (2023) Scenarios:**",
            "Reference Case    ",
            "High Growth and Low Solar Price     ",
            "Low Growth and High Solar Price     "
        )
    )) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = Year, y = value, color = name, linetype = name)) +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = c("NA", rep("#88c4f4",3), "NA", rep("#ff6663",3))) +
    scale_linetype_manual(
        values = c(
            "solid",
            "solid",
            "42",
            "12",
            "solid",
            "solid",
            "42",
            "12"
        )
    ) +
    scale_y_continuous(
        limits = c(0, 1.25e7), 
        labels = c(0, paste0(c(2.5, 5, 7.5, 10, 12.5), "M")),
        breaks = seq(0, 1.25e7, by = 2.5e6),
        expand = c(0,0)
    ) +
    labs(x = "", y = "", color = "", linetype = "") +
    theme(
        legend.position = "bottom", 
        legend.spacing.x = unit(.5, "in"), 
        legend.text = element_markdown()
    ) +
    guides(color = guide_legend(nrow = 4), linetype = guide_legend(nrow = 4))

ggsave("results/eia_nrel_projections.svg", p1, width = 7, height = 4)
