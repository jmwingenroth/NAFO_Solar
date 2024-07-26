##### Create alluvial figure by state/region from SEIA and USPVDB data #####

library(tidyverse)
library(ggalluvial)
library(cowplot)

# Load data

seia_raw <- read_csv("data/SEIA_data.csv")
land_cover_raw <- read_csv("results/uspvdb_land_cover.csv")

state_key <- tibble(state.abb, state.name)

# Tidy data

seia_scaled <- seia_raw %>%
    transmute(
        States,
        across(c(MWdc_2024, MWdc_2029), \(x) x*scaling_to_utility)
    )
    
sum(seia_scaled$MWdc_2024)  # Close to value from https://www.seia.org/solar-industry-research-data
                            # including community solar

land_cover_tidy <- land_cover_raw %>%
    transmute(
        p_state,
        MWdc_2021 = p_cap_dc,
        forest_2021 = rowSums(across(contains("Forest")), na.rm = TRUE)
    ) %>%
    group_by(p_state) %>%
    summarise(across(everything(), sum))

all_data <- land_cover_tidy %>%
    left_join(state_key, by = c("p_state" = "state.abb")) %>%
    left_join(seia_scaled, by = c("state.name" = "States")) %>%
    mutate(
        forest_2024 = forest_2021/MWdc_2021*MWdc_2024,
        forest_2029 = forest_2021/MWdc_2021*MWdc_2029,
        state_id = if_else(forest_2021 > 5e6 | forest_2029 > 5e7, p_state, "Other")
    ) %>%
    filter(!is.na(state_id))

# Create plot

p1 <- all_data %>%
    mutate(
        p_state = fct_reorder(p_state, -forest_2021),
        state_id = fct_reorder(state_id, -forest_2021)
    ) %>%
    group_by(state_id) %>%
    pivot_longer(contains("forest")) %>%
    group_by(state_id, name) %>%
    summarise(value = sum(value)) %>%
    mutate(
        year = as.numeric(str_sub(name,-4)),
        state_label = if_else(year == 2029, state_id, "")
    ) %>%
    ggplot(aes(x = factor(year), y = value/sq_m_per_acre, fill = state_id)) +
    geom_col(width = .33, show.legend = FALSE) +
    geom_alluvium(aes(alluvium = state_id), curve_type = "linear", alpha = .3, show.legend = FALSE) +
    geom_text(aes(label = state_label), position = position_stack(vjust = .5), color = "white") +
    theme_bw() +
    scale_color_manual(
        values = c("#04273c","#88c4f4","#ff6663","#50b161","#765ea5","#f4a25f","#ebd367","#74645e"),
        aesthetics = c("fill", "color")
    ) +
    scale_y_continuous(limits = c(0, 120000), expand = c(0,0), minor_breaks = NULL) +
    scale_x_discrete(breaks = NULL) +
    labs(x = "", y = "")

ggsave("results/seia_projections.svg", p1, width = 7, height = 4)
