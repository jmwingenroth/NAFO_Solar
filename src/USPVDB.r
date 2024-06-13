library(tidyverse)
library(sf)
library(spData)
library(cowplot)

sf_use_s2(FALSE)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    mutate(acreage = p_area/4046.8564224) # Convert units

state_key <- tibble(state.name, state.abb)

state_pop <- read_csv("data/NST-EST2023-ALLDATA.csv") %>%
    select(-REGION)

uspvdb %>%
    as.data.frame() %>%
    group_by(p_type) %>%
    summarise(ac = sum(p_cap_ac), dc = sum(p_cap_dc), area = sum(p_area)) %>%
    transmute(p_type, ac/sum(ac), dc/sum(dc), area/sum(area))
    # Almost 99% of sites are greenfield, i.e.,
    # "wildland, urbanized, cultivated, or reclaimed".
    # I also determined (using `p_state`) that Hawai'i is around 0.5% of total.

p1a <- uspvdb %>%
    st_cast("POLYGON") %>%
    st_centroid() %>%
    filter(p_state != "HI") %>%
    ggplot() +
    geom_sf(data = spData::us_states, fill = "white", color = "#04273c", linewidth = .4) +
    geom_sf(data = filter(spData::us_states, NAME %in% c("North Dakota", "West Virginia")), fill = "#e3e0df", color = "#04273c", linewidth = .4) +
    geom_sf(aes(size = acreage), color = "#50b161", pch = 1, alpha = .8) +
    theme_void() +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(
        size = "Individual\nfacility\nacreage"
    ) +
    scale_size_continuous(labels = function(x) paste0(x, " acres"))

p1b <- uspvdb %>%
    as.data.frame() %>%
    group_by(p_state) %>%
    summarise(acreage = sum(acreage), p_cap_ac = sum(p_cap_ac)) %>%
    left_join(state_key, by = c("p_state" = "state.abb")) %>%
    left_join(spData::us_states, by = c("state.name" = "NAME")) %>%
    left_join(state_pop, by = c("state.name" = "NAME")) %>% 
    ggplot() +
    geom_sf(data = spData::us_states, fill = "#e3e0df", color = "#04273c", linewidth = .4) +
    geom_sf(aes(fill = acreage/POPESTIMATE2023*1e3, geometry = geometry), color = "#04273c", linewidth = .4) +
    theme_void() +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(
        fill = "Acreage\nper\nthousand\npeople", 
        caption = "Year: 2023\nData sources: USPVDB (gray states have no listed facilities); US Census Bureau"
    ) +
    scale_fill_gradient(
        low = "white", 
        high = "#88c4f4", 
        lim = c(0, NA),
        labels = function(x) paste0(x, " acres"),
        guide = guide_colorbar(frame.colour = "#04273c", ticks.colour = "#04273c")
    )

p1 <- plot_grid(
    p1a, NULL, p1b,
    ncol = 1,
    align = "hv",
    axis = "tlbr", 
    rel_heights = c(1,0,1),
    labels = c("a", NULL, "b"), label_y = 1
    ) + 
    theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("results/plot_1.svg", p1)
ggsave("results/plot_1.png", p1)
