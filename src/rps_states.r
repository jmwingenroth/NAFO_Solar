library(tidyverse)
library(sf)
library(spData)

crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

rps_states <- read_csv("data/rps_states_2021.csv")

hawaii_map <- spData::hawaii %>%
    ggplot() +
    geom_sf(data = spData::hawaii, fill = "#50b161") +
    theme_void()

alaska_map <- spData::alaska %>%
    ggplot() +
    geom_sf(data = spData::alaska, fill = "white") +
    theme_void()

rps_sf <- rps_states %>%
    mutate(label = factor(
        fifty_or_greater, 
        levels = c(TRUE, FALSE),
        labels = c(
        "At least 50% renewables", 
        "Below 50% renewables"
        )
    )) %>%
    left_join(spData::us_states, by = c("State" = "NAME")) %>%
    st_as_sf()

p1 <- rps_sf %>%
    st_transform(crs) %>%
    ggplot(aes(fill = label)) +
    geom_sf(data = spData::us_states, fill = "white") +
    geom_sf() +
    annotation_custom(ggplotGrob(hawaii_map), xmin = -1.3e6, xmax = -7e5, ymin = -5e5, ymax = 1.2e6) +
    annotation_custom(ggplotGrob(alaska_map), xmin = -3e6, xmax = -1.5e6, ymin = -5e5, ymax = 1.8e6) +
    scale_fill_manual(values = c("#50b161", "#a8d8b0")) +
    labs(fill = "RPS targets:") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"), 
        panel.grid = element_blank(), 
        axis.text = element_blank()
    )

ggsave("results/rps_states.svg", p1, height = 4, width = 7)
