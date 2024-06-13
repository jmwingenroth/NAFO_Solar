library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(FedData)
library(cowplot)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson")

NC_sites_rand <- uspvdb %>%
    filter(p_state == "NC", p_year %in% 2013:2019) %>%
    sample_n(9)

nlcd_rasters_2021 <- list()
nlcd_rasters_2011 <- list()

plot_list_2021 <- list()
plot_list_2011 <- list()

for (i in 1:nrow(NC_sites_rand)) {

    nlcd_rasters_2021[[i]] <- get_nlcd(
        st_buffer(NC_sites_rand[i,], dist = 200), 
        label = paste0("temp_", i), 
        year = 2021, 
        force.redo = TRUE
    )

    plot_list_2021[[i]] <- ggplot() +
        geom_spatraster(data = nlcd_rasters_2021[[i]], show.legend = FALSE) +
        geom_sf(data = NC_sites_rand[i,], color = "purple", linewidth = .5, fill = NA) +
        labs(title = NC_sites_rand$p_year[i]) +
        theme_void()

    nlcd_rasters_2011[[i]] <- get_nlcd(
        st_buffer(NC_sites_rand[i,], dist = 200), 
        label = paste0("temp_", i), 
        year = 2011, 
        force.redo = TRUE
    )

    plot_list_2011[[i]] <- ggplot() +
        geom_spatraster(data = nlcd_rasters_2011[[i]], show.legend = FALSE) +
        geom_sf(data = NC_sites_rand[i,], color = "purple", linewidth = .5, fill = NA) +
        labs(title = NC_sites_rand$p_year[i]) +
        theme_void()
}

p_nlcd_a <- plot_grid(plotlist = plot_list_2021, labels = c("2021 NLCD:", rep(NULL,5)), label_size = 17, hjust = 0) +
    theme(plot.background = element_rect(fill = "white", color = NA))

p_nlcd_b <- plot_grid(plotlist = plot_list_2011, labels = c("2011 NLCD:", rep(NULL,5)), label_size = 17, hjust = 0) +
    theme(plot.background = element_rect(fill = "white", color = NA))

p_nlcd <- plot_grid(p_nlcd_b, NULL, p_nlcd_a, rel_widths = c(.4, .2, .4), ncol = 3) +
    theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("results/nlcd.png", p_nlcd, width = 9, height = 7)
