library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(stars)
library(FedData)
library(cowplot)

start_time <- Sys.time()

nlcd_years <- c(2001, 2004, 2006, 2008, 2011, 2016, 2019, 2021) %>%
    sort() # Code below depends on ascending order

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > min(nlcd_years) + 5) %>% # 5-year buffer for construction time, only drops two sites
    filter(!p_state %in% c("HI", "AK")) %>% # NLCD only covers CONUS
    mutate(nlcd_year = nlcd_years[findInterval(p_year - 5, nlcd_years)]) %>%
    st_transform(5070)

nlcd_rasters_before <- list()
# nlcd_rasters_2001 <- list()
# nlcd_rasters_2011 <- list()
# nlcd_rasters_2021 <- list()
nlcd_rasters_cropped <- list()
uspvdb_land_cover <- list()

for (i in 1:nrow(uspvdb)) {

    print(paste0("Downloading NLCD data: beginning rep ", i, " of ", nrow(uspvdb)))

    nlcd_rasters_before[[i]] <- try(
        get_nlcd(
            st_buffer(uspvdb[i,], dist = 90), 
            label = paste0("temp_b4_", i), 
            year = uspvdb$nlcd_year[i], 
            force.redo = TRUE
        )
    )

    # nlcd_rasters_2001[[i]] <- get_nlcd(
    #     uspvdb[i,], 
    #     label = paste0("temp_01_", i), 
    #     year = 2001, 
    #     force.redo = TRUE
    # )

    # nlcd_rasters_2011[[i]] <- get_nlcd(
    #     uspvdb[i,], 
    #     label = paste0("temp_11_", i), 
    #     year = 2011, 
    #     force.redo = TRUE
    # )

    # nlcd_rasters_2021[[i]] <- get_nlcd(
    #     uspvdb[i,], 
    #     label = paste0("temp_21_", i), 
    #     year = 2021, 
    #     force.redo = TRUE
    # )

}

data_pull_successes <- lapply(nlcd_rasters_before, function(x) {
        typeof(x) == "S4"
    }) %>%
    unlist() %>%
    sum()

if(data_pull_successes != nrow(uspvdb)) warning("NLCD queries failed for some USPVDB polygons. Check raster list for NULL values.")

for (i in 1:nrow(uspvdb)) {

    print(paste0("Analyzing NLCD data: beginning rep ", i, " of ", nrow(uspvdb)))

    nlcd_rasters_cropped[[i]] <- nlcd_rasters_before[[i]] %>%
        mask(
            resample(
                rast(st_rasterize(uspvdb[i,])), 
                nlcd_rasters_before[[i]]
            )
        )

    uspvdb_land_cover[[i]] <- nlcd_rasters_cropped[[i]] %>%
        freq() %>%
        filter(layer == 1)

}

end_time <- Sys.time()
print(end_time - start_time)

# p1 <- ggplot() +
#     geom_spatraster(data = nlcd_rasters_cropped[[i]], show.legend = FALSE) +
#     geom_sf(data = uspvdb[i,], color = "purple", fill = NA, linewidth = 1)

# p2 <- ggplot() +
#     geom_spatraster(data = nlcd_rasters_2021[[i]], show.legend = FALSE) +
#     geom_sf(data = uspvdb[i,], color = "purple", fill = NA, linewidth = 1)

# plot_grid(p1, p2)
