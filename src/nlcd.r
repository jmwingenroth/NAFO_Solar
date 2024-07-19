##### Create maps based on NLCD and USPVDB datasets #####

### Load packages

library(tidyverse)
library(sf)
library(stars)
library(tigris)
library(spData)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

### Load data

nlcd_file <- list.files("L:/Project-SCC/NLCD_GIS_2001/", pattern = "img", full.names = TRUE)
nlcd_rast <- read_stars(nlcd_file)

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson") %>%
    filter(p_year > 2001) %>% # Drop one site built before NLCD data was collected
    filter(!p_state %in% c("HI", "AK")) %>% # Drop non-contiguous states
    st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # Match NLCD CRS

if (st_crs(nlcd_rast) != st_crs(uspvdb)) stop("CRS mismatch")

### Clip NLCD with USPVDB polygons

uspv_stars <- list()
n <- nrow(uspvdb)

for (i in 1:n) {

    if (i%%10 == 0) print(paste0("Extracted ",i," of ",n))

    uspv_stars[[i]] <- uspvdb[i,] %>%
        st_crop(x = nlcd_rast) %>%
        st_as_stars()

    if (i == n) print("Finished extracting NLCD values at USPVDB sites")

}

### Aggregate NLCD data to counties

uspv_lc <- uspv_stars %>%
    lapply(as_tibble) %>%
    lapply(rename, land_cover = 3) %>%
    lapply(group_by, land_cover) %>%
    lapply(tally) %>%
    lapply(filter, !is.na(land_cover)) %>%
    lapply(pivot_wider, names_from = land_cover, values_from = n) %>%
    bind_rows()

county_year_lc <- uspvdb %>%
    st_drop_geometry() %>%
    select(p_county, p_state, p_year, p_area) %>%
    bind_cols(uspv_lc) %>% # Order is conserved when clipping
    mutate(county_id = if_else(p_state == "CT", "CT combined", paste(p_state, p_county))) %>%
    group_by(p_state, county_id, p_year) %>%
    summarise(
        across(`Hay/Pasture`:`Open Water`, \(x) sum(x, na.rm = TRUE)*30^2), # Convert to m^2
        solar_area = sum(p_area)
    ) %>% 
    ungroup() %>%
    transmute(
        p_state,
        county_id,
        p_year,
        solar_area,
        Water = `Open Water`,
        Developed = rowSums(across(contains("Developed"))),
        Forest = rowSums(across(contains("Forest"))),
        Barren = rowSums(across(contains("Barren"))),
        Shrubland = rowSums(across(contains("Scrub"))),
        Herbaceous,
        Agricultural = `Hay/Pasture` + `Cultivated Crops`,
        Wetlands = rowSums(across(contains("Wetlands")))
    )

county_lc <- county_year_lc %>%
    group_by(p_state, county_id) %>%
    summarise(across(solar_area:Wetlands, sum))

### Add in geographic data

county_raw <- tigris::counties()

county_tidy <-  county_raw %>%
    left_join(tigris::fips_codes, by = c("STATEFP" = "state_code", "COUNTYFP" = "county_code")) %>%
    mutate(county_id = if_else(state == "CT", "CT combined", paste(state, NAME))) %>%
    mutate(county_id = if_else(str_detect(county_id, "^NM.*Ana$"), "NM Dona Ana", county_id)) %>%
    group_by(state, county_id) %>%
    summarise(county_area = sum(ALAND))

state_key <- tibble(state.name, state.abb) %>%
    bind_rows(tibble(state.name = "District of Columbia", state.abb = "DC"))

county_sf <- county_tidy %>%
    st_intersection(st_union(spData::us_states)) %>%
    left_join(county_lc, by = "county_id") %>%
    st_transform(st_crs("+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")) %>%
    mutate(solar_density = (solar_area/sq_m_per_acre)/(county_area/sq_m_per_sq_mi)) # same units -> acres per sq. mi. 

### Create plots

# Solar area / county area

p1 <- county_sf %>%
    st_simplify(dTolerance = 1000) %>%
    mutate(
        solar_density = replace_na(solar_density, -1),
        cc_bins = cut(
            solar_density,
            breaks = c(-Inf,0,.01,.1,1,5.4), 
            labels = c("No facilities","0.001 to 0.01", "0.01 to 0.1", "0.1 to 1", "1 to 5.4")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = cc_bins, color = cc_bins)) +
    geom_sf(data = spData::us_states, fill = NA, color = "black", linewidth = .3) +
    scale_fill_viridis_d(
        option = "mako", 
        begin = .3, 
        direction = -1,
        aesthetics = c("fill", "color")
    ) +
    theme_minimal() +
    labs(
        fill =  "Acres per square mile:", 
        color = "Acres per square mile:" 
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), axis.text = element_blank())

ggsave("results/county_area_map.svg", p1, width = 7, height = 4)

# Forest area / solar area

p3 <- county_sf %>%
    mutate(
        forest_frac = replace_na(Forest/solar_area, -1),
        ff_bins = cut(
            forest_frac,
            breaks = c(-Inf, 0, .25, .5, .75, 1.1), # At least one county had 100% forest
            right = FALSE,
            labels = c("No facilities in USPVDB", "0% to 25%", "25% to 50%", "50% to 75%", "75% to 100%")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = ff_bins), color = alpha("black", .2)) +
    scale_fill_viridis_d(option = "rocket", begin = .3, direction = -1) +
    theme_minimal() +
    labs(
        fill = "Percentage of solar facility footprint\noccupying previously forested land"
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("results/county_forest_loss_map.png", p3, width = 11, height = 7)

# Land cover category most often found within solar facility footprint

p5 <- county_sf %>% 
    pivot_longer(Water:Wetlands) %>% 
    group_by(county_id) %>%
    filter(value == max(value)) %>% 
    filter(name == first(name)) %>% # Resolve 2 ties out of 832 counties
    mutate(name = factor(
        name,
        levels = c(
            "Developed",
            "Herbaceous",
            "Shrubland",
            "Agricultural",
            "Forest"
        )
    )) %>%
    filter(!is.na(name)) %>%
    ggplot() +
    geom_sf(data = county_sf, fill = "#e6e9ec", color = alpha("black", .2)) +
    geom_sf(aes(fill = name), color = alpha("black", .2)) +
    scale_fill_manual(values = c(
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "#62b971"
        ),
        guide = guide_legend(reverse = TRUE)
    ) +
    theme_minimal() +
    labs(
        fill = "Land cover category with greatest\nland area converted to solar facilties"
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("results/county_category_map.png", p5, width = 11, height = 7)

# Land cover category by year

region_percent_lc <- county_year_lc %>%
    filter(p_year %in% 2006:2021) %>%
    mutate(Other = Water + Wetlands + Barren) %>%
    select(-Water, -Wetlands, -Barren) %>%
    pivot_longer(Developed:Other) %>%
    left_join(st_drop_geometry(state_sf), by = c("p_state" = "state.abb")) %>%
    group_by(REGION, p_year, name) %>%
    summarise(value = sum(value)) %>%
    group_by(REGION, p_year) %>%
    mutate(percent_value = value/sum(value)) %>%
    ungroup() %>%
    mutate(name = factor(
        name,
        levels = c(
            "Other",
            "Developed",
            "Herbaceous",
            "Shrubland",
            "Agricultural",
            "Forest"
        )
    ))

p7 <- region_percent_lc %>%
    mutate(REGION = if_else(REGION == "Norteast", "Northeast", REGION)) %>%
    ggplot(aes(x = p_year, y = percent_value, fill = name)) +
    geom_area(alpha = .8, color = alpha("black", .2)) +
    facet_wrap(~REGION) +
    scale_fill_manual(values = c(
        "#e6e9ec",
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "#62b971"
        ),
        guide = guide_legend(reverse = TRUE)
    ) +
    scale_x_continuous(limits = c(2010, 2021), expand = c(0,0), breaks = 2010:2021) +
    scale_y_continuous(limits = c(0, 1), expand = c(0,0), labels = scales::percent) +
    theme_bw() +
    theme(
        panel.spacing = unit(.5, "inch"), 
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank()
    ) +
    labs(y = "Fraction of new solar\nfacilities' footprint areas", fill = "Land cover category", x = "Year")

ggsave("results/state_category_area_chart.png", p7, width = 7, height = 7)

p8 <- county_year_lc %>%
    filter(p_year >= 2006) %>%
    group_by(p_year) %>%
    summarise(forest = sum(Forest), total = sum(solar_area)) %>%
    ggplot(aes(x = p_year, y = forest/sq_m_per_acre)) +
    geom_line(color = "#62b971") +
    geom_point(color = "#62b971") +
    scale_x_continuous(limits = c(2005, 2022), expand = c(0,0), breaks = 2006:2021) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Year", y = "", title = "Acres of forest converted to solar facilities")

ggsave("results/forest_historical.png", p8, width = 7, height = 4)
