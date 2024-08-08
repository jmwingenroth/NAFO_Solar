##### Create land-cover related maps and figures based on NLCD and USPVDB datasets #####

### Load packages

library(tidyverse)
library(sf)
library(stars)
library(tigris)
library(spData)
library(cowplot)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

eastern_seaboard <- c(
    "FL", "GA", "SC", "NC", 
    "VA", "MD", "DC", "PA", 
    "DE", "NJ", "NY", "CT", 
    "RI", "MA", "NH", "ME"
)

### Load data

regions_data <- read_csv("data/StateFIPS.csv")

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

land_cover_output_data <- bind_cols(uspvdb, uspv_lc) %>% 
    st_drop_geometry() %>%
    mutate(across(`Hay/Pasture`:`Open Water`, \(x) x*30^2)) # Convert to m^2

write_csv(land_cover_output_data, "results/uspvdb_land_cover.csv")

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

state_key <- regions_data %>%
    left_join(spData::us_states, c("stname" = "NAME")) %>%
    st_as_sf() %>%
    st_transform(st_crs("+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"))

county_sf <- county_tidy %>%
    st_intersection(st_union(spData::us_states)) %>%
    left_join(county_lc, by = "county_id") %>%
    st_transform(st_crs("+proj=aea +lat_0=37.5 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")) %>%
    mutate(
        # same units -> acres per sq. mi. 
        solar_density = (solar_area/sq_m_per_acre)/(county_area/sq_m_per_sq_mi),
        forest_density = (Forest/sq_m_per_acre)/(county_area/sq_m_per_sq_mi)
    ) 

### Create plots

# Solar area / county area

p1 <- county_sf %>%
    st_simplify(dTolerance = 1000) %>%
    mutate(
        solar_density = replace_na(solar_density, -1),
        cc_bins = cut(
            solar_density,
            breaks = c(-Inf,0,.01,.1,1,5.4), 
            labels = c("0","0.001 to 0.01", "0.01 to 0.1", "0.1 to 1", "1 to 5.4")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = cc_bins, color = cc_bins)) +
    geom_sf(data = state_key, fill = NA, color = "black", linewidth = .3) +
    scale_fill_viridis_d(
        option = "mako", 
        begin = .3, 
        direction = -1,
        aesthetics = c("fill", "color")
    ) +
    theme_minimal() +
    labs(
        fill =  expression(Acres~per~mi^2), 
        color = expression(Acres~per~mi^2) 
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), axis.text = element_blank())

ggsave("results/county_area_map.svg", p1, width = 7, height = 4)

# Forest area / county area

p3 <- county_sf %>%
    filter(state %in% eastern_seaboard) %>%
    st_simplify(dTolerance = 1000) %>%
    mutate(
        forest_density = replace_na(forest_density, -1),
        ff_bins = cut(
            forest_density,
            breaks = c(-Inf,0,.01,.1,1,5.4), 
            labels = c("0","0.001 to 0.01", "0.01 to 0.1", "0.1 to 1", "1 to 3.3")
        )
    ) %>%
    ggplot() +
    geom_sf(aes(fill = ff_bins, color = ff_bins)) +
    geom_sf(
        data = filter(state_key, stAB %in% eastern_seaboard), 
        fill = NA, 
        color = "black", linewidth = .3) +
    scale_fill_viridis_d(
        option = "rocket", 
        begin = .3, 
        direction = -1,
        aesthetics = c("fill", "color")
    ) +
    theme_minimal() +
    labs(
        fill =  expression(Acres~per~mi^2), 
        color = expression(Acres~per~mi^2) 
    ) +
    theme(
        plot.background = element_rect(fill = "white", color = "white"), 
        panel.grid = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical"
    )

ggsave("results/county_forest_loss_map.svg", p3, width = 3.5, height = 7)

# Land cover category most often found within solar facility footprint

p5 <- county_sf %>% 
    st_simplify(dTolerance = 1000) %>%
    mutate(Other = Water + Wetlands + Barren) %>%
    select(-Water, -Wetlands, -Barren) %>%
    pivot_longer(Developed:Other) %>%
    group_by(county_id) %>%
    filter(value == max(value)) %>% 
    filter(name == first(name)) %>%
    mutate(name = factor(
        name,
        levels = rev(c(
            "Other",
            "Developed",
            "Herbaceous",
            "Shrubland",
            "Agricultural",
            "Forest"
        )),
        ordered = TRUE
    )) %>%
    arrange(desc(name)) %>%
    ggplot() +
    geom_sf(aes(fill = name, color = name)) +
    geom_sf(data = spData::us_states, fill = NA, color = "black", linewidth = .3) +
    scale_fill_manual(values = rev(c(
        "#fad1af",
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "#62b971"
        ))
    ) +
    scale_color_manual(values = rev(c(
        "#fad1af",
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "black"
        ))
    ) +
    theme_minimal() +
    labs(
        fill =  "",
        color = ""
    ) +
    theme(plot.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), axis.text = element_blank())

ggsave("results/county_category_map.svg", p5, width = 7, height = 4)

# Land cover category by year

region_percent_lc <- county_year_lc %>%
    filter(p_year %in% 2006:2021) %>%
    mutate(Other = Water + Wetlands + Barren) %>%
    select(-Water, -Wetlands, -Barren) %>%
    pivot_longer(Developed:Other) %>%
    left_join(state_key, by = c("p_state" = "stAB")) %>%
    group_by(stRegion, p_year, name) %>%
    summarise(value = sum(value)) %>%
    group_by(stRegion, p_year) %>%
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
    mutate(stRegion = factor(
        stRegion, 
        levels = c("Pacific", "Rockies", "Plains", "North", "South"),
        labels = c("1: Pacific", "2: Rockies", "3: Plains", "4: Northeast", "5: Southeast")
    )) %>%
    ggplot(aes(x = p_year, y = value/sq_m_per_acre, fill = name, color = name)) +
    geom_area(linewidth = .3) +
    facet_wrap(~stRegion) +
    scale_fill_manual(values = c(
        "#fad1af",
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "#62b971"
        ),
        guide = guide_legend(reverse = TRUE, ncol = 2)
    ) +
    scale_color_manual(values = c(
        "#fad1af",
        "#ffb3b1",
        "#c4e2fa",
        "#d6cfe4",
        "#f5e9b3",
        "black"
        ),
        guide = guide_legend(reverse = TRUE, ncol = 2)
    ) +
    scale_x_continuous(limits = c(2009, 2021), breaks = c(2010, 2015, 2020), expand = c(0,0), minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, 25000), expand = c(0,0), minor_breaks = NULL) +
    theme_bw() +
    theme(
        legend.position = "inside",
        legend.text = element_text(size = 8),
        legend.key.size = unit(.8, "lines"),
        legend.position.inside = c(5/6, .4),
        legend.background = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()
    ) +
    labs(y = "", fill = "", color = "", x = "")


regions_sf <- state_key %>%
    group_by(stRegion) %>%
    summarize(geometry = st_union(geometry))

labels_sf <- regions_sf %>%
    st_centroid() %>%
    mutate(label = factor(stRegion, levels = c("Pacific", "Rockies", "Plains", "North", "South"), labels = 1:5))

state_key_grob <- state_key %>%
    st_simplify(dTolerance = 1000) %>%
    ggplot() +
    geom_sf(fill = NA, color = "gray", linewidth = .1) +
    geom_sf(data = regions_sf, fill = NA, color = "black", linewidth = .5) +
    geom_sf_text(
        aes(label = label),
        data = labels_sf, 
        size = 4,
        nudge_x = c(-5.5e5,9e4,-4e4,1e5,0),
        nudge_y = c(0,4e5,8.8e5,5.7e5,0)
    ) +
    theme_void() +
    labs(x = "", y = "") +
    theme(plot.background = element_rect(fill = "white", color = "white"), panel.grid = element_blank(), axis.text = element_blank())

p7_combo <- ggdraw(p7) +
    draw_plot(state_key_grob, .7, .06, .3, .3)


uspv_lc %>%
    mutate_all(\(x) x*30^2) %>%
    mutate_all(replace_na, 0) %>%
    bind_cols(uspvdb) %>%
    transmute(
        p_name,
        p_state,
        xlong,
        ylat,
        p_year,
        p_area,
        p_cap_dc,
        p_cap_ac,
        Water = `Open Water`,
        Developed = rowSums(across(contains("Developed"))),
        Forest = rowSums(across(contains("Forest"))),
        Barren = rowSums(across(contains("Barren"))),
        Shrubland = rowSums(across(contains("Scrub"))),
        Herbaceous,
        Agricultural = `Hay/Pasture` + `Cultivated Crops`,
        Wetlands = rowSums(across(contains("Wetlands")))
    ) %>% st_drop_geometry() %>% write_csv("results/facility_areas.csv")

county_sf %>%
    st_drop_geometry() %>%
    select(state, county_id, county_area, solar_area, `solar_density (acres/mi2)` = solar_density) %>%
    write_csv("results/fig2_data.csv")
