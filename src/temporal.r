library(tidyverse)
library(sf)
library(broom)

### Load data

uspvdb <- read_sf("data/uspvdb_v1_0_20231108.geojson")

### Calculate area efficiency regression

uspvdb %>%
    st_drop_geometry() %>%
    summarise(sum(p_cap_dc))

area_eff <- uspvdb %>% 
    st_drop_geometry() %>%
    filter(p_year > 2006) %>%
    mutate(area_eff = p_cap_dc/p_area*1e6) # watts per m^2

View(area_eff)

area_eff %>% filter(area_eff > 200)

ann_area <- uspvdb %>% 
    st_drop_geometry() %>%
    filter(p_year > 2000, p_year < 2020) %>%
    group_by(p_year) %>%
    summarise(ann_area = sum(p_area))

ann_area %>%
    ggplot(aes(x = p_year, y = ann_area)) +
    geom_point() +
    geom_smooth(method = "lm", formula = log(y) ~ log(x))

ann_area %>%
    lm(formula = log(ann_area) ~ log(p_year)) %>% summary()

area_eff %>%
    filter(area_eff < quantile(area_eff, .98)) %>%
    # group_by(p_year) %>%
    # summarise(area_eff = sum(p_cap_dc)/sum(p_area)) %>%
    ggplot(aes(x = p_year, group = p_year, y = area_eff)) +
    geom_boxplot(col = "") +
    theme_bw()

plot(ann_area$p_year, sqrt(ann_area$ann_area))
lines(2000:2030, exp(-8481.8)*(2000:2030)^1117.2)


ann_area %>%
    nls(
        formula = ann_area ~ a*exp(b*(p_year - 2000))
    )

ann_area %>%
    lm(formula = ann_area ~ I(p_year) + I(p_year^2)) %>% summary()

plot(ann_area$p_year, ann_area$ann_area)

ann_area %>%
    lm(formula = log(ann_area) ~ I(p_year - 2000))

# Regression on all data for comparison

lm(I(p_cap_dc/p_area*1e6) ~ p_year + p_area + p_state, uspvdb) %>% summary()

uspvdb %>%
    mutate()

uspvdb %>% st_drop_geometry() %>% group_by(p_state) %>% summarise(state_cap = sum(p_cap_dc)) %>% arrange(desc(state_cap)) %>% .$state_cap %>% plot

lm(log(p_cap_dc) ~ log(p_year) + log(p_area), data = uspvdb) %>% summary()

uspvdb %>%
    ggplot(aes(x = p_year, y = p_cap_dc/p_area*1e6)) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ I(sqrt(x))) +
    scale_y_continuous(limits = c(0, 200))

lm(data = uspvdb, I(p_cap_dc/p_area*1e6) ~ p_year, weights = p_area)

uspvdb %>%
    st_drop_geometry() %>%
    filter(p_year > 2000) %>%
    group_by(p_year) %>%
    summarise(
        y1 = sum(p_cap_dc)/sum(p_area)*1e6
    ) %>%
    ggplot(aes(x = p_year, y = y1)) +
    geom_point()


plot(y = log(uspvdb$p_area), uspvdb$p_year)

area_eff <- uspvdb %>%
    st_drop_geometry() %>%
    transmute(p_year, area_eff = p_cap_dc/p_area*1e6) # watts per m^2

uspvdb %>%
    ggplot(aes(x = p_area, y = p_cap_dc)) +
    geom_point()

area_eff %>%
    filter(p_year > 2000) %>%
    ggplot(aes(x = p_year, y = area_eff)) 
    lm(formula = I(sqrt(area_eff)) ~ p_year) %>% summary()

plot(area_eff$area_eff, x = area_eff$p_year)

lm_data <- analysis_data %>%
    filter(p_year > 2000) %>%
    transmute(
        p_year, 
        area_eff, 
        sqrt_ann_area = annual_area ^ .5, # fits a quadratic better than an exponential
        cube_root_cumul_a = cumul_area ^ (1/3), # cumulative sum similar to integral
        log_ann_area = log10(annual_area),
        log_cumul_a = log10(cumul_area)
    )

lm_results <- lm_data %>%
    # first set the data in long format    
    gather(
    key = "column", 
    value = "value", 
    area_eff:log_cumul_a
    ) %>%
    nest(p_year, value) %>%                                         # now nest the dependent and independent factors
    mutate(model = map(data, ~lm(p_year ~ value, data = .))) %>%    # fit the model using purrr
    mutate(tidy_model = map(model, tidy)) %>%                       # clean the model output with broom
    mutate(R2 = map(model, ~summary(.x)$r.squared)) %>%             # extract R squared
    select(-data, -model) %>%                                       # remove the "untidy" parts
    unnest()

lm_results

analysis_data %>%
    filter(p_year > 2000) %>%
    ggplot(aes(x = p_year, y = annual_area)) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1, se = FALSE)

line_graph <- analysis_data %>%
    pivot_longer(annual_area:cumul_area) %>%
    ggplot(aes(x = p_year, y = value)) +
    geom_point() +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    scale_x_continuous(limits = c(2000,NA)) +
    theme_bw()

ggsave("results/line_graph.png", line_graph)
