library(tidyverse)
library(tigris)
library(sf)
library(usethis)
library(ggnewscale)

options(tigris_use_cache = TRUE)

# This can be a state name or abbreviation
state <- "NJ"

# List of roads to draw. See <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf> for abbreviations.
roads <- c(
  "New Jersey Tpke", "Garden State Pkwy", "Atlantic City Expy",
  "I- 95", "I- 295", "I- 80", "I- 78", "I- 287", "I- 195", "I- 280", "I- 676",
  "I- 76", "I- 278"
)

# use_zip("https://www.huduser.gov/portal/sites/default/files/zip/UPSAI_050820.zip", destdir = "20-population")

ahs_survey_2017 <- read_csv("20-population/UPSAI_050820/UPSAI_050820.csv") %>%
  janitor::clean_names()

reds <- c("#733D47", "#BE7D88", "#D2A8AF")
greens <- c("#959E4A", "#C2CA8C", "#D7DDB4")
blues <- c("#0091C1", "#71B4D7", "#9ECBE5")

state_tracts <- tracts(state = state) %>%
  janitor::clean_names() %>%
  st_transform(26917)

state_counties <- counties(state = state) %>%
  janitor::clean_names() %>%
  st_transform(26917)

state_tracts_class <- state_tracts %>%
  select(geoid, countyfp) %>%
  left_join(ahs_survey_2017) %>%
  mutate(upsai_cat_clean = case_when(
    acs17_occupied_housing_units_est == 0 ~ NA_real_,
    TRUE ~ upsai_cat_controlled
    ),
         upsai_cat_desc = case_when(
           upsai_cat_clean == 1 ~ "Urban",
           upsai_cat_clean == 2 ~ "Suburban",
           upsai_cat_clean == 3 ~ "Rural",
           is.na(upsai_cat_clean) ~ "No occupied housing",
           TRUE ~ NA_character_
         ),
    urban_cat = cut(x = upsai_urban, breaks = c(0, .6, .85, 1)),
    suburban_cat = cut(x = upsai_suburban, breaks = c(0, .6, .85, 1)),
    rural_cat = cut(x = upsai_rural, breaks = c(0, .6, .85, 1)))

roads_data <- primary_secondary_roads(state = state)
# print(roads_data[roads_data$FULLNAME %>% startsWith("New Jersey Tpke"),])
roads_data <- roads_data[roads_data$FULLNAME %in% roads,]

detail_map <- ggplot() +
  geom_sf(data = state_tracts_class %>%
            filter(upsai_cat_desc == "Urban"), aes(fill = urban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(reds)) +
  new_scale_fill() +
  geom_sf(data = state_tracts_class %>%
            filter(upsai_cat_desc == "Suburban"), aes(fill = suburban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(blues)) +
  new_scale_fill() +
  geom_sf(data = state_tracts_class %>%
            filter(upsai_cat_desc == "Rural"), aes(fill = rural_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(greens)) +
  geom_sf(data = state_counties, color = "gray10", lwd = 0.08, fill = NA) +
  geom_sf(data = roads_data) +
  theme_void()

ggsave("20-population/detailed_map.png", detail_map, dpi = 300, width = 12, height = 9)
# vim: set ts=2 sw=2 et:
