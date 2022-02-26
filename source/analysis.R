library(tidyverse)
incra_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# Map
minority_pop <- incra_trends %>%
  rowwise() %>%
  mutate(minority_pop_prison = sum(black_prison_pop, native_prison_pop, other_race_prison_pop,
                                                         latinx_prison_pop, aapi_prison_pop, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarise(
    mean_minority_prison_pop = mean(minority_pop_prison, na.rm = TRUE),
    mean_prison_total = mean(total_prison_pop, na.rm = TRUE),
    mean_perecentage_minority_prison = (mean_minority_prison_pop / mean_prison_total) * 100
  ) %>%
  mutate(state = tolower(setNames(state.name, state.abb)[state]))

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(minority_pop, by = "state")

mapPlot <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = mean_perecentage_minority_prison),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) + coord_map() +
  labs(title = "U.S. States Minority Prision Population Percentage",
  fill = "Minority prison Population Percentage") + 
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# Time Series
minority_pop_wa_overtime <- incra_trends %>%
  rowwise() %>%
  mutate(minority_pop_prison = sum(black_prison_pop, native_prison_pop, other_race_prison_pop,
                                 latinx_prison_pop, aapi_prison_pop, na.rm = TRUE),
         minority_pop_jail = sum(black_jail_pop, native_jail_pop, other_race_jail_pop,
                                 latinx_jail_pop, aapi_jail_pop, na.rm = TRUE)) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(
    mean_minority_prison_pop = mean(minority_pop_prison, na.rm = TRUE),
    mean_prison_total = mean(total_prison_pop, na.rm = TRUE),
    mean_perecentage_minority_prison = (mean_minority_prison_pop / mean_prison_total) * 100,
    mean_minority_jail_pop = mean(minority_pop_jail, na.rm = TRUE),
    mean_jail_total = mean(total_jail_pop, na.rm = TRUE),
    mean_perecentage_minority_jail = (mean_minority_jail_pop / mean_jail_total) * 100
  )

timePlot <- ggplot(minority_pop_wa_overtime) +
  geom_line(
    mapping = aes(x= year, y=mean_perecentage_minority_prison, color = "Prision Population"),
    key_glyph = "timeseries"
  ) +
  geom_line(
    mapping = aes(x= year, y=mean_perecentage_minority_jail, color = "Jail Population"),
    key_glyph = "timeseries"
  ) +
  labs(title = "Average Washigton Minority Prision & Jail Population from 1975 - 2018",
       y = "Average Population Percentage", color = "incarceration Facility") +
  theme_minimal()
# Variables Count
minority_prision_jail_pop <- incra_trends %>%
  rowwise() %>%
  mutate(minority_pop_prison = sum(black_prison_pop, native_prison_pop, other_race_prison_pop,
                                   latinx_prison_pop, aapi_prison_pop, na.rm = TRUE),
         minority_pop_jail = sum(black_jail_pop, native_jail_pop, other_race_jail_pop,
                                 latinx_jail_pop, aapi_jail_pop, na.rm = TRUE)) %>%
  select(minority_pop_prison, minority_pop_jail)

variable_plot <- ggplot(minority_prision_jail_pop) +
  geom_point(
    mapping = aes(x = minority_pop_jail, y = minority_pop_prison),
    alpha = .3
  ) +
  geom_smooth(
    mapping = aes(x = minority_pop_jail, y = minority_pop_prison),
    color = "red"
  ) +
  labs(x = "Minority Jail Population", y = "Minority Prision Population") +
  ggtitle("Minority Jail Population versus Prision Population") +
  theme_minimal()


# Summary Information
# What is the minority prison population percentage for the latest year with viable data?
# What is the population of each minority group for that year?
# How many observations and variables does this data set have?
summaryStat <- list()
summaryStat$obs <- nrow(incra_trends) 
summaryStat$var <- ncol(incra_trends)

minority_prision_jail_pop_year <- incra_trends %>%
  rowwise() %>%
  mutate(minority_pop_prison = sum(black_prison_pop, native_prison_pop, other_race_prison_pop,
                                   latinx_prison_pop, aapi_prison_pop, na.rm = TRUE),
         minority_pop_jail = sum(black_jail_pop, native_jail_pop, other_race_jail_pop,
                                 latinx_jail_pop, aapi_jail_pop, na.rm = TRUE)) %>%
  select(year, minority_pop_prison, minority_pop_jail, black_prison_pop, 
         native_prison_pop, other_race_prison_pop,
         latinx_prison_pop, aapi_prison_pop,total_prison_pop)

recent_minority_prision <- minority_prision_jail_pop_year %>%
  group_by(year) %>% 
  summarise(
    total_prison = sum(total_prison_pop, na.rm = TRUE),
    mean_prison_minority_pop = sum(minority_pop_prison),
    total_black_pop = sum(black_prison_pop, na.rm = TRUE),
    total_native_pop = sum(native_prison_pop, na.rm = TRUE),
    total_latinx_pop = sum(latinx_prison_pop, na.rm = TRUE),
    total_aapi_pop = sum(aapi_prison_pop, na.rm = TRUE),
    total_other_pop = sum(other_race_prison_pop, na.rm = TRUE)
  ) %>%
  filter(is.nan(mean_prison_minority_pop) == FALSE) %>%
  filter(mean_prison_minority_pop != 0) %>%
  arrange(+year) %>%
  tail(n=1) %>%
  summarise(
    minority_percentage = (mean_prison_minority_pop / total_prison) * 100,
    black_percentage =  (total_black_pop / total_prison) * 100,
    latinx_percentage =  (total_latinx_pop / total_prison) * 100,
    appi_percentage =  (total_aapi_pop / total_prison) * 100,
    native_percentage =  (total_native_pop / total_prison) * 100,
    other_percentage =  (total_other_pop / total_prison) * 100,
  )

# What year is the latest minority prison population statistic from?
recent_minority_prision_year <- minority_prision_jail_pop_year %>%
  group_by(year) %>% 
  summarise(
    mean_prison_minority_pop = mean(minority_pop_prison)
  ) %>%
  filter(is.nan(mean_prison_minority_pop) == FALSE) %>%
  filter(mean_prison_minority_pop != 0) %>%
  arrange(+year) %>%
  tail(n=1) %>%
  pull(year)
  