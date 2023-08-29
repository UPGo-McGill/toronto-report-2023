#### CHAPTER 6 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm", nthreads = availableCores())
ltr <- qread("output/data/ltr_processed.qs", nthreads = availableCores())


# How many properties have moved?  ----------------------------------------

# STR listings with matches
property |> 
  filter(!is.na(ltr_ID)) |> 
  nrow() |> 
  scales::comma()
  
# LTR listings with matches
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID)) |> 
  distinct(id) |> 
  nrow() |> 
  scales::comma()

# LTR platform
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID)) |> 
  distinct(id, .keep_all = TRUE) |> 
  count(kj) |> 
  mutate(pct = n / sum(n))

# STR creation date
property |> 
  st_drop_geometry() |> 
  filter(!is.na(ltr_ID)) |> 
  count(scraped >= "2020-01-01") |> 
  mutate(pct = n / sum(n))

# KJ rental length
ltr |> 
  st_drop_geometry() |> 
  filter(kj, !is.na(property_ID)) |> 
  distinct(id, .keep_all = TRUE) |> 
  count(type) |> 
  mutate(pct = n / sum(n))

# Furnished or not
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID), !is.na(furnished)) |> 
  distinct(id, .keep_all = TRUE) |> 
  count(furnished) |> 
  mutate(pct = n / sum(n))


# Figure 14 ---------------------------------------------------------------

# This figure is copied over from the previous report



# When did listings move? -------------------------------------------------

# End of March 2020
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID)) |> 
  filter(!is.na(created)) |> 
  unnest(property_ID) |> 
  inner_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  filter(ab_first) |> 
  select(id, created, scraped, property_ID) |> 
  nest(property_ID = property_ID) |> 
  count(created, sort = TRUE) |> 
  filter(created <= "2020-03-31")

# Average May-Dec 2020
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID)) |> 
  filter(!is.na(created)) |> 
  unnest(property_ID) |> 
  inner_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  filter(ab_first) |> 
  select(id, created, scraped, property_ID) |> 
  nest(property_ID = property_ID) |> 
  filter(created >= "2020-05-01", created <= "2020-12-31") |> 
  summarize(avg = n() / sum(days_in_month(5:12)))

# % moving STR-LTR
ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(property_ID)) |> 
  filter(!is.na(created)) |> 
  unnest(property_ID) |> 
  inner_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  filter(!is.na(ab_first)) |> 
  count(ab_first) |> 
  mutate(pct = n / sum(n))
  
# ab_first and ltr_first
ltr_first <- 
  property |> 
  st_drop_geometry() |> 
  unnest(ltr_ID) |> 
  left_join(
    ltr |> 
      st_drop_geometry() |> 
      select(ltr_ID = id, ltr_created = created), by = "ltr_ID",
    relationship = "many-to-many") |> 
  select(property_ID, ltr_ID, created, ltr_created) |> 
  mutate(ab_first = created < ltr_created) |> 
  distinct(property_ID, .keep_all = TRUE) |> 
  count(created, ab_first) |> 
  mutate(ab_first = coalesce(ab_first, TRUE)) |> 
  summarize(ltr_first = sum(n[!ab_first]) / sum(n), .by = created)

ab_first <- 
  ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(created)) |> 
  unnest(property_ID) |> 
  left_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  distinct(id, .keep_all = TRUE) |> 
  count(created, ab_first) |> 
  mutate(ab_first = coalesce(ab_first, FALSE)) |> 
  summarize(ab_first = sum(n[ab_first]) / sum(n), .by = created)

switches <-
  ab_first |> 
  full_join(ltr_first, by = "created") |> 
  pivot_longer(c(ab_first, ltr_first))


# Figure 15 ---------------------------------------------------------------

fig_15 <- 
  ltr |> 
  st_drop_geometry() |> 
  unnest(property_ID) |> 
  filter(!is.na(property_ID)) |> 
  inner_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  select(id, kj, created, scraped, ab_first, property_ID) |> 
  nest(property_ID = property_ID) |> 
  count(created, kj, ab_first) |> 
  mutate(n = if_else(!ab_first, n * -1, n)) |> 
  filter(created <= "2022-12-31") |> 
  ggplot() +
  geom_col(aes(created, n, fill = kj), lwd = 0) +
  geom_hline(yintercept = 0, lwd = 0.5, colour = "black") +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_fill_manual(name = NULL, labels = c("Craigslist", "Kijiji"), 
                    values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_15.png", plot = fig_15, width = 8, height = 5, 
       units = "in")


# Figure 16 ---------------------------------------------------------------

fig_16 <-
  switches |> 
  arrange(created, name) |> 
  mutate(value = slide_dbl(value, mean, .before = 6), .by = name) |>
  filter(!is.na(value)) |> 
  filter(created >= "2020-03-15") |>
  filter(created <= "2022-12-14") |>
  filter(name == "ab_first") |>
  mutate(label = if_else(created == "2021-01-01", case_when(
    name == "ab_first" ~ "Switched from STR to LTR",
    name == "ltr_first" ~ "Switched from LTR to STR"),
    NA_character_)) |>
  ggplot(aes(created, value, colour = name)) +
  geom_line(lwd = 1) +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(1, 5)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_16.png", plot = fig_16, width = 8, height = 3, 
       units = "in")


# Spatial distribution of matched listings --------------------------------

# Unique STRs that moved to LTR
str_to_ltr <-
  property |> 
  st_drop_geometry() |> 
  unnest(ltr_ID) |> 
  inner_join(
    ltr |> 
      st_drop_geometry() |> 
      select(ltr_ID = id, ltr_created = created), by = "ltr_ID",
    relationship = "many-to-many") |> 
  # select(property_ID, ltr_ID, created, ltr_created) |> 
  filter(is.na(ltr_created) | is.na(created) | created < ltr_created) |> 
  distinct(property_ID, .keep_all = TRUE) |> 
  select(property_ID, ltr_ID, DA, ward)
  
# How many in top wards
str_to_ltr |> 
  count(ward, sort = TRUE) |> 
  mutate(pct = n / sum(n)) |> 
  inner_join(
    monthly |> 
      filter(month == yearmonth("2020-03"), R + A > 0) |> 
      count(ward) |> 
      rename(n_2020 = n), by = "ward") |> 
  mutate(pct_2020 = n / n_2020) |> 
  inner_join(
    monthly |> 
      filter(month >= yearmonth("2020-03"), R + A > 0) |> 
      count(ward) |> 
      rename(n_2023 = n), by = "ward") |> 
  mutate(pct_ward_2023 = n / n_2023)


# Figure 17 ---------------------------------------------------------------

fig_17_left <-
  WD |> 
  inner_join(count(str_to_ltr, ward), by = "ward") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = n), colour = "white") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(name = "Total STR to\nLTR matches",
                    colors = col_palette[c(6, 2)],
                    labels = scales::comma,
                    breaks = c(0, 500, 1000, 1500, 2000),
                    na.value = "grey80") +
  gg_bbox(city) +
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(face = "plain", family = "Futura", size = 7),
        legend.title = element_text(face = "bold"))

fig_17_right <-
  WD |> 
  inner_join(count(str_to_ltr, ward), by = "ward") |> 
  inner_join(
    monthly |> 
      filter(month == yearmonth("2020-03"), R + A > 0) |> 
      count(ward) |> 
      rename(n_total = n), by = "ward") |> 
    mutate(pct = n / n_total) |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = pct), colour = "white") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(name = "Matches as %\nof active STRs",
                    colors = col_palette[c(2, 5)],
                    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                    na.value = "grey80",
                    label = scales::label_percent(accuracy = 1)) +
  gg_bbox(city) +
  theme_void() +
    theme(legend.position = "bottom",
          text = element_text(face = "plain", family = "Futura", size = 7),
          legend.title = element_text(face = "bold"))

fig_17 <- fig_17_left + fig_17_right

ggsave("output/figure_17.png", plot = fig_17, width = 8, height = 4.2, 
       units = "in")


# Asking rents ------------------------------------------------------------

ltr_unique <- 
  ltr |> 
  st_drop_geometry() |> 
  unnest(property_ID) |> 
  left_join(
    property |> 
      st_drop_geometry() |> 
      select(property_ID, ab_created = created, ab_scraped = scraped),
    by = "property_ID") |> 
  mutate(ab_first = ab_created < created) |> 
  arrange(desc(scraped)) |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(match = !is.na(property_ID) & (is.na(ab_first) | ab_first)) 

asking_rents <-
  ltr_unique |> 
  filter(price > 425, price < 8000) |> 
  summarize(avg_price = mean(price), .by = c(match, created)) |> 
  arrange(created, match) |> 
  mutate(status = if_else(match, "Matched to STR", "Not matched"), 
         .before = created) |>  
  select(-match)

asking_rents <- 
  ltr_unique |>  
  filter(price > 425, price < 8000) |> 
  summarize(avg_price = mean(price), .by = created) |> 
  mutate(status = "All listings", .before = created) |> 
  bind_rows(asking_rents) |> 
  mutate(geography = "City of Toronto")

asking_rents_dt <- 
  ltr_unique |> 
  filter(price > 425, price < 8000, ward == "Spadina-Fort York") |> 
  summarize(avg_price = mean(price), .by = c(match, created)) |> 
  arrange(created, match) |> 
  mutate(status = if_else(match, "Matched to STR", "Not matched"), 
         .before = created) |>  
  select(-match)

asking_rents <- 
  ltr_unique |>  
  filter(price > 425, price < 8000, ward == "Spadina-Fort York") |> 
  summarize(avg_price = mean(price), .by = created) |> 
  mutate(status = "All listings", .before = created) |> 
  bind_rows(asking_rents_dt) |> 
  mutate(geography = "Spadina-Fort York") |> 
  bind_rows(asking_rents)

asking_rents <- 
  asking_rents |> 
  arrange(created, status, geography)

# Average asking rent April 2020
asking_rents |> 
  filter(created >= "2020-04-01", created <= "2020-04-30",
         geography == "City of Toronto") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Average asking rent Jan 2021
asking_rents |> 
  filter(created >= "2021-01-01", created <= "2021-01-31",
         geography == "City of Toronto") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Average asking rent Apr 2020 - Jan 2021
asking_rents |> 
  filter(created >= "2020-04-01", created <= "2021-01-31",
         geography == "City of Toronto") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Average asking rent Fort York Apr 2020 - Jan 2021
asking_rents |> 
  filter(created >= "2020-04-01", created <= "2021-01-31",
         geography == "Spadina-Fort York") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Average asking rent first week April 2020
asking_rents |> 
  filter(created >= "2020-04-01", created <= "2020-04-07",
         geography == "City of Toronto") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Average asking rent last week Jan 2021
asking_rents |> 
  filter(created >= "2021-01-25", created <= "2021-01-31",
         geography == "City of Toronto") |> 
  summarize(avg_price = mean(avg_price), .by = status) |> 
  mutate(pct_higher = avg_price / avg_price[status == "All listings"] - 1)

# Pct change April 2020 - Jan 2021
asking_rents |> 
  filter(geography == "City of Toronto") |> 
  summarize(april = mean(avg_price[created >= "2020-04-01" & 
                                     created <= "2020-04-07"], na.rm = TRUE), 
            jan = mean(avg_price[created >= "2021-01-25" & 
                                     created <= "2021-01-31"], na.rm = TRUE), 
            .by = status) |> 
  mutate(change = (jan - april) / april)

# Pct change Feb 2021 - Dec 2022
asking_rents |> 
  filter(geography == "City of Toronto") |> 
  summarize(feb = mean(avg_price[created >= "2021-02-01" & 
                                   created <= "2021-02-07"], na.rm = TRUE), 
            dec = mean(avg_price[created >= "2022-12-08" & 
                                   created <= "2022-12-14"], na.rm = TRUE), 
            .by = status) |> 
  mutate(change = (dec - feb) / feb)


# Figure TK ---------------------------------------------------------------

fig_16 <- 
  asking_rents |> 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 13), 
         .by = c(status, geography)) |> 
  filter(created >= "2020-04-01", created <= "2022-12-14") |> 
  filter(status != "Not matched") |> 
  ggplot(aes(created, avg_price, color = status)) +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL, limits = c(as.Date("2020-04-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::dollar) +
  scale_color_manual(name = NULL, values = col_palette[c(4, 2)]) +
  facet_wrap(vars(geography)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Futura"), 
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_16.png", plot = fig_16, width = 8, height = 5, 
       units = "in")


# Financial performance ---------------------------------------------------

ltr_first_price <- 
  ltr |> 
  st_drop_geometry() |> 
  filter(price > 425, price < 8000) |> 
  arrange(id, scraped) |> 
  summarize(ltr_price = first(price), ltr_created = first(created), .by = id) |> 
  rename(ltr_ID = id)

prop_ltr <- 
  property |> 
  st_drop_geometry() |> 
  mutate(match = !is.na(ltr_ID)) |> 
  unnest(ltr_ID) |> 
  left_join(ltr_first_price, by = "ltr_ID") |> 
  select(property_ID, listing_type, created, scraped, ltr_ID, ltr_price, 
         ltr_created) |> 
  arrange(property_ID, ltr_created) |> 
  distinct(property_ID, .keep_all = TRUE) |> 
  mutate(str_to_ltr = !is.na(ltr_ID) & (is.na(created) | is.na(ltr_created) | 
                                          ltr_created > created)) |> 
  mutate(ltr_early = case_when(
    str_to_ltr & ltr_created < "2021-02-01" ~ "ltr_early",
    str_to_ltr & ltr_created >= "2021-02-01" ~ "ltr_late",
    .default = "no_ltr")) |> 
  select(property_ID, ltr_early)

# 2020 monthly rev
monthly |> 
  left_join(prop_ltr, by = "property_ID") |> 
  filter(year(month) >= 2020) |> 
  filter(month <= yearmonth("2022-12")) |> 
  filter(A + R > 0) |>
  summarize(rev = mean(revenue), .by = c(month, ltr_early)) |> 
  filter(year(month) == 2020) |> 
  summarize(rev = mean(rev), .by = ltr_early) |> 
  mutate(pct = rev / rev[ltr_early == "no_ltr"] - 1)
  
# 2021 monthly rev
monthly |> 
  left_join(prop_ltr, by = "property_ID") |> 
  filter(year(month) >= 2020) |> 
  filter(month <= yearmonth("2022-12")) |> 
  filter(A + R > 0) |>
  summarize(rev = mean(revenue), .by = c(month, ltr_early)) |> 
  # filter(year(month) == 2021) |> 
  filter(month >= yearmonth("2021-02"), month <= yearmonth("2022-06")) |> 
  summarize(rev = mean(rev), .by = ltr_early) |> 
  mutate(pct = rev / rev[ltr_early == "no_ltr"] - 1)


# Figure TK ---------------------------------------------------------------

fig_17 <-
  monthly |> 
  left_join(prop_ltr, by = "property_ID") |> 
  filter(year(month) >= 2020) |> 
  filter(month <= yearmonth("2022-12")) |> 
  filter(A + R > 0) |>
  summarize(rev = mean(revenue), .by = c(month, ltr_early)) |> 
  mutate(label = if_else(month == yearmonth("2020-08"), case_when(
    ltr_early == "ltr_early" ~ "Switched before Feb. 2021",
    ltr_early == "ltr_late" ~ "Switched after Feb. 2021",
    ltr_early == "no_ltr" ~ "Did not' switch"),
    NA_character_)) |>
  ggplot(aes(month, rev, colour = ltr_early)) +
  geom_line(lwd = 1.2) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  geom_vline(aes(xintercept = as.Date("2021-02-01")), linetype = 3) +
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_x_yearmonth(name = NULL) +
  scale_colour_manual(values = col_palette[c(2, 4, 7)]) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Futura"))

ggsave("output/figure_17.png", plot = fig_17, width = 8, height = 5, 
       units = "in")

