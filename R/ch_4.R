#### CHAPTER 4 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
GH <- qread("output/data/GH.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")
water <- qread("output/data/water.qs", nthreads = availableCores())


# STR- and MTR-induced housing loss ---------------------------------------

# FREH
FREH_total <- 
  monthly |> 
  filter(month >= yearmonth("2016-01-01")) |> 
  mutate(min_28 = minimum_stay >= 28) |> 
  summarize(FREH = sum(FREH_3), .by = c(min_28, month))

# Re-process GH
GH <- 
  GH |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  inner_join(monthly, by = c("month", "host_ID", "property_ID")) |> 
  summarize(
    property_IDs = list(property_ID),
    min_28 = mean(minimum_stay >= 28, na.rm = TRUE) >= 0.5,
    .by = c(ghost_ID, month, n, host_ID, listing_count, housing_units, 
            active_pct)) |> 
  mutate(property_ID = map_chr(property_IDs, first)) |> 
  inner_join(select(st_drop_geometry(property), property_ID, DA, ward), 
             by = "property_ID") |> 
  select(-property_ID)

# GH
GH_total <-
  GH |> 
  st_drop_geometry() |> 
  mutate(days = days_in_month(month)) |> 
  summarize(GH = sum(housing_units * n / days), .by = c(min_28, month)) |> 
  arrange(month, min_28)

# Total housing loss
housing_loss <-
  FREH_total |> 
  mutate(listing = if_else(min_28, "FREH MTR", "FREH STR")) |> 
  rename(value = FREH) |> 
  select(-min_28) |> 
  bind_rows(
    GH_total |> 
      mutate(listing = if_else(
        min_28, "Ghost hostel MTR", "Ghost hostel STR")) |> 
      select(-min_28) |> 
      rename(value = GH))

# Housing loss May 2023
housing_loss |> 
  filter(month == yearmonth("2023-05"))

housing_loss_2023 <- 
  housing_loss |> 
  filter(month == yearmonth("2023-05")) |> 
  pull(value) |> 
  sum() |> 
  scales::comma(accuracy = 10)

# Peak housing loss
housing_loss |> 
  summarize(value = sum(value), .by = month) |> 
  filter(value == max(value) | month == max(month)) |> 
  mutate(pct = value / max(value))

housing_loss_change_pct_2023 <-
  housing_loss |> 
  mutate(year = year(month)) |> 
  filter(year %in% 2022:2023, month(month) == 5) |> 
  summarize(units = sum(value), .by = year) |> 
  summarize(pct = (max(units) - min(units)) / min(units)) |> 
  pull() |> 
  scales::percent(0.1)

active_change_pct_2023 <- 
  monthly |> 
  mutate(year = year(month)) |> 
  filter(year %in% 2022:2023, month(month) == 5) |> 
  summarize(active = sum(R + A), .by = year) |> 
  summarize(pct = (active[year == 2023] - active[year == 2022]) / 
              active[year == 2022]) |> 
  pull(pct) |> 
  abs() |> 
  scales::percent(0.1)

# Housing loss by ward
housing_loss_ward_2022_2023 <-
  monthly |> 
  filter(month(month) == 5, year(month) %in% 2022:2023) |> 
  mutate(year = year(month)) |> 
  summarize(FREH = sum(FREH_3), .by = c(year, ward)) |> 
  inner_join(
    GH |> 
      filter(month(month) == 5, year(month) %in% 2022:2023) |> 
      mutate(year = year(month)) |> 
      mutate(days = days_in_month(month)) |> 
      summarize(GH = sum(housing_units * n / days), .by = c(year, ward)),
    by = c("year", "ward")) |> 
  mutate(FREH = coalesce(FREH, 0), GH = coalesce(GH, 0), units = FREH + GH)


# Figure 9 ----------------------------------------------------------------

fig_9 <-
  housing_loss |> 
  mutate(listing = factor(listing, levels = c(
    "Ghost hostel MTR", "FREH MTR", "Ghost hostel STR", "FREH STR"))) |> 
  ggplot(aes(month, value, fill = listing)) +
  geom_col(lwd = 0) +
  scale_fill_manual(name = "Listing type", 
                    values = col_palette[c(1, 2, 5, 4)]) +
  scale_x_yearmonth(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  # facet_wrap(~min_28, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"),
        legend.title = element_text(face = "bold"))

ggsave("output/figure_9.png", plot = fig_9, width = 8, height = 5, units = "in")


# Figure 10 ---------------------------------------------------------------

housing_loss_ward <- 
  monthly |> 
  filter(month == yearmonth("2023-05")) |> 
  summarize(FREH = sum(FREH_3), .by = ward) |> 
  inner_join(
    GH |> 
      filter(month == yearmonth("2023-05")) |> 
      mutate(days = days_in_month(month)) |> 
      summarize(GH = sum(housing_units * n / days), .by = ward),
    by = "ward") |> 
  mutate(FREH = coalesce(FREH, 0), GH = coalesce(GH, 0), units = FREH + GH)

housing_loss_DA <- 
  monthly |> 
  filter(month == yearmonth("2023-05")) |> 
  summarize(FREH = sum(FREH_3), .by = DA) |> 
  full_join(
    GH |> 
      filter(month == yearmonth("2023-05")) |> 
      mutate(days = days_in_month(month)) |> 
      summarize(GH = sum(housing_units * n / days), .by = DA),
    by = "DA") |> 
  mutate(FREH = coalesce(FREH, 0), GH = coalesce(GH, 0), units = FREH + GH)

fig_10_1 <-
  WD |> 
  left_join(housing_loss_ward, by = "ward") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = units / dwellings), colour = "white") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(6, 2, 1)], na.value = "grey90",
                    limits = c(0, 0.016), oob = scales::squish,
                    breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015), 
                    labels = scales::percent)  +
  gg_bbox(city) +
    guides(fill = guide_coloursteps(title = "% housing lost to STR/MTR", 
                                    title.vjust = 0.8)) +
    theme_void() +
    theme(text = element_text(family = "Futura"),
          legend.title = element_text(face = "bold"))
  
fig_10_2 <-
  DA |> 
  left_join(housing_loss_DA, by = c("GeoUID" = "DA")) |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = units / dwellings), colour = "transparent") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(6, 2, 1)], na.value = "grey90",
                    limits = c(0, 0.016), oob = scales::squish,
                    breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015), 
                    labels = scales::percent)  +
  gg_bbox(city) +
  guides(fill = guide_coloursteps(title = "% housing lost to STR/MTR", 
                                  title.vjust = 0.8)) +
  theme_void() +
  theme(text = element_text(family = "Futura"),
        legend.title = element_text(face = "bold"))

fig_10 <- 
  fig_10_1 + fig_10_2 + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom", legend.key.width = unit(1.8, "lines"))

ggsave("output/figure_10.png", plot = fig_10, width = 8, height = 4.2, 
       units = "in")


# Table 3 -----------------------------------------------------------------

table_3 <- 
  housing_loss_ward_2022_2023 |> 
  select(ward, units, year) |> 
  pivot_wider(names_from = year, names_prefix = "units_", 
              values_from = units) |> 
  filter(units_2023 >= 100) |> 
  arrange(-units_2023) |> 
  mutate(chg = (units_2023 - units_2022) / units_2022) |> 
  left_join(st_drop_geometry(WD), by = "ward") |> 
  mutate(pct = units_2023 / dwellings) |> 
  select(ward, units_2023, units_2022, chg, pct)

housing_loss_ward_2022_2023 |> 
  select(ward, units, year) |> 
  pivot_wider(names_from = year, names_prefix = "units_", 
              values_from = units) |>
  left_join(st_drop_geometry(WD), by = "ward") |> 
  summarize(ward = "City of Toronto", units_2022 = sum(units_2022),
            units_2023 = sum(units_2023), dwellings = sum(dwellings)) |> 
  mutate(chg = (units_2023 - units_2022) / units_2022) |> 
  mutate(pct = units_2023 / dwellings) |> 
  select(ward, units_2023, units_2022, chg, pct) |> 
  bind_rows(table_3) |> 
  mutate(across(c(units_2023, units_2022), \(x) scales::comma(x, 10)),
         across(c(chg, pct), \(x) scales::percent(x, 0.1))) |> 
  gt::gt()


# Housing loss trend analysis ---------------------------------------------

# Get daily housing loss
housing_loss_monthly_series <- 
  housing_loss |>  
  summarize(units = sum(value, na.rm = TRUE), .by = month) |> 
  tsibble::as_tsibble(index = month)

# Create housing loss model
housing_loss_model <- 
  housing_loss_monthly_series |> 
  filter(month <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast <-
  housing_loss_model |> 
  forecast(h = "49 months") |> 
  as_tibble() |> 
  select(month, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  full_join(housing_loss_forecast, by = "month")

# Add decay to growth rate
housing_loss_monthly_decay <-
  housing_loss_monthly_series |> 
  mutate(decay = 0.98 ^ (as.numeric(month) - 602)) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[month == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[month == yearmonth("Mar 2020")] + 
      (lag * decay))

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_decay |> 
  select(month, units, units_trend = units_trend_month) |> 
  mutate(units_trend = if_else(month >= yearmonth("2020-03-01"), 
                               units_trend, NA_real_),
         units_trend = if_else(month == yearmonth("2020-02"), 
                               units, units_trend))

housing_loss_2023 <-
  housing_loss_monthly_series |> 
  filter(month == yearmonth("2023 May")) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_trend_2023 <- 
  housing_loss_monthly_series |> 
  filter(month == yearmonth("2023 May")) |> 
  pull(units_trend) |> 
  scales::comma(10)

housing_loss_dif_pct_2023 <- 
  housing_loss_monthly_series |> 
  filter(month == yearmonth("2023 May")) |> 
  summarize(dif = (units_trend - units) / units) |> 
  pull(dif) |> 
  scales::percent(0.1)

housing_loss_trend_2023 <- 
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  filter(month == yearmonth("2023 Jun")) |> 
  summarize(units = sum(units), units_trend = sum(units_trend),
            dif = (units_trend - units) / units)


# Figure 11 ---------------------------------------------------------------

fig_11 <-
  housing_loss_monthly_series |> 
  as_tibble() |> 
  filter(month <= yearmonth("2023-05")) |> 
  pivot_longer(-month) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    month == yearmonth("2020-09") & name == "units" ~ "Actual housing loss",
    month == yearmonth("2021-01") & name == "units_trend" ~ 
      "Expected housing loss",
    .default = NA_character_)) |>
  ggplot() +
  geom_ribbon(aes(x = month, ymin = units, ymax = units_trend, group = 1),
              data = filter(housing_loss_monthly_series,
                            month <= yearmonth("2023-05")), 
              fill = col_palette[2], alpha = 0.2) +
  geom_line(aes(month, value, color = name), lwd = 0.5) +
  geom_label(aes(month, value, label = label, color = name),
             fill = alpha("white", 0.75), size = 3) +
  scale_x_yearmonth(name = NULL, limits = as.Date(c("2017-07-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_11.png", fig_11, width = 8, height = 5)




