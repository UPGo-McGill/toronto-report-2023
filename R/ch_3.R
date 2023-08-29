#### CHAPTER 3 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
ltr <- qread("output/data/ltr_processed.qs", nthreads = availableCores())
qload("output/data/geometry.qsm", nthreads = availableCores())
water <- qread("output/data/water.qs", nthreads = availableCores())


# Medium-term rentals on Airbnb and Vrbo ----------------------------------

# MTR shares in 2018-2019
monthly |> 
  filter(year(month) >= 2017) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(
    displayed_pct = sum(A[min28] + R[min28] + B[min28]) / sum(A + R + B),
    active_pct = sum(A[min28] + R[min28]) / sum(A + R),
    res_pct = sum(R[min28]) / sum(R),
    rev_pct = sum(revenue[min28] / sum(revenue)),
    .by = month) |> 
  filter(year(month) %in% 2018:2019) |> 
  summarize(across(everything(), mean))

# MTR shares in 2021-2023
monthly |> 
  filter(year(month) >= 2021) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(
    displayed_pct = sum(A[min28] + R[min28] + B[min28]) / sum(A + R + B),
    active_pct = sum(A[min28] + R[min28]) / sum(A + R),
    res_pct = sum(R[min28]) / sum(R),
    rev_pct = sum(revenue[min28] / sum(revenue)),
    .by = month) |> 
  mutate(year = year(month)) |> 
  summarize(across(everything(), mean), .by = year)

  
# Figure 6 ----------------------------------------------------------------

figure_6 <- 
  monthly |> 
  filter(year(month) >= 2017) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(
    displayed_pct = sum(A[min28] + R[min28] + B[min28]) / sum(A + R + B),
    active_pct = sum(A[min28] + R[min28]) / sum(A + R),
    res_pct = sum(R[min28]) / sum(R),
    rev_pct = sum(revenue[min28] / sum(revenue)),
    .by = month) |> 
  pivot_longer(-month) |> 
  mutate(label = if_else(month == yearmonth("2022-05-01"), case_when(
    name == "displayed_pct" ~ "Displayed listings",
    name == "active_pct" ~ "Active listings",
    name == "res_pct" ~ "Reserved nights",
    name == "rev_pct" ~ "Host revenue"), 
    NA_character_)) |>
  ggplot(aes(month, value, colour = name)) +
  geom_line(lwd = 1.5) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_yearmonth(name = NULL) +
  scale_linetype_discrete(name = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1, 2, 6)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_6.png", plot = figure_6, width = 8, height = 5, 
       units = "in")


# Figure 7 ----------------------------------------------------------------

fig_7 <- 
  monthly |> 
  filter(month >= yearmonth("2017-07")) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  filter(min28) |> 
  summarize(active = sum(A + R), active_pct = active / sum(A + R + B), 
            .by = month) |> 
  mutate(active = active / days_in_month(month)) |> 
  pivot_longer(c(active, active_pct)) |> 
  mutate(name = if_else(name == "active", "Average daily active MTR listings",
                        "Share of displayed MTR listings which are active")) |> 
  ggplot(aes(month, value, colour = name)) + 
  geom_line(lwd = 1.2) +
  geom_vline(aes(xintercept = as.Date("2021-01-01")), linetype = 3) +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  facetted_pos_scales(y = list(
    scale_y_continuous(name = NULL, labels = scales::comma),
    scale_y_continuous(name = NULL, label = scales::percent))) +
  scale_x_yearmonth(name = NULL) +
  scale_colour_manual(values = col_palette[c(1, 5)]) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_7.png", plot = fig_7, width = 8, height = 5, units = "in")


# Figure 8 ----------------------------------------------------------------

mtr_for_map <- 
  monthly |> 
  filter(month == yearmonth("2023-05"), A + R > 0) |> 
  mutate(min28 = minimum_stay >= 28) |> 
  summarize(active = sum(A[min28] + R[min28]) / days_in_month(5),
            active_pct = active / (sum(A + R) / days_in_month(5)),
            .by = ward)

fig_8_1 <-
  WD |> 
  inner_join(mtr_for_map, by = "ward") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active / dwellings), colour = "white") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(6, 2)], na.value = "grey80",
                    limits = c(0, 0.015), oob = scales::squish, 
                    breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015), 
                    labels = scales::percent)  +
  gg_bbox(city) +
  guides(fill = guide_coloursteps(title = "MTRs/dwelling", title.vjust = 0.8)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")

fig_8_2 <-
  WD |> 
  inner_join(mtr_for_map, by = "ward") |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = active_pct), colour = "white") +
  geom_sf(data = water, fill = "white", colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(2, 5)], na.value = "grey80",
                    breaks = c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), 
                    labels = scales::percent)  +
  gg_bbox(city) +
  guides(fill = guide_coloursteps(title = "MTRs share of Airbnb/Vrbo listings", 
                                  title.vjust = 0.8)) +
  theme_void() +
  theme(text = element_text(family = "Futura", size = 7),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom")

fig_8 <- fig_8_1 + fig_8_2

ggsave("output/figure_8.png", plot = fig_8, width = 8, height = 4, 
       units = "in")


# What is the total volume of MTR activity in Toronto?  -------------------

# Construct listing table
MTR_listings <-
  monthly |> 
  filter(month == max(month), minimum_stay >= 28) |> 
  summarize(platform = "Airbnb/Vrbo", available = sum(A) / 31, 
            total = sum(A + R) / 31) |> 
  bind_rows(tribble(
    ~platform,    ~available, ~total,
    "Kijiji", 630, NA,
    "Craigslist", 395, NA,
    "Rentals.ca", 302, NA,
    "The Square", NA, 1696, 
    "Rent It Furnished", 76, 1061,
    "Toronto Furnished Rentals", NA, 309,
    "Toronto Luxury Suites", NA, 181,
    "Sabbatical Homes", 84, 195,
    "Premier Suites", NA, 179,
    "Amber Student", 3, 114,
    "Toronto Boutique Apartments", 0, 73))

total_listings <-
  MTR_listings |> 
  mutate(available = if_else(platform == "Airbnb/Vrbo", 0, available)) |> 
  summarize(lst = sum(available, na.rm = TRUE) + sum(total, na.rm = TRUE) + 944)
  
vac_rate <-
  MTR_listings |> 
  filter(platform != "Airbnb/Vrbo", !is.na(available), !is.na(total)) |> 
  summarize(rt = sum(available) / sum(total)) |> 
  pull()
  
MTR_listings <- 
  MTR_listings |> 
  mutate(total = round(coalesce(total, available / vac_rate)),
         available = round(coalesce(available, total * vac_rate))) |> 
  arrange(-available) |> 
  add_row(platform = "Other platforms", available = round(944 * vac_rate), 
          total = 944)


# Table 2 -----------------------------------------------------------------

MTR_listings |> 
  reframe(
    platform = c("Total", "Total (50% LTR duplicates)"),
    across(c(available, total), \(x) c(
      sum(x), sum(x[3:4] * .5, x[-c(3:4)])))) |> 
  bind_rows(x = MTR_listings, y = _) |> 
  mutate(across(c(available:total), \(x) scales::comma(x, 10))) |> 
  gt::gt()



# Are MTRs increasing in Toronto? -----------------------------------------

ltr |> 
  st_drop_geometry() |> 
  filter(kj, scraped >= "2020-06-01") |> 
  summarize(furn_pct = mean(furnished, na.rm = TRUE), .by = scraped) |> 
  ggplot(aes(scraped, furn_pct)) + 
  geom_line()



# Figure 9 ----------------------------------------------------------------

fig_9 <-
  ltr |> 
  st_drop_geometry() |> 
  filter(scraped >= "2020-06-01", scraped <= "2022-12-31") |> 
  summarize(short_pct = mean(short_long == "short", na.rm = TRUE), 
            furn_pct = mean(furnished, na.rm = TRUE), .by = scraped) |> 
  filter(short_pct >= 0.01, furn_pct > 0.1) |>
  mutate(furn_pct = slide_dbl(furn_pct, mean, .before = 1),
         short_pct = slide_dbl(short_pct, mean, .before = 1)) |> 
  pivot_longer(c(short_pct, furn_pct)) |> 
  mutate(name = if_else(name == "short_pct", "Share of Kijiji listings which are monthly rentals",
                        "Share of Craigslist/Kijiji listings which are furnished rentals")) |> 
  ggplot(aes(scraped, value, colour = name)) +
  geom_line(lwd = 1) +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  scale_colour_manual(values = col_palette[c(1, 5)]) +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_9.png", plot = fig_9, width = 8, height = 5, 
       units = "in")


# How do MTR rents compare to STR and LTR rents? --------------------------

rents <- 
  ltr |> 
  st_drop_geometry() |> 
  filter(!is.na(short_long), !is.na(price), !is.na(ward)) |> 
  filter(price > 425, price < 8000) |> 
  mutate(month = yearmonth(scraped)) |> 
  filter(month >= yearmonth("2020-04")) |> 
  summarize(rent = mean(price), .by = c(month, short_long, ward)) |> 
  summarize(rent = mean(rent), .by = c(month, short_long)) |> 
  transmute(platform = "Kijiji", month, 
            type = if_else(short_long == "short", "MTR", "LTR"), rent)

rents <- 
  monthly |> 
  mutate(min_28 = minimum_stay >= 28) |> 
  filter(month >= yearmonth("2020-04"), revenue > 0) |> 
  summarize(rent = sum(revenue) / sum(R), .by = c(month, min_28, ward)) |> 
  # Check to make sure all wards have observations
  filter(n() == max(n()), .by = ward) |> 
  summarize(rent = mean(rent), .by = c(month, min_28)) |> 
  transmute(platform = "Airbnb/Vrbo", month, 
            type = if_else(min_28, "MTR", "STR"), rent) |> 
  bind_rows(rents)


# Figure 10 ---------------------------------------------------------------

fig_10 <- 
  rents |> 
  filter(year(month) != 2023) |> 
  mutate(label = if_else(month == yearmonth("2021-07"), type, NA_character_)) |>
  ggplot(aes(month, rent, colour = type)) +
  geom_line(lwd = 1) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  facet_wrap(~platform, scale = "free_y", nrow = 2) + 
  scale_y_continuous(name = NULL, label = scales::dollar) +
  scale_x_yearmonth(name = NULL) +
  scale_colour_manual(values = col_palette[c(2, 5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"),
        strip.text = element_text(face = "bold"))

ggsave("output/figure_10.png", plot = fig_10, width = 8, height = 5, 
       units = "in")

monthly |> 
  mutate(min_28 = minimum_stay >= 28) |> 
  filter(month >= yearmonth("2020-04"), revenue > 0) |> 
  summarize(rent = sum(revenue) / sum(R), .by = c(month, min_28, ward)) |> 
  # Check to make sure all wards have observations
  filter(n() == max(n()), .by = ward) |> 
  summarize(rent = mean(rent), .by = c(month, min_28)) |> 
  ggplot(aes(month, rent, colour = min_28)) +
  geom_line()


