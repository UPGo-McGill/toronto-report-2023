#### CHAPTER 2 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")


# Active listings and host revenue ----------------------------------------

# Active and blocked listings in May 2023
monthly |> 
  filter(month == yearmonth("2023-05"), minimum_stay < 28) |> 
  summarize(active = scales::comma(sum(R + A) / 31, 10),
            blocked = scales::comma(sum(B) / 31, 10))

# Active hosts
monthly |> 
  filter(month == yearmonth("2023-05"), minimum_stay < 28) |> 
  filter(R > 0 | A > 0) |> 
  count(host_ID) |> 
  nrow() |> 
  round(-1)

# 2022 rev
monthly |> 
  filter(year(month) == 2022, minimum_stay < 28) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(total_rev = sum(rev), mean_rev = mean(rev))

# 2019 active listings
monthly |> 
  filter(year(month) == 2019, minimum_stay < 28) |> 
  summarize(active = scales::comma(sum(R + A) / 365, accuracy = 10))

# 2019 rev
monthly |> 
  filter(year(month) == 2019, minimum_stay < 28) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(total_rev = sum(rev), mean_rev = mean(rev))

# 2020 active listings
monthly |> 
  filter(year(month) == 2020, minimum_stay < 28) |> 
  summarize(active = round(sum(R + A) / 366, -1))

# 2020 rev
monthly |> 
  filter(year(month) == 2020, minimum_stay < 28) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(total_rev = sum(rev), mean_rev = mean(rev))

# 2021 active listings
monthly |> 
  filter(year(month) == 2021, minimum_stay < 28) |> 
  summarize(active = sum(R + A) / 365)

# 2021 rev
monthly |> 
  filter(year(month) == 2021, minimum_stay < 28) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(total_rev = sum(rev), mean_rev = mean(rev))

# STR vs MTR rev in May 2023
monthly |> 
  filter(month == yearmonth("2023-05")) |> 
  group_by(min28 = minimum_stay >= 28) |> 
  summarize(rev = scales::dollar(sum(revenue)), .groups = "drop")

# Activity by ward
monthly |> 
  filter(month == yearmonth("2023-05"), minimum_stay < 28) |> 
  summarize(active = sum(R + A) / 31, .by = ward) |> 
  arrange(-active) |> 
  group_by(top_3 = ward %in% c("Spadina-Fort York", "University-Rosedale",
                               "Toronto Centre")) |> 
  summarize(active = sum(active)) |> 
  ungroup() |> 
  summarize(top_3 = active[top_3] / sum(active))
  

# Figure 1 ----------------------------------------------------------------

figure_1 <- 
  monthly |> 
  filter(month >= yearmonth("2017-01-01")) |> 
  group_by(month, MTR = minimum_stay >= 28) |> 
  summarize(active = sum(R + A), displayed = sum(R + A + B),
            .groups = "drop") |> 
  mutate(active = active / days_in_month(month),
         displayed = displayed / days_in_month(month)) |> 
  pivot_longer(cols = c(active, displayed)) |> 
  mutate(label = if_else(month == yearmonth("2019-07-01"), case_when(
    !MTR & name == "active" ~ "Active STR listings",
    MTR & name == "active" ~ "Active MTR listings",
    !MTR & name == "displayed" ~ "Displayed STR listings",
    MTR & name == "displayed" ~ "Displayed MTR listings"), 
    NA_character_)) |>
  ggplot(aes(month, value, linetype = name, colour = MTR)) +
  geom_vline(aes(xintercept = as.Date("2021-01-01")), linetype = 3) +
  annotate("text", x = as.Date("2021-02-15"), y = 16000, hjust = "left",
           label = "STR regulations \nintroduced",
           family = "Futura") +
  geom_line(lwd = 1.5) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_yearmonth(name = NULL) +
  scale_linetype_discrete(name = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_1.png", plot = figure_1, width = 8, height = 5, 
       units = "in")


# Figure 2 ----------------------------------------------------------------

figure_2 <- 
  monthly |> 
  group_by(month, MTR = minimum_stay >= 28) |> 
  summarize(active = sum(R + A), rev = sum(revenue), .groups = "drop") |> 
  mutate(active = active / days_in_month(month),
         rev = rev / days_in_month(month)) |> 
  mutate(active_growth = slide_dbl(active, \(x) (x[13] - x[1]) / x[1], 
                                   .before = 12, .complete = TRUE),
         rev_growth = slide_dbl(rev, \(x) (x[13] - x[1]) / x[1], .before = 12, 
                                .complete = TRUE), .by = MTR) |> 
  filter(month >= yearmonth("2018-06-01")) |> 
  select(-active, -rev) |> 
  pivot_longer(cols = c(active_growth, rev_growth)) |> 
  mutate(label = if_else(month == yearmonth("2022-04-01"), case_when(
    !MTR & name == "active_growth" ~ "Active STR listings",
    MTR & name == "active_growth" ~ "Active MTR listings",
    !MTR & name == "rev_growth" ~ "STR revenue",
    MTR & name == "rev_growth" ~ "MTR revenue"), 
    NA_character_)) |>
  ggplot(aes(month, value, linetype = name, colour = MTR)) +
  geom_line(lwd = 1.5) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_yearmonth(name = NULL) +
  scale_linetype_discrete(name = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)]) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_2.png", plot = figure_2, width = 8, height = 5, 
       units = "in")


# Figure 3 ----------------------------------------------------------------

fig_3_1 <- 
  monthly |> 
  filter(month == yearmonth("2023-05"), minimum_stay < 28) |> 
  summarize(active = sum(R + A) / 31, .by = ward) |> 
  left_join(WD, by = "ward") |> 
  st_as_sf() |> 
  mutate(act_per_dwelling = active / dwellings) |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = act_per_dwelling), colour = "white") +
  scale_fill_stepsn(colors = col_palette[c(6, 2, 1)], na.value = "grey80",
                    limits = c(0, 0.015), oob = scales::squish, 
                    breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015), 
                    labels = scales::percent)  +
  gg_bbox(city) +
  guides(fill = guide_coloursteps(title = "STRs/dwelling", title.vjust = 0.8)) +
  theme_void() +
  theme(text = element_text(family = "Futura"))

fig_3_2 <- 
  monthly |> 
  filter(month == yearmonth("2023-05"), minimum_stay < 28) |> 
  summarize(active = sum(R + A) / 31, .by = DA) |> 
  left_join(DA, by = c("DA" = "GeoUID")) |> 
  st_as_sf() |> 
  mutate(act_per_dwelling = active / dwellings) |> 
  ggplot() +
  geom_sf(data = CMA, fill = "grey80", colour = "transparent") +
  geom_sf(data = city, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = act_per_dwelling), colour = "transparent") +
  scale_fill_stepsn(colors = col_palette[c(6, 2, 1)], na.value = "grey80",
                    limits = c(0, 0.015), oob = scales::squish, 
                    breaks = c(0, 0.003, 0.006, 0.009, 0.012, 0.015), 
                    labels = scales::percent)  +
  gg_bbox(city) +
  guides(fill = guide_coloursteps(title = "STRs/dwelling", title.vjust = 0.8)) +
  theme_void() +
  theme(text = element_text(family = "Futura"))

figure_3 <- 
  fig_3_1 + fig_3_2 + plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom", legend.key.width = unit(1.8, "lines"))

ggsave("output/figure_3.png", plot = figure_3, width = 8, 
       height = 4.2, units = "in")


# Table 1 -----------------------------------------------------------------

table_1 <- 
  monthly |> 
  filter(month(month) == 5, year(month) %in% 2022:2023, minimum_stay < 28) |> 
  summarize(
    active = sum(R[year(month) == 2023] + A[year(month) == 2023]) / 31,
    active_2022 = sum(R[year(month) == 2022] + A[year(month) == 2022]) / 31,
    active_growth = (active - active_2022) / active_2022,
    rev = sum(revenue[year(month) == 2023]),
    rev_2022 = sum(revenue[year(month) == 2022]),
    rev_growth = (rev - rev_2022) / rev_2022,
    .by = ward) |> 
  left_join(WD, by = "ward") |> 
  filter(active >= 80) |> 
  arrange(-active) |> 
  mutate(active_per_dwelling = active / dwellings) |> 
  select(ward, active, active_growth, active_per_dwelling, rev, rev_growth) |> 
  mutate(active = scales::comma(active, 10),
         active_growth = scales::percent(active_growth, 0.1),
         active_per_dwelling = scales::percent(active_per_dwelling, 0.1),
         rev = scales::dollar(rev, 10, scale = 1/1000, suffix = "k"),
         rev_growth = scales::percent(rev_growth, 0.1))

monthly |> 
  filter(month(month) == 5, year(month) %in% 2022:2023, minimum_stay < 28) |> 
  summarize(
    active = sum(R[year(month) == 2023] + A[year(month) == 2023]) / 31,
    active_2022 = sum(R[year(month) == 2022] + A[year(month) == 2022]) / 31,
    active_growth = (active - active_2022) / active_2022,
    rev = sum(revenue[year(month) == 2023]),
    rev_2022 = sum(revenue[year(month) == 2022]),
    rev_growth = (rev - rev_2022) / rev_2022,
    .by = ward) |> 
  left_join(WD, by = "ward") |> 
  summarize(
    active = sum(active),
    active_2022 = sum(active_2022),
    active_growth = (active - active_2022) / active_2022,
    rev = sum(rev),
    rev_2022 = sum(rev_2022),
    rev_growth = (rev - rev_2022) / rev_2022,
    dwellings = sum(dwellings)) |> 
  mutate(ward = "City of Toronto", active_per_dwelling = active / dwellings) |> 
  select(ward, active, active_growth, active_per_dwelling, rev, rev_growth) |> 
  mutate(active = scales::comma(active, 10),
         active_growth = scales::percent(active_growth, 0.1),
         active_per_dwelling = scales::percent(active_per_dwelling, 0.1),
         rev = scales::dollar(rev, 10, scale = 1/1000, suffix = "k"),
         rev_growth = scales::percent(rev_growth, 0.1)) |> 
  bind_rows(table_1) |> 
  gt::gt()


# Home sharers and commercial operators -----------------------------------

host_rev <-
  monthly |> 
  filter(year(month) == 2023, !is.na(host_ID), minimum_stay < 28, R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID)

# Median 2023 rev
median(host_rev$rev)
  
# > $100k in 2023
host_rev |> 
  filter(rev >= 100000)

# > $100k in 2022
monthly |> 
  filter(year(month) == 2022, month(month) <= 5, !is.na(host_ID), 
         minimum_stay < 28, R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  filter(rev >= 100000)

# > $100k in 2019
monthly |> 
  filter(year(month) == 2019, month(month) <= 5, !is.na(host_ID), 
         minimum_stay < 28, R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  filter(rev >= 100000)

# Total host rev in 2023
sum(host_rev$rev)

# Top 1% in 2019
monthly |> 
  filter(year(month) == 2019, month(month) <= 5, !is.na(host_ID), 
         minimum_stay < 28, R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_11 = sum(rev[rev > quantile(rev, c(0.99))] / all))

# Top 1% in 2023
monthly |> 
  filter(year(month) == 2023, month(month) <= 5, !is.na(host_ID), 
         minimum_stay < 28, R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_11 = sum(rev[rev > quantile(rev, c(0.99))] / all))

# ML in May 2023
monthly |> 
  filter(minimum_stay < 28, month == yearmonth("2023-05")) |> 
  summarize(active = sum(R + A), revenue = sum(revenue), .by = multi) |> 
  summarize(active = active[multi] / sum(active),
            rev = revenue[multi] / sum(revenue))


# Figure 4 ----------------------------------------------------------------

revenue_colour <- colorRampPalette(col_palette[c(5, 4, 3, 6, 2, 1)])(10)

host_deciles <-
  monthly |> 
  filter(year(month) %in% c(2019, 2023), month(month) <= 5, 
         minimum_stay < 28, R > 0) |> 
  mutate(year = year(month)) |> 
  summarize(rev = sum(revenue), .by = c(host_ID, year)) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all),
            .by = year) |> 
  select(-all) |> 
  pivot_longer(-year, names_to = "percentile", values_to = "value") |> 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) |> 
  mutate(perfect_distribution = 0.1,
         decile = rep(1:10, 2),
         dummy_1 = perfect_distribution,
         dummy_2 = value) |> 
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) |> 
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") |> 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) |> 
  group_by(year, position) |> 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) |> 
  ungroup() |> 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

figure_4 <-
  host_deciles |> 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  geom_text(aes(x = 0.02, y = absolute_val, label = display_percentile), 
            data = filter(host_deciles, position == 0, decile <= 2),
            family = "Futura", size = 2.5, hjust = 0) +
  geom_text(aes(x = 0.98, y = absolute_val, label = display_val), 
            data = filter(host_deciles, position == 1, decile <= 2),
            family = "Futura", size = 2.5, hjust = 1) +
  facet_wrap(~year) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, NA),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        strip.text = element_text(family = "Futura", face = "bold"))

ggsave("output/figure_4.png", plot = figure_4, width = 8, height = 4, 
       units = "in")


# Figure 5 ----------------------------------------------------------------

figure_5 <-
  monthly |> 
  filter(minimum_stay < 28, year(month) >= 2017) |> 
  summarize(active = sum(R + A), revenue = sum(revenue), 
            .by = c(month, multi)) |> 
  mutate(active = active / days_in_month(month)) |> 
  summarize(active = active[multi] / sum(active),
            rev = revenue[multi] / sum(revenue),
            .by = month) |> 
  pivot_longer(c(active, rev)) |> 
  mutate(label = if_else(month == yearmonth("2018-01-01"), case_when(
    name == "active" ~ "Active STR listings",
    name == "rev" ~ "STR revenue"), NA_character_)) |>
  ggplot(aes(month, value, colour = name)) +
  geom_line(lwd = 1.5) +
  geom_label(aes(label = label), alpha = 0.9, family = "Futura", size = 3) +
  scale_y_continuous(name = NULL, 
                     label = scales::percent_format(accuracy = 1)) +
  scale_x_yearmonth(name = NULL) +
  scale_colour_manual(values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figure_5.png", plot = figure_5, width = 8, height = 5, 
       units = "in")

