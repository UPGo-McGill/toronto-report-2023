#### CHAPTER 3 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")




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

