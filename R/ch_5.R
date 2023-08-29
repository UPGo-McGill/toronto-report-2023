#### CHAPTER 5 #################################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")
license <- qread("output/data/airbnb_licenses.qs")
reg_city <- qread("output/data/reg_city.qs")


# STR regulatory compliance since 2021 ------------------------------------

# Listings active and scraped
property |> 
  filter(exists) |> 
  nrow() |> 
  scales::comma()

# Registration proportions
property |> 
  st_drop_geometry() |> 
  filter(exists) |> 
  mutate(status = case_when(
    reg_status == "VALID" ~ "Valid license",
    reg_status %in% c("HOTEL", "NON-HOUSING EXEMPT", "OTHER EXEMPT") ~ 
      "Exempt (hotel, etc.)",
    reg_status %in% c("LTR", "LTR EXEMPT") ~ "Exempt (MTR)",
    reg_status == "INVALID" ~ "Invalid license")) |> 
  count(status) |> 
  mutate(pct = n / sum(n))

# Valid listings located more than 0.5 km from property address
prop_distant <- 
  property |>
  filter(exists) |>
  filter(reg_status == "VALID") |>
  inner_join(
    reg_city |> 
      select(license, lon, lat) |> 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
      st_transform(32617) |> 
      as_tibble() |> 
      rename(reg_geom = geometry), by = "license") |> 
  mutate(dist = as.numeric(st_distance(geometry, reg_geom, 
                                       by_element = TRUE))) |> 
  filter(dist >= 500) |> 
  pull(property_ID)

# Properties with more than 3 listings sharing a license
prop_multiple <- 
  property |> 
  filter(exists) |>
  filter(reg_status == "VALID") |> 
  filter(n() > 3, .by = license) |> 
  pull(property_ID)

# Properties which are high probability FREH
prop_FREH <- 
  monthly |> 
  filter(property_ID %in% (property |> 
           filter(exists) |>
           filter(reg_status == "VALID") |>
           pull(property_ID)),
         year(month) == 2023) |> 
  summarize(FREH_avg = mean(FREH_3), .by = property_ID) |> 
  filter(FREH_avg > 0.6) |> 
  pull(property_ID)

# Total number of suspect listings
property |> 
  filter(exists) |>
  filter(reg_status == "VALID") |> 
  st_drop_geometry() |> 
  transmute(
    property_ID,
    license,
    distant = property_ID %in% prop_distant,
    multiple = property_ID %in% prop_multiple,
    FREH = property_ID %in% prop_FREH) |> 
  summarize(
    n_distant = sum(distant),
    pct_distant = mean(distant),
    n_multiple = sum(multiple),
    pct_multiple = mean(multiple),
    n_FREH = sum(FREH),
    pct_FREH = mean(FREH),
    n_any = sum(distant + multiple + FREH > 0),
    pct_any = mean(distant + multiple + FREH > 0)
  )


# Figure 12 ---------------------------------------------------------------

fig_12 <-
  property |> 
  st_drop_geometry() |> 
  filter(exists) |> 
  mutate(status = case_when(
    reg_status == "VALID" ~ "Valid license",
    reg_status %in% c("HOTEL", "NON-HOUSING EXEMPT", "OTHER EXEMPT") ~ 
      "Exempt (hotel, etc.)",
    reg_status %in% c("LTR", "LTR EXEMPT") ~ "Exempt (MTR)",
    reg_status == "INVALID" ~ "Invalid license")) |> 
  count(status) |> 
  mutate(n = round(n / 103)) |> 
  reframe(status = rep(status, n)) |> 
  waffle_iron(aes_d(group = status), rows = 12) |> 
  ggplot() +
  geom_waffle(aes(x, y, fill = group)) +
  coord_equal() +
  scale_fill_manual(name = "Registration status",
                    values = col_palette[c(1, 2, 5, 6)]) +
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figure_12.png", plot = fig_12, width = 8, height = 4, 
       units = "in")


# Figure 13 ---------------------------------------------------------------

fig_13 <-
  property |> 
  st_drop_geometry() |> 
  filter(reg_status == "VALID") |> 
  mutate(status = case_when(
    property_ID %in% prop_distant + property_ID %in% prop_multiple + 
      property_ID %in% prop_FREH > 1 ~ "Several problems",
    property_ID %in% prop_distant ~ "Location mismatch",
    property_ID %in% prop_multiple ~ "> 3 listings",
    property_ID %in% prop_FREH ~ "Not principal res.",
    .default = "Valid")) |> 
  count(status) |>
  mutate(n = round(n / 22.8)) |> 
  reframe(status = rep(status, n)) |> 
  waffle_iron(aes_d(group = status), rows = 12) |> 
  ggplot() +
  geom_waffle(aes(x, y, fill = group)) +
  coord_equal() +
  scale_fill_manual(name = "Registration status",
                    values = col_palette[c(1, 2, 4, 5, 6)]) +
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figure_13.png", plot = fig_13, width = 8, height = 4, 
       units = "in")


# Table 4 ----------------------------------------------------------------

property |> 
  st_drop_geometry() |> 
  filter(exists) |> 
  mutate(status = case_when(
    property_ID %in% c(prop_distant, prop_multiple, prop_FREH) ~ 
      "Potential violation",
    reg_status == "VALID" ~ "Valid license",
    reg_status %in% c("HOTEL", "NON-HOUSING EXEMPT", "OTHER EXEMPT") ~ 
      "Exempt (hotel, etc.)",
    reg_status %in% c("LTR", "LTR EXEMPT") ~ "Exempt (MTR)",
    reg_status == "INVALID" ~ "Invalid license")) |> 
  inner_join(
    monthly |> 
      filter(year(month) == 2023) |> 
      summarize(
        n_res = sum(R),
        n_active = sum(A + R),
        rev = sum(revenue),
        .by = property_ID), by = "property_ID") |> 
  summarize(
    n_listings = n(),
    pct_eh = mean(listing_type == "Entire home/apt"),
    mean_res = mean(n_res),
    mean_active = mean(n_active),
    mean_rev_per_night = sum(rev) / sum(n_res),
    .by = status) |> 
  mutate(n_listings = scales::comma(n_listings),
         pct_eh = scales::percent(pct_eh, accuracy = 0.1),
         mean_res = scales::comma(mean_res, accuracy = 0.1),
         mean_active = scales::comma(mean_active, accuracy = 0.1),
         mean_rev_per_night = scales::dollar(mean_rev_per_night, 
                                             accuracy = 1)) |> 
  gt::gt()


