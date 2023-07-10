#### IMPORT STR DATA ###########################################################

source("R/01_startup.R")
qload("output/data/geometry.qsm")

# Add property file -------------------------------------------------------

property <- 
  read_csv("data/property.csv", show_col_types = FALSE) |> 
  select(property_ID = `Property ID`, listing_title = `Listing Title`, 
         property_type = `Property Type`, listing_type = `Listing Type`,
         created = `Created Date`, scraped = `Last Scraped Date`, 
         latitude = `Latitude`, longitude = `Longitude`, bedrooms = `Bedrooms`,
         city = `City`, minimum_stay = "Minimum Stay", 
         ab_property = `Airbnb Property ID`, ab_host = `Airbnb Host ID`, 
         ha_property = `HomeAway Property ID`, 
         ha_host = `HomeAway Property Manager ID`,
         img_url = `Listing Main Image URL`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-")) |>
  mutate(host_ID = coalesce(as.character(ab_host), ha_host),
         .after = property_ID) |> 
  strr_housing() |> 
  mutate(housing = if_else(property_type %in% c(
    "Home", "condo", "apartment", "house", "townhome", "cottage", "bungalow",
    "studio"), TRUE, housing))


# Do raffle and add wards -------------------------------------------------

property <- 
  property |> 
  strr_as_sf(32617) |> 
  # Remove extremely distant outliers
  # st_intersection(st_buffer(city, 1000)) |> 
  strr_raffle(DA, GeoUID, dwellings)

# Save progress
qsave(property, file = "output/data/property.qs", nthreads = availableCores())

property <-
  property |> 
  mutate(ward = WD$ward[st_nearest_feature(property, WD)], 
         .before = geometry) |> 
  rename(DA = GeoUID)

property <- 
  property |> 
  select(-.grid_ID, -DA) |> 
  rename(DA = GeoUID_new)


# Add monthly file --------------------------------------------------------

monthly_all <- 
  read_csv("data/monthly.csv", show_col_types = FALSE) |> 
  select(property_ID = `Property ID`, month = `Reporting Month`,
         rev = `Revenue (Native)`, r = `Reservation Days`,
         a = `Available Days`, b = `Blocked Days`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-"),
         month = tsibble::yearmonth(month)) |> 
  filter(month <= yearmonth("2023-05"))


# Process monthly ---------------------------------------------------------

monthly <- 
  monthly_all |> 
  filter(property_ID %in% property$property_ID[property$housing]) |> 
  # Hold on to old reservations for later adjusting revenue
  mutate(r_old = r)

# Start by removing all months that are < created, > scraped, or 0 days
monthly <- 
  monthly |> 
  left_join(select(st_drop_geometry(property), property_ID, created, scraped), 
            by = "property_ID") |> 
  filter(month >= yearmonth(created), month <= yearmonth(scraped),
         r + a + b > 0)

# Proportionally remove entries so r + a + b == scraped_days
scraped_change <- 
  monthly |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(scraped)) |> 
  mutate(scraped_days = day(scraped)) |> 
  mutate(across(c(r:b), \(x) x * scraped_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > scraped_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < scraped_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:scraped_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > scraped_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > scraped_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < scraped_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < scraped_days, a + 1, a)) |> 
  select(-scraped_days)

monthly <- 
  monthly |> 
  anti_join(scraped_change, by = c("property_ID", "month")) |> 
  bind_rows(scraped_change) |> 
  arrange(property_ID, month)

# Repeat for created
created_change <-
  monthly |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(created)) |> 
  mutate(created_days = days_in_month(
    month(as.Date(month))) - day(created) + 1) |> 
  mutate(across(c(r:b), \(x) x * created_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > created_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < created_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:created_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > created_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > created_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < created_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < created_days, a + 1, a)) |> 
  select(-created_days)

monthly <- 
  monthly |> 
  anti_join(created_change, by = c("property_ID", "month")) |> 
  bind_rows(created_change) |> 
  arrange(property_ID, month)

# Adjust revenue with r / r_old
monthly <- 
  monthly |> 
  mutate(rev = coalesce(rev * r / r_old, 0)) |> 
  select(-r_old, -created, -scraped)

monthly <- 
  monthly |> 
  left_join(select(st_drop_geometry(property), property_ID, host_ID, 
                   listing_type, DA, ward), by = "property_ID") |> 
  transmute(property_ID, month, R = r, A = a, B = b, revenue = rev, host_ID, 
            listing_type, DA, ward)


# Calculate multilistings -------------------------------------------------

monthly_host <- monthly
data.table::setDT(monthly_host)
monthly_host <- monthly_host[!is.na(host_ID), .(host_ID, month, listing_type)]
host <- monthly_host[, .(count = .N), by = .(host_ID, month, listing_type)]
host <- dplyr::as_tibble(host)
thresholds <- c(EH = 2L, PR = 3L, SR = NA, HR = NA)
thresholds_names <- names(thresholds)
thresholds <- if_else(thresholds == 0, NA_integer_, as.integer(thresholds))
col_names <- names(monthly)
data.table::setDT(monthly)
data.table::setDT(host)
EH <- thresholds[thresholds_names %in% c("EH", "Entire home/apt")]
PR <- thresholds[thresholds_names %in% c("PR", "Private room")]
SR <- thresholds[thresholds_names %in% c("SR", "Shared room")]
HR <- thresholds[thresholds_names %in% c("HR", "Hotel room")]
multi <- host[listing_type == "Entire home/apt" & count >= 
                EH][, `:=`(c(".ML", "count"), list(TRUE, NULL))]

multi <- data.table::rbindlist(list(multi, host[
  listing_type == "Private room" & count >= PR][
    , `:=`(c(".ML", "count"), list(TRUE, NULL))]))

join_cols <- setdiff(names(multi), ".ML")
multi <- multi[, .(.ML = sum(.ML)), by = c("host_ID", "month")][
  , `:=`(.ML, as.logical(.ML))]
join_cols <- setdiff(names(multi), c(".ML", "listing_type"))

monthly <- multi[monthly, on = join_cols][
  , `:=`(.ML, if_else(is.na(.ML), FALSE, .ML))]
monthly <- dplyr::as_tibble(monthly)

monthly <- 
  monthly |> 
  rename(multi = .ML) |> 
  relocate(multi, .after = listing_type) |> 
  relocate(property_ID, month) |> 
  relocate(host_ID, .after = revenue)



# Minimum stay ------------------------------------------------------------

min_stay_remote <- tbl(.con, "min_stay")

min_stay <- 
  min_stay_remote |> 
  filter(property_ID %in% !!property$property_ID) |> 
  collect() |> 
  arrange(property_ID, start_date)

qsave(min_stay, file = "output/data/min_stay.qs",
      nthreads = future::availableCores())

rm(min_stay_remote)

min_stay <- qread("output/data/min_stay.qs")

min_stay <-
  property |> 
  st_drop_geometry() |> 
  select(property_ID, start_date = scraped, minimum_stay) |> 
  mutate(start_date = min(as.Date("2023-05-31"), start_date)) |> 
  bind_rows(min_stay) |> 
  arrange(property_ID, start_date) |> 
  distinct()

min_stay <- 
  min_stay |> 
  mutate(month = yearmonth(start_date)) |> 
  filter(start_date == max(start_date), .by = c(property_ID, month)) |> 
  select(property_ID, month, minimum_stay)

min_stay_last <- 
  min_stay |> 
  filter(month == max(month), .by = property_ID) |> 
  select(property_ID, min_2 = minimum_stay)

monthly <- 
  monthly |> 
  left_join(min_stay, by = c("property_ID", "month")) |> 
  group_by(property_ID) |> 
  fill(minimum_stay, .direction = "downup") |> 
  ungroup() |> 
  left_join(min_stay_last, by = "property_ID") |> 
  mutate(minimum_stay = coalesce(minimum_stay, min_2, 1)) |> 
  select(-min_2)


# Save output -------------------------------------------------------------

qsave(property, file = "output/data/property.qs", nthreads = availableCores())
qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())

rm(created_change, host, monthly_all, monthly_host, multi, 
   scraped_change, col_names, EH, HR, join_cols, PR, SR, thresholds, 
   thresholds_names, min_stay, min_stay_last)
