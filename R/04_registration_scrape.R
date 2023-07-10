#### 08 REGISTRATION PROCESSING ################################################

source("R/01_startup.R")
library(rvest)
library(httr)


# Load previous data ------------------------------------------------------

property <- qread("output/data/property.qs", nthreads = availableCores())


# Get all the property IDs ------------------------------------------------

PIDs <-
  property |>
  filter(scraped > "2023-03-01") |>
  filter(!is.na(ab_property)) |>
  pull(property_ID) |>
  str_remove("^ab-")


# Prepare the tibble ------------------------------------------------------

# airbnb_licenses <- qread("output/data/airbnb_licenses.qs")
airbnb_licenses <-
  tibble(property_ID = PIDs,
         license = character(length(PIDs)),
         exempt = logical(length(PIDs)),
         exists = NA,
         double_check = FALSE)


# Prepare proxies ---------------------------------------------------------

proxy_list <- read_lines(.proxy_URL)


# Loop over property IDs --------------------------------------------------

for (i in seq_along(airbnb_licenses$property_ID)) {

  if (!is.na(airbnb_licenses$exists[[i]])) {
    
    # Skip listings which were double-checked
    if (airbnb_licenses$double_check[[i]]) next
    
    # Skip listings which weren't double-checked but have a license
    if (!is.na(airbnb_licenses$license[[i]])) next
    
    # Set double_check flag
    airbnb_licenses$double_check[[i]] <- TRUE
    
  }

  print(i)

  proxy <- 
    proxy_list |> 
    sample(1) |> 
    str_split(":") |> 
    pluck(1)
  
  pg <- tryCatch(
    GET(paste0("https://www.airbnb.ca/rooms/", 
               airbnb_licenses$property_ID[[i]]),
        use_proxy(proxy[1], as.numeric(proxy[2]), proxy[3], proxy[4])), 
    error = function(e) {
      Sys.sleep(2)
      tryCatch(
        GET(paste0("https://www.airbnb.ca/rooms/", 
                   airbnb_licenses$property_ID[[i]]),
            use_proxy(proxy[1], as.numeric(proxy[2]), proxy[3], proxy[4])), 
        error = function(e) NULL)})

  if (is.null(pg)) {
    airbnb_licenses$double_check[[i]] <- FALSE
    stop("NULL result")
  } else if (pg$status_code == 410) {
    desc <- NA_character_
  } else if (pg$status_code == 200) {
    desc <-
      pg |> 
      read_html() |> 
      html_element("body") |> 
      html_element("#data-state") |> 
      html_text2() |> 
      str_extract("STR-\\d{4}-\\w{6}")
    if (is.na(desc)) {
      desc <- 
        pg |> 
        read_html() |> 
        html_element("body") |> 
        html_element("#data-state") |> 
        html_text2() |> 
        str_extract("Exempt")
    }
    if (is.na(desc)) {
      desc <- 
        pg |> 
        read_html() |> 
        html_element("body") |> 
        html_element("#data-state") |> 
        html_text2() |> 
        str_extract("Approved by government")
    }
  } else stop(paste0("STATUS CODE ", pg$status_code, "; PROXY ", proxy[1]))

  # Add rows to the output df
  airbnb_licenses$license[[i]] <- desc
  airbnb_licenses$exists[[i]] <- pg$status_code == 200
  airbnb_licenses$exempt[[i]] <- desc == "Exempt"

  # Save the dataframe every iteration
  qs::qsave(airbnb_licenses, "output/data/airbnb_licenses.qs")
}

rm(pg, PIDs, desc, i, proxy, proxy_list)


# Extract license and join to property file -------------------------------

airbnb_licenses <- qread("output/airbnb_licenses.qs")

airbnb_licenses <-
  airbnb_licenses |> 
  mutate(property_ID = paste0("ab-", property_ID)) |> 
  select(property_ID, exists, license, exempt)

property <- 
  property |> 
  select(-any_of(c("exists", "license", "exempt", "reg_status", "type",
                   "address"))) |> 
  left_join(airbnb_licenses, by = "property_ID") |> 
  relocate(geometry, .after = last_col())


# Import City registration data -------------------------------------------

reg_city <-
  read_csv("data/short-term-rental-registrations-data.csv") |> 
  select(-`_id`, -ward_number) |> 
  set_names(c("license", "reg_postal", "reg_ward"))


# Merge City data with scrape results -------------------------------------

property <- 
  property |> 
  select(-any_of(c("reg_postal", "reg_ward"))) |> 
  left_join(reg_city, by = "license") |> 
  relocate(geometry, .after = last_col())


# Double check strange results --------------------------------------------

property <- 
  property |>
  arrange(property_ID) |> 
  mutate(minimum_stay = case_when(
    property_ID == "ab-6644497" ~ 366,
    property_ID == "ab-6748289" ~ 366,
    property_ID == "ab-19811629" ~ 366,
    property_ID == "ab-22363032" ~ 500,
    property_ID == "ab-17220189" ~ 1000,
    property_ID == "ab-7253069" ~ 1000,
    property_ID == "ab-6603272" ~ 1124,
    property_ID == "ab-8001458" ~ 1125,
    .default = minimum_stay))

# Find listings which are actually not in Toronto
at_border <- 
  property |> 
  filter(is.na(license) & minimum_stay < 28 & exists) |> 
  st_filter(st_buffer(st_cast(city, "MULTILINESTRING"), 200)) |> 
  pull(property_ID)

at_border <- 
  property |> 
  filter(is.na(license) & minimum_stay < 28 & exists) |> 
  filter(!property_ID %in% at_border, !listing_type == "Hotel room") |> 
  st_filter(city, .predicate = st_disjoint) |> 
  pull(property_ID) |> 
  c(at_border)

property <- 
  property |> 
  filter(!property_ID %in% at_border)

property |> 
  filter(!is.na(license)) |> 
  select(property_ID, license:reg_ward)

property <-
  property |> 
  mutate(license = if_else(exempt, NA_character_, license)) |> 
  mutate(reg_status = case_when(
    is.na(exists) | !exists ~ NA_character_,
    exempt & minimum_stay >= 30 ~ "LTR EXEMPT",
    exempt & !housing ~ "NON-HOUSING EXEMPT",
    exempt ~ "OTHER EXEMPT",
    listing_type == "Hotel room" | str_detect(property_type, "otel") ~ "HOTEL",
    minimum_stay >= 30 ~ "LTR",
    is.na(license) & minimum_stay >= 28 ~ "MISSING 28",
    is.na(license) ~ "MISSING STR",
    !is.na(license) & is.na(reg_postal) ~ "INVALID",
    !is.na(license) ~ "VALID",
    .default = "TKTK"), .before = geometry)
  

# Harmonize monthly with property -----------------------------------------

monthly <- 
  monthly |> 
  filter(property_ID %in% property$property_ID)


# Save output -------------------------------------------------------------

qsave(property, file = "output/data/property.qs", nthreads = availableCores())
qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())
qsave(reg_city, file = "output/data/reg_city.qs")

rm(airbnb_licenses)
