#### LTR LISTING MATCH #########################################################

source("R/01_startup.R")
library(matchr)

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
qload("output/data/geometry.qsm")


# Load previous data ------------------------------------------------------

qload("output/data/matches_old.qsm", nthreads = availableCores())
qload("output/data/cl_changes.qsm", nthreads = availableCores())
qload("output/data/kj_changes.qsm", nthreads = availableCores())
ltr <- qread("output/data/ltr_raw.qs", nthreads = availableCores())
rm(ab_matches, cl_changes, kj_changes)
dl_location <- "/Volumes/Data 2/Scrape photos/toronto"


# Clean up matches --------------------------------------------------------

cl_matches <-
  cl_matches |> 
  filter(match) |> 
  mutate(
    x_name = str_replace_all(get_path(x_sig),
                             paste0(dl_location, "/ab/|-[:digit:]+.jpg"), ""),
    y_name = str_replace_all(get_path(y_sig),
                             paste0(dl_location, "/cl/|-[:digit:]+.jpg"), ""))

kj_matches <-
  kj_matches |> 
  filter(match) |> 
  mutate(
    x_name = str_replace_all(get_path(x_sig),
                             paste0(dl_location, "/ab/|-[:digit:]+.jpg"), ""),
    y_name = str_replace_all(get_path(y_sig),
                             paste0(dl_location, "/kj/|-[:digit:]+.jpg"), ""))

matches <-
  bind_rows(kj_matches, cl_matches) |> 
  select(property_ID = x_name, ltr_ID = y_name) |> 
  bind_rows(matches) |> 
  distinct()

# Make sure matches is in sync with property file
matches <-
  matches |> 
  filter(property_ID %in% property$property_ID)

rm(cl_matches, kj_matches)


# Connect STR and LTR listings --------------------------------------------

property_nest <-
  property |> 
  st_drop_geometry() |> 
  select(-any_of("ltr_ID")) |> 
  left_join(matches, by = "property_ID") |> 
  select(property_ID, ltr_ID)

property <-
  property_nest |> 
  summarize(ltr_ID = list(ltr_ID), .by = property_ID) |> 
  inner_join(select(property, -any_of("ltr_ID")), by = "property_ID") |> 
  relocate(ltr_ID, .before = geometry) |> 
  st_as_sf()

ltr_nest <-
  ltr |> 
  select(-any_of("property_ID")) |> 
  st_drop_geometry() |> 
  left_join(matches, by = c("id" = "ltr_ID"), relationship = "many-to-many") |> 
  select(id, property_ID)

ltr <-
  ltr_nest |> 
  summarize(property_ID = list(property_ID), .by = id) |> 
  inner_join(select(ltr, -any_of("property_ID")), by = "id") |> 
  relocate(property_ID, .before = geometry) |> 
  mutate(property_ID = map(property_ID, ~unique(unlist(.x)))) |> 
  st_as_sf()


# Save output -------------------------------------------------------------

qsave(property, file = "output/data/property.qs", nthreads = availableCores())
qsave(ltr, file = "output/data/ltr_processed.qs", nthreads = availableCores())
qsave(matches, file = "output/data/matches_processed.qs",
       nthreads = availableCores())
