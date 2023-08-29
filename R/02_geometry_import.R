#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)


# Toronto CMA without Toronto ---------------------------------------------

CMA <-
  get_census(
    dataset = "CA21", regions = list(CMA = "35535"), level = "CSD",
    geo_format = "sf") %>%
  filter(name != "Toronto (C)") %>%
  st_transform(32617)


# Toronto CSD -------------------------------------------------------------

city <-
  get_census(
    dataset = "CA21", regions = list(CSD = "3520005"), geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# Toronto DAs -------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA21", regions = list(CSD = "3520005"), level = "DA",
    geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# Toronto neighbourhoods --------------------------------------------------

WD <- 
  read_sf("data/wards/City Wards Data - 4326.shp") |> 
  select(ward = AREA_NA13) |> 
  st_set_agr("constant") |> 
  st_transform(32617) 

WD <-
  DA |> 
  select(dwellings) |> 
  st_interpolate_aw(WD, extensive = TRUE) |> 
  st_drop_geometry() |> 
  (\(x) cbind(WD, x))() |> 
  arrange(ward) |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_set_agr("constant")

WD <-
  WD |> 
  st_intersection(city$geometry)


# Water -------------------------------------------------------------------

water <-
  read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |> 
  as_tibble() |> 
  st_as_sf() |> 
  filter(PRUID == "35") |> 
  st_transform(32617) |> 
  st_filter(CMA)


# Save output -------------------------------------------------------------

qsavem(city, CMA, DA, WD, file = "output/data/geometry.qsm")
qsave(water, file = "output/data/water.qs")
