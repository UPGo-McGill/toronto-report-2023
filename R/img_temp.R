### IMG TEMP

library(tidyverse)
library(qs)
library(matchr)
progressr::handlers(global = TRUE)
library(future)
plan(multisession)
library(sf)

property <- qread("output/data/property.qs", nthreads = availableCores())

cl_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/cl", 
                       full.names = TRUE)
kj_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/kj", 
                       full.names = TRUE)


# Get signatures ----------------------------------------------------------


cl_sigs <- create_signature(cl_paths)
qsave(cl_sigs, file = "cl_sigs.qs", nthreads = availableCores())
cl_sigs <- qread("cl_sigs.qs")

kj_sigs <- create_signature(kj_paths)
qs::qsave(kj_sigs, file = "kj_sigs.qs", nthreads = availableCores())
file.copy("kj_sigs.qs", "~/Desktop")

qsavem(ab_sigs, cl_sigs, kj_sigs, file = "output/data/img_sigs.qsm",
       nthreads = availableCores())
qload("~/Desktop/img_sigs.qsm")


download_images(property,
                "/Volumes/Data 2/Scrape photos/toronto/ab",
                path = img_url,
                id = property_ID)

get_path(ab_sigs) <- str_replace(get_path(ab_sigs), "img/", "/Volumes/Data 2/Scrape photos/toronto/")


# Match signatures --------------------------------------------------------

cl_match <- match_signatures(ab_sigs, cl_sigs)
kj_match <- match_signatures(ab_sigs, kj_sigs)

cl_matches <- identify_matches(cl_match)
kj_matches <- identify_matches(kj_match)

qsave(cl_matches, "~/Desktop/cl_matches.qs")
qsave(kj_matches, "~/Desktop/kj_matches.qs")
