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
qload("output/data/img_sigs.qsm", nthreads = availableCores())


download_images(property,
                "/Volumes/Data 2/Scrape photos/toronto/ab",
                path = img_url,
                id = property_ID)

get_path(ab_sigs) <- str_replace(get_path(ab_sigs), "img/", 
                                 "/Volumes/Data 2/Scrape photos/toronto/")


# Match signatures --------------------------------------------------------

# CL
cl_matrix <- match_signatures(ab_sigs[1:20000], cl_sigs)
cl_matches_1 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, file = "~/Desktop/cl_matches.qsm", nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[20001:40000], cl_sigs)
cl_matches_2 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, file = "~/Desktop/cl_matches.qsm", 
       nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[40001:60000], cl_sigs)
cl_matches_3 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, 
       file = "~/Desktop/cl_matches.qsm", nthreads = 32)
       
cl_matrix <- match_signatures(ab_sigs[60001:80000], cl_sigs)
cl_matches_4 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4, 
       file = "~/Desktop/cl_matches.qsm", nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[80001:100000], cl_sigs)
cl_matches_5 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4, cl_matches_5, 
       file = "~/Desktop/cl_matches.qsm", nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[100001:115416], cl_sigs)
cl_matches_6 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4, cl_matches_5, 
       cl_matches_6, file = "~/Desktop/cl_matches.qsm", nthreads = 32)

cl_matches <- bind_rows(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4,
                        cl_matches_5, cl_matches_6)
qsave(cl_matches, "~/Desktop/cl_matches.qs")

# KJ
kj_matrix <- match_signatures(ab_sigs[1:20000], kj_sigs)
kj_matches_1 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, file = "~/Desktop/kj_matches.qsm", nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[20001:40000], kj_sigs)
kj_matches_2 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, file = "~/Desktop/kj_matches.qsm", 
       nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[40001:60000], kj_sigs)
kj_matches_3 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, 
       file = "~/Desktop/kj_matches.qsm", nthreads = 32)


kj_matrix <- match_signatures(ab_sigs[60001:80000], kj_sigs)
kj_matches_4 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4, 
       file = "~/Desktop/kj_matches.qsm", nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[80001:100000], kj_sigs)
kj_matches_5 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4, kj_matches_5, 
       file = "~/Desktop/kj_matches.qsm", nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[100001:115416], kj_sigs)
kj_matches_6 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4, kj_matches_5, 
       kj_matches_6, file = "~/Desktop/kj_matches.qsm", nthreads = 32)

kj_matches <- bind_rows(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4,
                        kj_matches_5, kj_matches_6)
qsave(kj_matches, "~/Desktop/kj_matches.qs")

cl_changes <- confirm_matches(cl_matches)
cl_updated <- integrate_changes(cl_matches, cl_changes)
cl_changes <- confirm_matches(cl_updated)
cl_updated <- integrate_changes(cl_matches, cl_changes)

qsavem(cl_changes, cl_updated, file = "~/Desktop/cl_changes.qsm")



kj_changes <- confirm_matches(kj_matches)
kj_updated <- integrate_changes(kj_matches, kj_changes)
kj_changes <- confirm_matches(kj_updated)
kj_updated <- integrate_changes(kj_matches, kj_changes)

qsavem(kj_changes, kj_updated, file = "~/Desktop/kj_changes.qsm")

