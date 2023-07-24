### IMAGE MATCHING #############################################################

source("R/01_startup.R")

property <- qread("output/data/property.qs", nthreads = availableCores())


# Get Airbnb images -------------------------------------------------------

# Get AB urls
ab_urls <-
  property |> 
  pull(img_url) |> 
  str_replace('(?<=(jpg|JPEG|jpeg|JPG)).*', '')

# Get AB IDs
ab_ids <- 
  property |> 
  pull(property_ID)

matchr::download_images(
  destination = "/Volumes/Data 2/Scrape photos/toronto/ab",
  path = ab_urls,
  id = ab_ids)


# Get signatures ----------------------------------------------------------

ab_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/ab", 
                       full.names = TRUE)
cl_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/cl", 
                       full.names = TRUE)
kj_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/kj", 
                       full.names = TRUE)

ab_sigs <- create_signature(ab_paths)
qsavem(ab_sigs, file = "output/data/img_sigs.qsm",
       nthreads = availableCores())

cl_sigs <- create_signature(cl_paths)
qsavem(ab_sigs, cl_sigs, file = "output/data/img_sigs.qsm",
       nthreads = availableCores())

kj_sigs <- create_signature(kj_paths)
qsavem(ab_sigs, cl_sigs, kj_sigs, file = "output/data/img_sigs.qsm",
       nthreads = availableCores())


# Match signatures --------------------------------------------------------

# CL
cl_matrix <- match_signatures(ab_sigs[1:30000], cl_sigs)
cl_matches_1 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, file = "output/data/cl_matches.qsm", nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[30001:60000], cl_sigs)
cl_matches_2 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, file = "output/data/cl_matches.qsm", 
       nthreads = 32)

cl_matrix <- match_signatures(ab_sigs[60001:90000], cl_sigs)
cl_matches_3 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, 
       file = "output/data/cl_matches.qsm", nthreads = 32)
       
cl_matrix <- match_signatures(ab_sigs[90001:115416], cl_sigs)
cl_matches_4 <- identify_matches(cl_matrix)
qsavem(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4, 
       file = "output/data/cl_matches.qsm", nthreads = 32)

cl_matches <- bind_rows(cl_matches_1, cl_matches_2, cl_matches_3, cl_matches_4)
qsave(cl_matches, "output/data/cl_matches.qs")

# KJ
kj_matrix <- match_signatures(ab_sigs[1:30000], kj_sigs)
kj_matches_1 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, file = "output/data/kj_matches.qsm", nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[30001:60000], kj_sigs)
kj_matches_2 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, file = "output/data/kj_matches.qsm", 
       nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[60001:90000], kj_sigs)
kj_matches_3 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, 
       file = "output/data/kj_matches.qsm", nthreads = 32)

kj_matrix <- match_signatures(ab_sigs[90001:115416], kj_sigs)
kj_matches_6 <- identify_matches(kj_matrix)
qsavem(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4, 
       file = "output/data/kj_matches.qsm", nthreads = 32)

kj_matches <- bind_rows(kj_matches_1, kj_matches_2, kj_matches_3, kj_matches_4)
qsave(kj_matches, "output/data/kj_matches.qs")


# Confirm with Shiny app --------------------------------------------------

cl_changes <- confirm_matches(cl_matches)
cl_matches <- integrate_changes(cl_matches, cl_changes)
qsavem(cl_changes, cl_matches, file = "output/data/cl_changes.qsm")

kj_changes <- confirm_matches(kj_matches)
kj_matches <- integrate_changes(kj_matches, kj_changes)
qsavem(kj_changes, kj_matches, file = "output/data/kj_changes.qsm")

