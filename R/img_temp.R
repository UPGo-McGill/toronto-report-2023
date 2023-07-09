### IMG TEMP

library(matchr)
progressr::handlers(global = TRUE)
library(future)
plan(multisession)

cl_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/cl", 
                       full.names = TRUE)
kj_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/kj", 
                       full.names = TRUE)


# Get signatures ----------------------------------------------------------

cl_sigs <- create_signature(cl_paths)
qsave(cl_sigs, file = "cl_sigs.qs", nthreads = availableCores())

kj_sigs <- create_signature(kj_paths)
qsave(kj_sigs, file = "kj_sigs.qs", nthreads = availableCores())