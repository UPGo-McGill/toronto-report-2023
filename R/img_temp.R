### IMG TEMP

library(matchr)
handlers(global = TRUE)

cl_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/cl", 
                       full.names = TRUE)
kj_paths <- list.files("/Volumes/Data 2/Scrape photos/toronto/kj", 
                       full.names = TRUE)


# Get signatures ----------------------------------------------------------

cl_sigs <- create_signature(cl_paths)
kj_sigs <- create_signature(kj_paths)

qsavem(cl_sigs, kj_sigs, file = "output/img_sigs.qsm",
       nthreads = availableCores())
