
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