## code to prepare `Ecat_lookup` dataset goes here
library(devtools)
library(data.table)
Ecat_lookup <- fread("data-raw/Ecat_lookup.csv")
Ecat_lookup[is.na(Ecat_lookup)] <- ""
usethis::use_data(Ecat_lookup, overwrite = TRUE)
