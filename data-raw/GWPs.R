## code to prepare `GWPs` dataset goes here
library(devtools)
library(data.table)
GWPs <- fread("data-raw/GWPs.csv")
GWPs[is.na(GWPs)] <- ""
usethis::use_data(GWPs, overwrite = TRUE)

