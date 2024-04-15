## code to prepare `eGRIDlookup` dataset goes here
library(devtools)
library(data.table)
eGRIDlookup <- fread("data-raw/eGRIDlookup.csv")
eGRIDlookup[is.na(eGRIDlookup)] <- ""
usethis::use_data(eGRIDlookup, overwrite = TRUE)
