## code to prepare `eGRIDlookup` dataset goes here
library(devtools)
library(readxl)
eGRIDlookup <- read_excel("data-raw/openEFL.xlsx", sheet = "egrid_lookup")
eGRIDlookup[is.na(eGRIDlookup)] <- ""
usethis::use_data(eGRIDlookup, overwrite = TRUE)
