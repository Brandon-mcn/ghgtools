## code to prepare `GWPs` dataset goes here
library(devtools)
library(readxl)
GWPs <- read_excel("data-raw/openEFL.xlsx", sheet = "GWPs")
GWPs[is.na(GWPs)] <- ""
usethis::use_data(GWPs, overwrite = TRUE)

