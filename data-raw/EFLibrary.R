## code to prepare `EFLibrary` dataset goes here
library(devtools)
library(readxl)
EFLibrary <- read_excel("data-raw/openEFL.xlsx", sheet = "EFL")
EFLibrary[is.na(EFLibrary)] <- ""
usethis::use_data(EFLibrary, overwrite = TRUE)


