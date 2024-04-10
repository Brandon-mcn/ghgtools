## code to prepare `EFLibrary` dataset goes here
library(devtools)
library(data.table)
EFLibrary <- fread("data-raw/EFLibrary.csv")
usethis::use_data(EFLibrary, overwrite = TRUE)
