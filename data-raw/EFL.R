## code to prepare `EFL` dataset goes here
library(devtools)
library(data.table)
EFL <- fread("data-raw/EFL.csv")
usethis::use_data(EFL, overwrite = TRUE)
