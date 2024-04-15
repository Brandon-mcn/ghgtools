## code to prepare `AssetPortfolio` dataset goes here
library(devtools)
library(data.table)
AssetPortfolio <- fread("data-raw/AssetPortfolio.csv")
AssetPortfolio[is.na(AssetPortfolio)] <- ""
usethis::use_data(AssetPortfolio, overwrite = TRUE)
