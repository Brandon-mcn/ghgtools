## code to prepare `ActivityData` dataset goes here
library(devtools)
library(data.table)
ActivityData <- fread("data-raw/ActivityData.csv")
ActivityData[is.na(ActivityData)] <- ""
usethis::use_data(ActivityData, overwrite = TRUE)
