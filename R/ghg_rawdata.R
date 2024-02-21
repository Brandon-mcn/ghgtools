#' ghg_rawdata
#'
#' ghg_rawdata requires correctly formatted **Activity Data, Asset Portfolio, and Emission Factor Library** to exist in your repository. Please visit [ghgtools.io](https://www.ghgtools.io/tools/) for detailed instructions and resources.
#'
#' @param GWP Select your desired global warming potentials (GWPs). Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @return Two data sets are written to a csv file in your repository.
#' @return The first is an error report for records of activity data that failed to calculate GHG emissions properly. Please review this file carefully and make any necessary changes to your activity data and/or asset portfolio.
#' @return The second data set contains the GHG emissions for each record of activity data that passed through the function successfully.
#' @export
ghg_rawdata <- function(GWP){

  #load required packages

  if (!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(readxl, magrittr, tidyverse, lubridate, devtools, googlesheets4)

  #load emission factor library from google sheets and create csv in repository

  EFL <- read_sheet("1CIyUsINOLlQZnuRGZlPpFMLAuq3RHOoSvs3u6KMUfG0","EFL") %>%
    write_excel_csv("EFL.csv")

  #load asset portfolio from google sheets and create csv in repository

  AssetPortfolio <- read_sheet("1TGh_Q29n7yfnuZRb_yOtqonyPFAymflXyA_mWukOTBA") %>%
    write_excel_csv("Asset_Portfolio.csv")

  #load activity data from google sheets and create csv in repository

  ActivityData <- read_sheet("1xxVNpq_QWKLtEHW6ty3-bGTXmtHaKXRqF73q88Ey2mU") %>%
    write_excel_csv("Activity_Data.csv")

  #load in eGRID lookup table for electricity subregions

  eGRIDlookup <- read_sheet("1CIyUsINOLlQZnuRGZlPpFMLAuq3RHOoSvs3u6KMUfG0","egrid_lookup")

  #load in GWPs and filter to selected assessment report

  x <- if(GWP == "SAR"){
    2
  } else if (GWP == "TAR"){
    3
  } else if (GWP == "AR4"){
    4
  } else if (GWP == "AR5"){
    5
  } else {
    NA
  }

  GWPs <- read_sheet("1CIyUsINOLlQZnuRGZlPpFMLAuq3RHOoSvs3u6KMUfG0","GWPs")
  GWPs <- GWPs[c(1,x)]
  colnames(GWPs)[2] <- "GWP"

  #Create EFL_CO2e table

  EFL_CO2e <- EFL %>%
    left_join(GWPs, by = "ghg") %>%
    mutate(co2e_per_unit = ghg_emission_factor * GWP) %>%
    group_by(source, ef_publishdate, ef_activeyear, service_type, emission_category, emission_scope, country, subregion, service_type_unit) %>%
    summarise(ef_kg_co2e = sum(co2e_per_unit)) %>%
    rename(year = ef_activeyear)

  #ghgtools Initiate complete. Now user needs to add their data
  #------------------------------------------------------------

  #load Activity Data and join to Asset Portfolio

  GHGrawdata <- read_csv("Activity_Data.csv",show_col_types = FALSE) %>%
    mutate(year = year(date)) %>%
    relocate(year, .after = date) %>%
    left_join(AssetPortfolio, by = "asset_id") %>%

    #join egrid_subregions for electricity to Activity Data

    left_join(eGRIDlookup, by = c("zip", "service_type")) %>%

    #create emission_category

    mutate(emission_category = if_else(service_type == "Electricity", "Indirect Energy",
                                       if_else(asset_type == "Vehicle", "Mobile", "Stationary"))) %>%

    #Map emission factors to Activity Data

    left_join(EFL_CO2e, by = c("year", "service_type", "emission_category", "country", "subregion", "service_type_unit")) %>%

    #Calculate GHG emissions for each activity data record

    mutate(kg_co2e = usage * ef_kg_co2e) %>%
    mutate(MT_co2e = kg_co2e/1000)

  #Generate error report

  GHG_ErrorReport <- GHGrawdata %>%
    filter(is.na(MT_co2e)) %>%
    mutate(across(everything(), as.character)) %>%
    replace(is.na(.), "") %>%
    write_excel_csv("GHG_ErrorReport.csv")

  #Generate GHG Raw Data

  GHG_FullRawData <- GHGrawdata %>%
    filter(!is.na(MT_co2e)) %>%
    mutate(across(everything(), as.character)) %>%
    replace(is.na(.), "") %>%
    write_excel_csv("GHG_RawData.csv")

}
