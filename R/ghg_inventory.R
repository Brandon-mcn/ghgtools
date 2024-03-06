#' ghg_inventory
#'
#' ghg_inventory requires correctly formatted **Activity Data, Asset Portfolio, and Emission Factor Library** to exist in your r project directory. Please visit [ghgtools.io](https://www.ghgtools.io/tools/) for detailed instructions and resources.
#'
#' @param GWP Select your desired global warming potentials (GWPs). Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @return Two data sets are written to a csv file in your repository.
#' @return The first is an error report for records of activity data that failed to calculate GHG emissions properly. Please review this file carefully and make any necessary changes to your activity data and/or asset portfolio.
#' @return The second data set contains the GHG emissions for each record of activity data that passed through the function successfully.
#' @export
ghg_inventory <- function(GWP){

  gwp_key <- c("ghg",GWP)
  EFL <- fread("EFL.csv")
  GWPs <- fread("GWPs.csv")
  GWPs <- GWPs[, gwp_key, with = FALSE]
  colnames(GWPs)[2] <- "GWP"
  EFL_CO2e <- data.table(merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE))
  EFL_CO2e[, ':='(sum_co2e=ghg_emission_factor*GWP)]
  EFL_CO2e <- EFL_CO2e[, .(kgco2e_perunit = sum(sum_co2e)), by = .(ef_source, ef_publishdate, ef_activeyear, service_type, unit, emission_category, service_subcategory1, service_subcategory2, emission_scope, country, subregion)]
  EFL_CO2e[, ef_publishdate := format(ef_publishdate, "%m/%d/%Y")]
  EFL_CO2e[EFL_CO2e == ""] <- NA
  setnames(EFL_CO2e, "ef_activeyear", "year")

  AssetPortfolio <- fread("AssetPortfolio.csv")

  ActivityData <- fread("ActivityData.csv")

  eGRIDlookup <- fread("eGRID_lookup.csv")
  Ecat_lookup <- fread("Ecat_lookup.csv")
  DT1 <- fread("ActivityData.csv")
  DT1[DT1 == ""] <- NA
  DT2 <- data.table(merge.data.table(DT1, AssetPortfolio, sort = FALSE, all.x = TRUE))
  DT3 <- data.table(merge.data.table(DT2, Ecat_lookup, by = c("asset_type", "service_type"), sort = FALSE))
  DT4 <- data.table(merge.data.table(DT3, eGRIDlookup, sort = FALSE, all.x = TRUE))
  GHGrawdata <- data.table(merge.data.table(DT4, EFL_CO2e, by = c("year", "service_type", "emission_category", "service_subcategory1", "service_subcategory2", "country", "subregion", "unit"), all.x = TRUE, sort = FALSE))
  GHGrawdata[, kg_co2e := usage * kgco2e_perunit]
  GHGrawdata[, mt_co2e := kg_co2e/1000]
  setcolorder(GHGrawdata, c("asset_id", "asset_type", "asset_subtype", "address", "city", "state", "zip", "country", "region", "subregion", "business_unit", "year_built", "sqft", "service_type", "unit", "vendor", "account_id", "meter_number", "date", "year", "cost", "usage", "emission_category", "service_subcategory1", "service_subcategory2", "emission_scope", "kgco2e_perunit", "kg_co2e", "mt_co2e", "ef_source", "ef_publishdate"))
  GHGrawdata[is.na(GHGrawdata)] <- ""
  fwrite(GHGrawdata, "GHGrawdata.csv")

}
