#' create_efl
#'
#' This function consolidates and returns an emission factor library based on your selection of global warming potentials (GWPs). If you have no preference for GWPs, we recommend following UNFCCC guidelines, which require the use of GWP values from the IPCC's Fifth Assessment Report (AR5).
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @return The consolidated emission factor library
#' @import data.table
#' @export
create_efl <- function(GWP){

gwp_key <- c("ghg",GWP)
GWPs <- GWPs[, gwp_key, with = FALSE]
colnames(GWPs)[2] <- "gwp"

EFL_CO2e <- merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE)
EFL_CO2e[, ar := GWP]
EFL_CO2e[, kgco2e_perunit := ghg_perunit*gwp]
EFL_CO2e[, ef_publishdate := format(ef_publishdate, "%m/%d/%Y")]
EFL_CO2e[EFL_CO2e == ""] <- NA
setcolorder(EFL_CO2e, c("ef_source",
                        "ef_publishdate",
                        "ef_activeyear",
                        "emission_category",
                        "service_type",
                        "unit",
                        "service_subcategory1",
                        "service_subcategory2",
                        "emission_scope",
                        "country",
                        "subregion",
                        "source_emission_factor",
                        "ghg_unit",
                        "ghg",
                        "unit_conversion",
                        "conversion_factor",
                        "ghg_perunit",
                        "ar",
                        "gwp",
                        "kgco2e_perunit"))
EFL_CO2e <- EFL_CO2e[, .(kgco2e_perunit = sum(kgco2e_perunit)), by = .(ef_source, ef_publishdate, ef_activeyear, service_type, unit, emission_category, service_subcategory1, service_subcategory2, emission_scope, country, subregion, ar)]
setnames(EFL_CO2e, "ef_activeyear", "year")

fwrite(EFL_CO2e, "EFL.csv")

}

#' create_templates
#'
#' This function creates the activity data and asset portfolio templates as CSV files in your project directory.
#'
#' @return The activity data and asset portfolio templates
#' @import data.table
#' @export
create_templates <- function(){

  fwrite(ActivityData, "ActivityData.csv")
  fwrite(AssetPortfolio, "AssetPortfolio.csv")
  return(c("Success! Check your directory for .csv templates",
         "Add your data to these templates, making sure not to change the column headers",
         "With your data in the templates, run `check_load_data` to load your data into r"))
}

#' check_load_data
#'
#' This function checks your input data and loads it into the global environment
#'
#' @return The required dataframes for ghgtools
#' @import data.table
#' @export
check_load_data <- function(){

  Activity_Data <- fread("ActivityData.csv")
  Asset_Portfolio <- fread("AssetPortfolio.csv")
  EF_Library <- fread("EFL.csv")

  assign("Activity_Data", Activity_Data, envir = .GlobalEnv)
  assign("Asset_Portfolio", Asset_Portfolio, envir = .GlobalEnv)
  assign("EF_Library", EF_Library, envir = .GlobalEnv)

  return(c("Success! Check your global environment for 'Activity_Data', 'Asset_Portfolio', and 'EF_Library'",
           "You are now ready to run 'ghg_inventory' and calculate some GHG emissions!"))

}

#' ghg_inventory
#'
#' Calculates a GHG inventory using input data from EF_Library, Asset_Portfolio, and Activity_Data variables in the global environment.
#'
#' @return The GHG raw data
#' @import data.table
#' @export
ghg_inventory <- function(){

  DT1 <- Activity_Data
  DT2 <- data.table(merge.data.table(DT1, Asset_Portfolio, sort = FALSE, all.x = TRUE))
  DT3 <- data.table(merge.data.table(DT2, Ecat_lookup, by = c("asset_type", "service_type"), sort = FALSE))
  DT4 <- data.table(merge.data.table(DT3, eGRIDlookup, sort = FALSE, all.x = TRUE))
  DT4[is.na(DT4)] <- ""
  GHGrawdata <- data.table(merge.data.table(DT4, EF_Library, by = c("year",
                                                                  "service_type",
                                                                  "emission_category",
                                                                  "service_subcategory1",
                                                                  "service_subcategory2",
                                                                  "country",
                                                                  "subregion",
                                                                  "unit"),
                                            all.x = TRUE, sort = FALSE))
  GHGrawdata[, kg_co2e := usage * kgco2e_perunit]
  GHGrawdata[, mt_co2e := kg_co2e/1000]
  setcolorder(GHGrawdata, c("asset_id",
                            "asset_type",
                            "asset_subtype",
                            "address",
                            "city",
                            "state",
                            "zip",
                            "country",
                            "region",
                            "subregion",
                            "business_unit",
                            "year_built",
                            "sqft",
                            "service_type",
                            "unit",
                            "vendor",
                            "account_id",
                            "meter_number",
                            "date",
                            "year",
                            "cost",
                            "usage",
                            "emission_category",
                            "service_subcategory1",
                            "service_subcategory2",
                            "emission_scope",
                            "kgco2e_perunit",
                            "kg_co2e",
                            "mt_co2e",
                            "ef_source",
                            "ef_publishdate"))
  GHGrawdata[is.na(GHGrawdata)] <- ""

  fwrite(GHGrawdata, "GHGrawdata.csv")
  assign("GHG_rawdata", GHGrawdata, envir = .GlobalEnv)
  return(c("Success! Check your directory for 'GHGrawdata.csv'"))

}
