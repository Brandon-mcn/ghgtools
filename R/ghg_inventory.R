#' create_templates
#'
#' This function creates the activity data and asset portfolio templates as CSV files in your project directory.
#'
#' @return The activity data, asset portfolio templates, and the full emission factor library
#' @import data.table
#' @export
create_templates <- function(output_location = NULL){
  # I might specify where you want to save your files/template to
  # or have an automatic place to save to. If this is meant to be performed locally.
  # E.G. - potential example totally up to you, just maybe something to think about
  if(is.null(output_location)){
    green_house_gas <- "Green House Gas Accounting"
    out_folder <- file.path(tempdir(), green_house_gas)
    # Check if directory exists
    if(!file.exists(out_folder)){
      dir.create(out_folder) # saves to a temporary directory, can be input manually
    }
  }

  data.table::fwrite(ActivityData, file.path(out_folder, "ActivityData.csv"))
  data.table::fwrite(AssetPortfolio, file.path(out_folder, "AssetPortfolio.csv"))
  data.table::fwrite(EFLibrary, file.path(out_folder, "EFLibrary.csv"))
  print("Success! Check your directory for ActivityData.csv, AssetPortfolio.csv, and EFLibrary.csv")
  return(out_folder)
}

#' ghg_inventory
#'
#' Calculates a GHG inventory using input data from EF_Library, Asset_Portfolio, and Activity_Data variables in the global environment.
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#'
#' @param AD Activity Data organized according to the template.
#' @param AP Asset Portfolio organized according the template.
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6".
#' @param EFL Emissions Factor Library organized according to the template. Defaults to \code{EFLibrary}
#'
#' @return The GHG raw data
#'
#' @examples
#' ghg_inventory(ActivityData, AssetPortfolio, "AR5")
#'
#' @import data.table
#'
#' @export
ghg_inventory <- function(AD, AP, GWP = "AR5", EFL = EFLibrary){

  # Create the table of global warming potentials
  gwp_key <- c("ghg",GWP)
  GWPs <- GWPs[, gwp_key, with = FALSE] # I think it is best practice to duplicate this dataset
  # If you rename it and are trying to troubleshoot it within a session, this will
  # cause errors because you have subsetted the dataset. It did for me at least.
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs[GWPs$ghg == "CO2", gwp]
  ch4gwp <- GWPs[GWPs$ghg == "CH4", gwp]
  n2ogwp <- GWPs[GWPs$ghg == "N2O", gwp]

  # Consolidate the emission factor library into CO2e values
  EFL1 <- data.table::merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE)
  EFL1[, gwps_ar := GWP]
  EFL1[, kgco2e_perunit := ghg_perunit*gwp] # getting error here
  # Error in ghg_perunit * gwp : non-numeric argument to binary operator
  # I think it is because gwp is a string
  EFL1[, ef_publishdate := format(ef_publishdate, "%m/%d/%Y")]
  EFL1[, ghg := ifelse(ghg %in% c("CO2", "CH4", "N2O"), ghg, "other_ghgs")]
  EFL_CO2e <- data.table::dcast(EFL1, ef_source +
                      ef_publishdate +
                      ef_activeyear +
                      service_type +
                      unit +
                      emission_category +
                      service_subcategory1 +
                      service_subcategory2 +
                      supplier +
                      emission_scope +
                      country +
                      subregion +
                      gwps_ar ~ ghg,
                    value.var = "kgco2e_perunit",
                    fun.aggregate = sum)
  EFL_CO2e[, kgco2e_perunit := CO2 + CH4 + N2O + other_ghgs]
  EFL_CO2e[, co2_gwp := co2gwp]
  EFL_CO2e[, ch4_gwp := ch4gwp]
  EFL_CO2e[, n2o_gwp := n2ogwp]
  EFL_CO2e[, CO2 := CO2 / co2gwp]
  EFL_CO2e[, CH4 := CH4 / ch4gwp]
  EFL_CO2e[, N2O := N2O / n2ogwp]
  data.table::setnames(EFL_CO2e, "CO2", "co2_kgperunit")
  data.table::setnames(EFL_CO2e, "CH4", "ch4_kgperunit")
  data.table::setnames(EFL_CO2e, "N2O", "n2o_kgperunit")
  data.table::setnames(EFL_CO2e, "other_ghgs", "otherghgs_kgco2eperunit")
  data.table::setnames(EFL_CO2e, "ef_activeyear", "year")
  EFL_CO2e[is.na(EFL_CO2e)] <- ""

  # ** This seems like it could be a separate function - Checking the
  # Activity Data and the Assets
  # Merge activity data with assset portfolio
  AD1 <- data.table::data.table(data.table::merge.data.table(AD, AP, sort = FALSE, all.x = TRUE))
  AD1[, zip := as.numeric(zip)]

  # Add column to specify the emission category
  AD1 <- data.table::data.table(data.table::merge.data.table(AD1,
                                                             Ecat_lookup,
                                                             by = c("asset_type", "service_type"),
                                                             sort = FALSE))

  # Add column to specify the eGRID subregion for electricity
  AD1 <- data.table::data.table(data.table::merge.data.table(AD1,
                                                             eGRIDlookup,
                                                             sort = FALSE,
                                                             all.x = TRUE))
  AD1[is.na(AD1)] <- ""

  # ** This chunk to above could be separated into a separate function
  # that is called and returns the "AD1"  table to be merged with EFL_C02e
  # It might be more work at first, but you could have some checks within that
  # function that could determine if the input data is clean and up to snuff

  # Merge the emission factor library with activity data
  GHGrawdata <- data.table(merge.data.table(AD1, EFL_CO2e, by = c("year",
                                                                  "service_type",
                                                                  "emission_category",
                                                                  "service_subcategory1",
                                                                  "service_subcategory2",
                                                                  "country",
                                                                  "supplier",
                                                                  "subregion",
                                                                  "unit"),
                                            all.x = TRUE, sort = FALSE))

  # Calculate GHG emissions
  GHGrawdata[, kg_co2 := usage * co2_kgperunit]
  GHGrawdata[, kg_ch4 := usage * ch4_kgperunit]
  GHGrawdata[, kg_n2o := usage * n2o_kgperunit]
  GHGrawdata[, kg_co2e := usage * kgco2e_perunit]
  GHGrawdata[, mt_co2e := kg_co2e/1000]

  # Organize data
  setcolorder(GHGrawdata, c("asset_id", "asset_type", "asset_subtype", "address", "city",
                            "state", "zip", "country", "region", "subregion", "business_unit",
                            "year_built", "sqft", "service_type", "unit", "supplier", "account_id",
                            "meter_number", "date", "year", "cost", "usage", "emission_category",
                            "service_subcategory1", "service_subcategory2", "emission_scope",
                            "co2_kgperunit", "ch4_kgperunit","n2o_kgperunit", "otherghgs_kgco2eperunit",
                            "gwps_ar", "co2_gwp","ch4_gwp", "n2o_gwp", "kgco2e_perunit", "kg_co2",
                            "kg_ch4", "kg_n2o", "kg_co2e", "mt_co2e", "ef_source", "ef_publishdate"))
  GHGrawdata[is.na(GHGrawdata)] <- ""
  return(GHGrawdata)

}

#' export_efl
#'
#' This function consolidates and returns an emission factor library based on your selection of global warming potentials (GWPs). If you have no preference for GWPs, we recommend following UNFCCC guidelines, which require the use of GWP values from the IPCC's Fifth Assessment Report (AR5).
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @param EFL Emissions Factor Library organized according to the template. Defaults to \code{EFLibrary}
#' @return The consolidated emission factor library
#' @import data.table
#' @export
export_efl <- function(GWP, EFL = EFLibrary){

  gwp_key <- c("ghg",GWP)
  GWPs <- GWPs[, gwp_key, with = FALSE]
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs[GWPs$ghg == "CO2", gwp]
  ch4gwp <- GWPs[GWPs$ghg == "CH4", gwp]
  n2ogwp <- GWPs[GWPs$ghg == "N2O", gwp]

  EFL1 <- merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE)
  EFL1[, gwps_ar := GWP]
  EFL1[, kgco2e_perunit := ghg_perunit*gwp]
  EFL1[, ef_publishdate := format(ef_publishdate, "%m/%d/%Y")]
  EFL1[, ghg := ifelse(ghg %in% c("CO2", "CH4", "N2O"), ghg, "other_ghgs")]
  EFL_CO2e <- dcast(EFL1, ef_source +
                          ef_publishdate +
                          ef_activeyear +
                          service_type +
                          unit +
                          emission_category +
                          service_subcategory1 +
                          service_subcategory2 +
                          emission_scope +
                          country +
                          subregion +
                          gwps_ar ~ ghg,
                          value.var = "kgco2e_perunit",
                          fun.aggregate = sum)
  EFL_CO2e[, kgco2e_perunit := CO2 + CH4 + N2O + other_ghgs]
  EFL_CO2e[, co2_gwp := co2gwp]
  EFL_CO2e[, ch4_gwp := ch4gwp]
  EFL_CO2e[, n2o_gwp := n2ogwp]
  EFL_CO2e[, CO2 := CO2 / co2gwp]
  EFL_CO2e[, CH4 := CH4 / ch4gwp]
  EFL_CO2e[, N2O := N2O / n2ogwp]
  setnames(EFL_CO2e, "CO2", "co2_kgperunit")
  setnames(EFL_CO2e, "CH4", "ch4_kgperunit")
  setnames(EFL_CO2e, "N2O", "n2o_kgperunit")
  setnames(EFL_CO2e, "other_ghgs", "otherghgs_kgco2eperunit")
  setnames(EFL_CO2e, "ef_activeyear", "year")
  setcolorder(EFL_CO2e, c("ef_source", "ef_publishdate", "year", "service_type", "unit",
                          "emission_category", "service_subcategory1", "service_subcategory2",
                          "emission_scope", "country", "subregion", "co2_kgperunit", "ch4_kgperunit",
                          "n2o_kgperunit", "otherghgs_kgco2eperunit", "gwps_ar", "co2_gwp",
                          "ch4_gwp", "n2o_gwp", "kgco2e_perunit"))
  return(EFL_CO2e)

}


