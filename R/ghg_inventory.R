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
ghg_inventory <- function(AD, AP, GWP, EFL = EFLibrary){

  # Create the table of global warming potentials
  gwp_key <- c("ghg",GWP)
  GWPs <- GWPs[, gwp_key, with = FALSE]
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs[GWPs$ghg == "CO2", gwp]
  ch4gwp <- GWPs[GWPs$ghg == "CH4", gwp]
  n2ogwp <- GWPs[GWPs$ghg == "N2O", gwp]

  # Consolidate the emission factor library into CO2e values
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
  setnames(EFL_CO2e, "CO2", "co2_kgperunit")
  setnames(EFL_CO2e, "CH4", "ch4_kgperunit")
  setnames(EFL_CO2e, "N2O", "n2o_kgperunit")
  setnames(EFL_CO2e, "other_ghgs", "otherghgs_kgco2eperunit")
  setnames(EFL_CO2e, "ef_activeyear", "year")
  EFL_CO2e[is.na(EFL_CO2e)] <- ""

  # Merge activity data with asset portfolio
  AD1 <- data.table(merge.data.table(AD, AP, sort = FALSE, all.x = TRUE))
  AD1[, zip := as.numeric(zip)]

  # Add column to specify the emission category
  AD1[, emission_category := ifelse(asset_type == "Vehicle", "Mobile",
                              ifelse(asset_type == "Equipment", asset_subtype,
                              ifelse(service_type == "Steam" | service_type == "Chilled Water" | service_type == "Electricity", "Indirect Energy",
                              ifelse(service_type == "Purchased Goods and Services", "Purchased Goods and Services",
                              ifelse(service_type == "Capital Goods", "Capital Goods",
                              ifelse(service_type == "Business Travel", "Business Travel",
                              ifelse(service_type == "Employee Commuting", "Employee Commuting",
                              "Stationary")))))))]

  # Add column to specify the eGRID subregion for electricity
  AD1 <- data.table(merge.data.table(AD1, eGRIDlookup, sort = FALSE, all.x = TRUE))
  AD1[is.na(AD1)] <- ""

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



