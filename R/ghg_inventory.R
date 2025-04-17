#' ghg_inventory
#'
#' Calculates a GHG inventory using input data from EF_Library, Asset_Portfolio, and Activity_Data variables in the global environment.
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#'
#' @param AD Activity Data organized according to the template.
#' @param AP Asset Portfolio organized according the template.
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "TAR", "AR4", or "AR5".
#' @param EFL Emissions Factor Library organized according to the template. Defaults to \code{EFLibrary}
#'
#' @return The GHG raw data
#'
#' @examples
#' AR5_Inventory <- ghg_inventory(ActivityData, AssetPortfolio, "ar5")
#' head(AR5_Inventory)
#'
#' @import data.table
#'
#' @export
ghg_inventory <- function(AD, AP, GWP, EFL = EFLibrary){

  gwp_key <- c("ghg", GWP)
  GWPs <- GWPs[, gwp_key]
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs$gwp[GWPs$ghg == "co2"]
  ch4gwp <- GWPs$gwp[GWPs$ghg == "ch4"]
  n2ogwp <- GWPs$gwp[GWPs$ghg == "n2o"]

  EFL1 <- merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE)
  EFL1 <- as.data.table(EFL1)
  EFL1[, gwps_ar := GWP]
  EFL1[, kgco2e_perunit := kg_ghg_perunit*gwp]
  EFL1[, ef_publishdate := as.Date(ef_publishdate, format = "%Y-%m-%d")]
  EFL1[, ghg := ifelse(ghg %in% c("co2", "ch4", "n2o"), ghg, "other_ghgs")]
  EFL_co2e <- dcast(EFL1, ef_source +
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
  EFL_co2e[, kgco2e_perunit := co2 + ch4 + n2o + other_ghgs]
  EFL_co2e[, co2_gwp := co2gwp]
  EFL_co2e[, ch4_gwp := ch4gwp]
  EFL_co2e[, n2o_gwp := n2ogwp]
  EFL_co2e[, co2 := co2 / co2gwp]
  EFL_co2e[, ch4 := ch4 / ch4gwp]
  EFL_co2e[, n2o := n2o / n2ogwp]
  setnames(EFL_co2e, "co2", "co2_kgperunit")
  setnames(EFL_co2e, "ch4", "ch4_kgperunit")
  setnames(EFL_co2e, "n2o", "n2o_kgperunit")
  setnames(EFL_co2e, "other_ghgs", "otherghgs_kgco2eperunit")
  setnames(EFL_co2e, "ef_activeyear", "year")
  EFL_co2e[, year := as.numeric(year)]
  EFL_co2e[, ef_publishdate := as.Date(ef_publishdate)]
  EFL_co2e[is.na(EFL_co2e)] <- ""
  setcolorder(EFL_co2e, c("ef_source", "ef_publishdate", "year", "supplier", "service_type", "unit",
                          "emission_category", "service_subcategory1", "service_subcategory2",
                          "emission_scope", "country", "subregion", "co2_kgperunit", "ch4_kgperunit",
                          "n2o_kgperunit", "otherghgs_kgco2eperunit", "gwps_ar", "co2_gwp",
                          "ch4_gwp", "n2o_gwp", "kgco2e_perunit"))

  # Create EFL without supplier specific factors
  EFL_co2e_merge2 <- EFL_co2e[EFL_co2e$supplier == "", ]
  EFL_co2e_merge2 <- copy(EFL_co2e_merge2)[, supplier := NULL]

  # Create EFL with only global EFs
  EFL_co2e_merge3 <- EFL_co2e_merge2[EFL_co2e_merge2$country == "global", ]
  EFL_co2e_merge3 <- copy(EFL_co2e_merge3)[, country := NULL]

  # Check if kgco2e_perunit column is numeric
  if (!is.numeric(EFL_co2e$kgco2e_perunit)) {
    stop(paste("EFL Error: non-numeric value for co2e EF"))
  } else {
    message(paste("EFL conversion successful"))
  }

  # Merge activity data with asset portfolio
  AD1 <- data.table(merge.data.table(AD, AP, sort = FALSE, all.x = TRUE))
  AD1[, zip := as.numeric(zip)]

  # Add column to specify the eGRID subregion for electricity
  AD1 <- data.table(merge.data.table(AD1, eGRIDlookup, sort = FALSE, all.x = TRUE))
  AD1[is.na(AD1)] <- ""

  # Make sure year is a number
  AD1[, year := as.numeric(year)]

  # Add row ID
  AD1[, id := .I]

  # Merge the emission factor library with activity data
  merge_1 <- data.table(merge.data.table(AD1, EFL_co2e, by = c("year",
                                                               "service_type",
                                                               "emission_category",
                                                               "service_subcategory1",
                                                               "service_subcategory2",
                                                               "supplier",
                                                               "country",
                                                               "subregion",
                                                               "unit"),
                                         all.x = TRUE, sort = FALSE))

  failed_id1 <- merge_1[is.na(kgco2e_perunit), id]
  AD_subset1 <- AD1[id %in% failed_id1]

  merge_2 <- data.table(merge.data.table(AD_subset1, EFL_co2e_merge2, by = c("year",
                                                                             "service_type",
                                                                             "emission_category",
                                                                             "service_subcategory1",
                                                                             "service_subcategory2",
                                                                             "country",
                                                                             "subregion",
                                                                             "unit"),
                                         all.x = TRUE, sort = FALSE))

  failed_id2 <- merge_2[is.na(kgco2e_perunit), id]
  AD_subset2 <- AD1[id %in% failed_id2]

  merge_3 <- data.table(merge.data.table(AD_subset2, EFL_co2e_merge3, by = c("year",
                                                                             "service_type",
                                                                             "emission_category",
                                                                             "service_subcategory1",
                                                                             "service_subcategory2",
                                                                             "subregion",
                                                                             "unit"),
                                         all.x = TRUE, sort = FALSE))

  combined_merge <- data.table(rbindlist(list(merge_1, merge_2, merge_3), use.names = TRUE, fill = TRUE))
  setorder(combined_merge, id)

  GHGrawdata <- combined_merge[!is.na(kgco2e_perunit)]

  # Check if all activity data records were properly merged
  ad_id <- as.vector(AD1$id)
  ghg_id <- as.vector(GHGrawdata$id)

    if (!identical(ad_id, ghg_id)) {
      mismatched_rows <- which(ad_id != ghg_id)
      stop(paste("Error: The emission factor merge has failed. Mismatched rows #",
                 paste(mismatched_rows, collapse = ", ")))
    }

  GHGrawdata$id <- NULL

  # Calculate GHG emissions
  GHGrawdata[, kg_co2 := usage * co2_kgperunit]
  GHGrawdata[, kg_ch4 := usage * ch4_kgperunit]
  GHGrawdata[, kg_n2o := usage * n2o_kgperunit]
  GHGrawdata[, kg_co2e := usage * kgco2e_perunit]
  GHGrawdata[, mt_co2e := kg_co2e/1000]

  # Organize data
  setcolorder(GHGrawdata, c("asset_id", "asset_name","asset_type", "asset_subtype", "asset_description", "address", "city",
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



