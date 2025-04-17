#' co2e_efl
#'
#' This function consolidates and returns an emission factor library based on your selection of global warming potentials (GWPs). If you have no preference for GWPs, we recommend following UNFCCC guidelines, which require the use of GWP values from the IPCC's Fifth Assessment Report (AR5).
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "TAR", "AR4", or "AR5".
#' @param EFL Emissions Factor Library organized according to the template. Defaults to \code{EFLibrary}
#' @return An emission factor library with CO2e values based on your GWP selection.
#' @import data.table
#' @export
co2e_efl <- function(GWP, EFL = EFLibrary){

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
  return(EFL_co2e)

}
