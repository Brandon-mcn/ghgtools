#' co2e_efl
#'
#' This function consolidates and returns an emission factor library based on your selection of global warming potentials (GWPs). If you have no preference for GWPs, we recommend following UNFCCC guidelines, which require the use of GWP values from the IPCC's Fifth Assessment Report (AR5).
#'
#' @seealso [Gudiance from EPA about GWPs](https://www.epa.gov/ghgemissions/understanding-global-warming-potentials)
#' @param GWP Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @param EFL Emissions Factor Library organized according to the template. Defaults to \code{EFLibrary}
#' @return An emission factor library with CO2e values based on your GWP selection.
#' @import data.table
#' @export
co2e_efl <- function(GWP, EFL = EFLibrary){

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
