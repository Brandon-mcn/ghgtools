#' Emission Factor Library
#'
#' Emission factors are organized as a single record of data for each relevant greenhouse gas.
#'
#' @format ## `EFLibrary`
#' A data frame with 33,421 rows and 18 columns:
#' \describe{
#'   \item{ef_source}{Emission factor source.}
#'   \item{ef_publishdate}{Emission factor publication date.}
#'   \item{ef_activeyear}{Year for which the emission factor is active for GHG reporting.}
#'   \item{service_type}{Name for the fuel or activity which generates emissions. Must match servicd type in activity data.}
#'   \item{unit}{Unit for which the service type activity data is provided.}
#'   \item{emission_category}{Category of emissions, e.g., mobile, stationary, and indirect energy.}
#'   \item{service_subcategory1}{Optional input. If used, must match a service_subcategory1 in activity date}
#'   \item{service_subcategory2}{Optional input. If used, must match a service_subcategory2 in activity date}
#'   \item{supplier}{Optional input. Supplier of the service type.}
#'   \item{emission_scope}{Scope of emissions, i.e., scope 1, 2, or 3.}
#'   \item{country}{Used to specify a country for the emission factor.}
#'   \item{subregion}{Used to specify a sub-region for electricity emission factors.}
#'   \item{ghg}{The greenhouse gas procued by the service type.}
#'   \item{ghg_unit}{The greenhous egas unit. This matches the unit provided in the emission factor source documentation.}
#'   \item{source_emission_factor}{The emission factor in source units.}
#'   \item{unit_conversion}{ghgtools requires emission factors are in kg of ghgs. The ghg_unit must be converted to kg using a unit conversion.}
#'   \item{conversion_factor}{ghg unit conversion factor.}
#'   \item{ghg_perunit}{Emission factor in kg ghg per service type unit.}
#'   ...
#' }
#' @seemore <ghgtools.io>
"EFLibrary"

