#' Asset Portfolio Template
#'
#' A template for an asset portfolio used in ghgtools. You must follow the template structure in order for ghgtools to function properly.
#'
#' @format ## `AssetPortfolio`
#' A data frame with 12 rows and 12 columns:
#' \describe{
#'   \item{asset_id}{Required input. Must be alpha-numeric. The asset_id is the primary key with activity data.}
#'   \item{address}{Optional input. Address of the asset.}
#'   \item{city}{Optional input. City of the asset.}
#'   \item{state}{Optional input. State of the asset.}
#'   \item{zip}{Required input for USA. Must be a numberic 5-digit zip code. Zip code is required for selecting the correct emisison factor for electricity in the USA.}
#'   \item{country}{Required input. Country of the asset.}
#'   \item{sqft}{Optional input. Square feet for buildings. Must be numeric value.}
#'   \item{region}{Optional input. Can be any value.}
#'   \item{buisness_unit}{Optional input. Can be any value.}
#'   \item{asset_type}{Required input. Must match an asset type in the Ecat_lookup table.}
#'   \item{asset_subtype}{Required input. Must match an asset subtype in the Ecat_lookup table.}
#'   \item{year_built}{Optional input. The year the asset was built.}
#'   ...
#' }
"AssetPortfolio"
