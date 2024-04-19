#' Activity Data Template
#'
#' A template for activity data used in ghgtools. You must follow the template structure in order for ghgtools to function properly.
#'
#' @format ## `ActivityData`
#' A data frame with 36 rows and 12 columns:
#' \describe{
#'   \item{asset_id}{Required input. Must be alpha-numeric. Must match an AssetID from your Asset Portfolio.}
#'   \item{supplier}{Optional input. Name of the utility vendor or supplier.}
#'   \item{account_number}{Optional input. Account number for utility service.}
#'   \item{meter_number}{Optional input. Meter number for utility service.}
#'   \item{date}{Optional input. Date of the service or activity. If used, it must be in short date format, i.e. 04/16/1993.}
#'   \item{year}{Required input. Year of the service or activity.}
#'   \item{cost}{Optional input. Cost of the activit or service. Must be numeric.}
#'   \item{service_type}{Required input. Must match a service type in the emission factor library.}
#'   \item{service_subcategory1}{Optional input. If used, must match a service_subcategory1 in the eission factor library.}
#'   \item{service_subcategory2}{Optional input. If used, must match a service_subcategory2 in the eission factor library.}
#'   \item{unit}{Required input. Must match a unit for the service type in the emission factor library.}
#'   \item{usage}{Required input. The amount of the activity or service.}
#'   ...
#' }
#' @seealso [ghgtools.io](ghgtools.io)
"ActivityData"
