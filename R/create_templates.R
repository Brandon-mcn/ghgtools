#' create_templates
#'
#' This function creates the activity data and asset portfolio templates as CSV files in your project directory.
#'
#' @return The activity data, asset portfolio templates, and the full emission factor library
#' @import data.table
#' @export
create_templates <- function(){

  fwrite(ActivityData, "ActivityData.csv")
  fwrite(AssetPortfolio, "AssetPortfolio.csv")
  fwrite(EFLibrary, "EFLibrary.csv")
  return("Success! Check your directory for ActivityData.csv, AssetPortfolio.csv, and EFLibrary.csv")
}
