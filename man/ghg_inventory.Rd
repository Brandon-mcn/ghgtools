% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ghg_inventory.R
\name{ghg_inventory}
\alias{ghg_inventory}
\title{ghg_inventory}
\usage{
ghg_inventory(GWP)
}
\arguments{
\item{GWP}{Select your GWPs. Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"}
}
\value{
The GHG raw data
}
\description{
Calculates a GHG inventory using data in AssetPortfolio and ActivityData. An emission factor library is generated based on your selection of global warming potentials (GWPs). If you have no preference for GWPs, we reccomend following UNFCCC guidelines, which require the use of GWP values from the IPCC's Fifth Assessment Report (AR5).
}
\seealso{
\href{https://www.epa.gov/ghgemissions/understanding-global-warming-potentials}{Gudiance from EPA about GWPs}
}