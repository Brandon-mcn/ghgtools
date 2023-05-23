#' GHG Raw Data
#'
#' GHG_RawData requires correctly formatted **Activity Data, Asset Portfolio, and Emission Factor Library** to exist in your repository. Please visit [ghgtools.io](https://www.ghgtools.io/tools/) for detailed instructions and resources. 
#' 
#' @param GWP Select your desired global warming potentials (GWPs). Enter with quotation marks in the function. Choices are "SAR", "AR4", "AR5", or "AR6"
#' @return Two data sets are written to a csv file in your repository. 
#' @return The first is an error report for records of activity data that failed to calculate GHG emissions properly. Please review this file carefully and make any necessary changes to your activity data and/or asset portfolio.
#' @return The second data set contains the GHG emissions for each record of activity data that passed through the function successfully. 
#' @export
GHG_RawData <- function(GWP){
  
  #load in Asset Portfolio
  
  AssetPortfolio <- read_excel("ghgtools_AssetPortfolio.xlsx", sheet = "AssetPortfolio")
  
  #load in emission factor library and filter out extra columns
  
  EFL <-  read_excel("OpenEFL.xlsx", sheet = "EFL") %>% 
    select(-c("Vendor", "ProductID", "EFStartDate", "EFRetireDate")) %>% 
    select(-c("CO2_SourceUnit.per.UOM", "CH4_SourceUnit.per.UOM",	"N2O_SourceUnit.per.UOM", "AUXCO2e_SourceUnit.per.UOM",	"GHGSourceUnit", "Validated?")) %>% 
    select(-starts_with("SubCat"))
  
  #load in scope type mapping
  
  ScopeType <-  read_excel("OpenEFL.xlsx", sheet = "ScopeType")
  
  #load in eGRID lookup table for electricity subregions
  
  eGRIDlookup <- read_excel("OpenEFL.xlsx", sheet = "eGRIDlookup") 
  
  #load in GWPs and filter to selected assessment report
  
  x <- if(GWP == "SAR"){
    2
  } else if (GWP == "AR4"){
    3
  } else if (GWP == "AR5"){
    4
  } else if (GWP == "AR6"){
    5
  } else {
    NA
  }
  GWPs <- read_excel("OpenEFL.xlsx", sheet = "GWPs")
  GWPs <- GWPs[c(1,x)]
  
  #load Activity Data and join to Asset Portfolio
  
  GHGrawdata <- read_excel("ghgtools_ActivityData.xlsx", sheet = "ActivityData") %>%
    mutate(Year = year(Date)) %>%
    relocate(Year, .after = Date) %>%
    left_join(AssetPortfolio, by = "AssetID") %>%
    
    #filter to USA only
    
    filter(Country == "United States") %>% 
    
    #join egrid_subregions for electricity to Activity Data
    
    left_join(eGRIDlookup, by = c("ZIP", "ServiceType")) %>%
    
    #map scope type and emissions category Activity Data
    
    left_join(ScopeType, by = c("AssetType", "ServiceType")) %>% 
    
    #Map emission factors to Activity Data
    
    left_join(EFL, by = c("Year", "ServiceType", "EmissionCategory", "Country", "Subregion", "UOM")) %>% 
    
    #Load GWPs
    
    add_column(GWP = colnames(GWPs[,2])) %>% 
    add_column(CO2GWP = GWPs[[1,2]]) %>%
    add_column(CH4GWP = GWPs[[2,2]]) %>%
    add_column(N2OGWP = GWPs[[3,2]]) %>% 
    
    #Calculate GHG emissions for each activity data record
    
    mutate(kgCO2e.per.UOM = (CO2_kg.per.UOM*CO2GWP) + (CH4_kg.per.UOM*CH4GWP) + (N2O_kg.per.UOM*N2OGWP) + (AUXCO2e_kg.per.UOM)) %>% 
    mutate(kgCO2e = Usage*kgCO2e.per.UOM) %>% 
    mutate(MetricTonsCO2e = kgCO2e/1000)
  
  #Generate error report
  
  GHG_ErrorReport <- GHGrawdata %>% 
    filter(is.na(MetricTonsCO2e)) %>% 
    write_excel_csv("GHG_ErrorReport.csv")
  
  #Generate GHG Raw Data
  
  GHG_FullRawData <- GHGrawdata %>% 
    filter(!is.na(MetricTonsCO2e)) %>% 
    write_excel_csv("GHG_RawData.csv")
}

#' GHG Annual Report
#'
#' GHG_Report requires a **GHG_RawData.csv** file to exist in your repository. This file is generated using the *GHG_RawData* function. Please visit [ghgtools.io](https://www.ghgtools.io/tools/) for detailed instructions and resources. 
#' 
#' @param ReportingYear Select the year you wish to generate the GHG report for. 
#' @return This function is a component of the GHG Inventory Report repository. 
#' @export
GHG_AnnualReport <- function(ReportingYear){
  data <- read_csv("GHG_RawData.csv") %>% 
    filter(Year == ReportingYear) %>% 
  write_excel_csv("GHG_AnnualReport.csv")
}
