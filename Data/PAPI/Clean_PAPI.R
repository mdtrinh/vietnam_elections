#### From 2020/08/21, use this file to clean raw data from PAPI ####

library(haven)
library(xlsx)
library(dplyr)
library(ggplot2)
library(vietnamcode)
library(vietnamdata)

comp_name <- Sys.info()["nodename"]
if(comp_name == 'MDTRINH-WIN10'){
  setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data")
  dropbox <- ("G:/Dropbox (MIT)")
  OS <- "Windows"
} else if (comp_name == 'Minh-Trinh-PC' | comp_name == 'MINHTRINH-PC') {
  setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data")
  dropbox <- ("/media/dropbox/dropbox/Dropbox (MIT)")
  OS <- "Linux"
} else if (comp_name == "xvii") {
  setwd("~")
  OS <- "Linux"
}

setwd("PAPI")

#### Import PAPI Data
# file name
file.papi <- "PAPI 2011-2019.xlsx"

# read in PAPI data for every year
# raw data is already consistently formatted
papi <- do.call(bind_rows, lapply(1:9, function(i) {
  
  if(i < 6) {
    colnames <- c("prov",
                     "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_elections", "papi_participation_contributions",
                     "papi_transparency", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                     "papi_accountability", "papi_accountability_interaction", "papi_accountability_inspection", "papi_accountability_investment",
                     "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                     "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                     "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                     "papi_unweighted")
  } else if(i < 8) {
    # in 2016 there were some changes in indicators
    colnames <- c("prov",
                     "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_elections", "papi_participation_contributions",
                     "papi_transparency", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                     "papi_accountability", "papi_accountability_interaction", "papi_accountability_responsive", "papi_accountability_inspection",
                     "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                     "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                     "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                     "papi_unweighted")
  } else if(i == 8) {
    # in 2018 theere are some new changes in indicators
    colnames <- c("prov",
                     "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_localelections", "papi_participation_contributions",
                     "papi_transparency", "papi_transparency_accessinfo", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                     "papi_accountability", "papi_accountability_interaction", "papi_accountability_responsiveappeals", "papi_accountability_justiceservices",
                     "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                     "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                     "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                     "papi_environment", "papi_environment_seriousprotection", "papi_environment_air", "papi_environment_water",
                     "papi_egovernment", "papi_egovernment_accessportal", "papi_egovernment_accessinternet")
  } else {
    # in 2019 theere one new indicator is added to E-government
    colnames <- c("prov",
                  "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_localelections", "papi_participation_contributions",
                  "papi_transparency", "papi_transparency_accessinfo", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                  "papi_accountability", "papi_accountability_interaction", "papi_accountability_responsiveappeals", "papi_accountability_justiceservices",
                  "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                  "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                  "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                  "papi_environment", "papi_environment_seriousprotection", "papi_environment_air", "papi_environment_water",
                  "papi_egovernment", "papi_egovernment_accessportal", "papi_egovernment_accessinternet", "papi_egovernment_responsiveness")
  }
  years <- c(2011:2019)
  
  papi <- read.xlsx2(file.papi, sheetIndex=i, startRow=3, endRow = 65, colIndex = c(3:(length(colnames)+2)),
                     header=F, colClasses = c("character", rep("numeric",length(colnames)-1)), stringsAsFactors=F)
  
  names(papi) <- colnames
  
  papi$year <- years[i]
  
  return(papi)
}))

#### Cleaning

# clean up province names using the Vietnam Code function
papi$prov <- vietnamcode(papi$prov, origin = "province_name", destination = "province_name")

# arrange variables
papi <- papi %>%
  select(prov, year,
         papi_participation, papi_transparency, papi_accountability, papi_corruption, papi_procedures, papi_service, papi_environment, papi_egovernment,
         papi_unweighted,
         papi_participation_knowledge, papi_participation_opportunities, papi_participation_elections, papi_participation_contributions,
         papi_transparency_povertylists, papi_transparency_accessinfo, papi_transparency_budgetexpenditure, papi_transparency_landuse,
         papi_accountability_interaction, papi_accountability_inspection, papi_accountability_investment, papi_accountability_responsive,
         papi_corruption_public, papi_corruption_service, papi_corruption_employment, papi_corruption_willingness,
         papi_procedures_certification, papi_procedures_construction, papi_procedures_landuse, papi_procedures_personal,
         papi_service_health, papi_service_primary, papi_service_infrastructure, papi_service_law,
         papi_environment, papi_environment_seriousprotection, papi_environment_air, papi_environment_water,
         papi_egovernment, papi_egovernment_accessportal, papi_egovernment_accessinternet, papi_egovernment_responsiveness) %>%
  arrange(prov, year)

# fill in unweighted score for all obs
# also "old" PAPI score for consistent comparison
papi <- papi %>%
  mutate(papi_unweighted = ifelse(is.na(papi_unweighted), rowSums(.[3:10], na.rm=TRUE), papi_unweighted),
         papi_unweighted_old = rowSums(.[3:8], na.rm = TRUE))

#### Save final output
save(papi, file = "PAPI_cleaned.RData")
