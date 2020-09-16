#### From 2020/08/21, use this file to clean raw data from PCI ####

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

setwd("PCI")

#### Import PCI Data

# guides contains info on which rows/columns to keep
guide.pci <- read.csv("PCI_Import.csv", stringsAsFactors=F)

# read in PCI data for every year using guides
pci <- do.call(bind_rows, lapply(1:nrow(guide.pci), function(i) {
    x <- guide.pci[i,]

    year <- as.numeric(x[,1])
    type <- as.character(x[,2])
    sheet <- as.numeric(x[,3])
    beginrow <- as.numeric(x[,4]) # row where data begins, row-1 to include headings
    endrow <- as.numeric(x[,5])
    colraw <- as.numeric(x[, 7:20])
    col <- colraw[!is.na(colraw)]

    if(x[,6]!=0) {
        province <- as.numeric(x[,6])
        class <- c("character", rep("numeric", length(col)))
    } else {
        province <- col-1
        class <- rep(c("character", "numeric"), length(col))
    }

    file.pci <- paste("PCI ", year, ".", type, sep="")

    pci <- read.xlsx2(file.pci, sheetIndex=sheet, startRow=beginrow, endRow = endrow, colIndex = c(province, col),
                      header=F, colClasses = class, stringsAsFactors=F)

    # if table is organized in (province, score, province, score, etc...) format
    if(length(province) > 1) {
        pci <- data.frame(as.character(pci[order(pci[,1]),1]),
                          lapply(seq(2,ncol(pci),2), function(c) {pci[order(pci[,c-1]),c]}))
    }

    namesraw <- c("pci_entry", "pci_land", "pci_transparency", "pci_time", "pci_informal", "pci_implement",
                  "pci_bias_old", "pci_bias", "pci_proactive", "pci_support", "pci_labor", "pci_legal", 
                  "pci_unweighted", "pci_weighted")
    names(pci) <- c("prov", namesraw[!is.na(colraw)][order(col)])
    pci$year <- year

    return(pci)
}))

# Read in .dta for 2005-2019 from Eddy which contains Infrastructure Index
pci_infra <- read_dta("PCI_2005-2019_Eddy.dta") %>%
  mutate(pci_infrastructure = sub11_infrastructure,
         prov = pci_id) %>%
  select(prov, year, pci_infrastructure)

#### Cleaning

# clean up province names using the Vietnam Code function
pci$prov <- vietnamcode(pci$prov, origin = "province_name", destination = "province_name")
pci_infra$prov <- vietnamcode(pci_infra$prov, origin = "pci", destination = "province_name")

# merge public data with infrastructure index data
pci_full <- left_join(pci, pci_infra, by = c("prov", "year"))

# arrange variables
pci <- pci %>%
    select(prov, year, pci_entry, pci_land, pci_transparency, pci_time, pci_informal, pci_implement,
           pci_bias_old, pci_bias, pci_proactive, pci_support, pci_labor, pci_legal, pci_unweighted, pci_weighted) %>%
    arrange(prov, year)

# fill in unweighted score for all obs
pci <- pci %>%
    mutate(pci_unweighted = ifelse(is.na(pci_unweighted), rowSums(.[3:12], na.rm=TRUE), pci_unweighted))


#### Save final output
save(pci, file = "PCI_cleaned.RData")
