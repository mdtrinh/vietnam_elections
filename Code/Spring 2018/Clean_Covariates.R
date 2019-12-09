library(xlsx)
library(dplyr)
library(tidyr)
library(vietnamcode)
library(vietnamdata)

setwd("../Covariates/")

#### 05/31/19: NOTE: This file only contains covariates for 2015
#### Use it only for balance tests.

#### Import GRDP and Other Covariates Data

load("grdp_provinces_1992-2017.RData")
gso_covs <- read.xlsx2("gso_covariates_2015.xlsx", 
                       sheetIndex = 1, startRow = 1,
                       colClasses = c("character", rep("numeric", 10)))

#### Cleaning

## Extract only 2015 Total GRDP and Agriculture GRDP

grdp_2015 <- gdp_clean %>% 
  filter(year == 2015) %>%
  group_by(prov) %>%
  filter(yearbook_year == max(yearbook_year)) %>%
  filter(statistic %in% c("Agriculture forestry and fishing" , "Gross Domestic Product" )) %>%
  select(prov, statistic, price_current) %>%
  spread(statistic, price_current)

names(grdp_2015) <- c("prov", "grdp_ag", "grdp")


## Clean names of covariates
gso_covs <- gso_covs %>%
  mutate(prov = vietnamcode(prov, "province_name", "province_name")) %>%
  drop_na(prov)

names(gso_covs) <- c("prov", "area_2015", "population_2015", "population_density_2015",
                     "monthly_income_2015", "share_ag_land_2015", "employment_2015",
                     "infant_mortality_2015", "schools_2015", "schools_primary_2015",
                     "public_hosp_beds_2015")

## Merge both together

covs <- full_join(grdp_2015, gso_covs, by = "prov")
