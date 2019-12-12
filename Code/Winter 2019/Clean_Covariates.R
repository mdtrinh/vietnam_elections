library(readxl)
library(dplyr)
library(tidyr)
library(vietnamcode)
library(vietnamdata)

setwd("../Covariates/")

#### 19/05/31: NOTE: This file only contains covariates for 2015
#### Use it only for balance tests.

#### 19/12/09: Added covariates for 2006 and 2010
#### 19/12/09: Switched out xlsx::read.xlsx2 for readxl::read_excel

#### Import GRDP and Other Covariates Data

load("grdp_provinces_1992-2017.RData")
gso_covs_2015 <- read_excel("gso_covariates.xlsx", sheet = 3) %>%
  mutate(year = 2015)

gso_covs_2010 <- read_excel("gso_covariates.xlsx", sheet = 2) %>%
  mutate(year = 2010)

gso_covs_2006 <- read_excel("gso_covariates.xlsx", sheet = 1) %>%
  mutate(year = 2006)

#### Cleaning

## Extract only Total GRDP and Agriculture GRDP

# 2015
grdp <- gdp_clean %>% 
  filter(year %in% c(2015, 2010, 2006)) %>%
  group_by(prov, year) %>%
  filter(yearbook_year == max(yearbook_year)) %>%
  filter(statistic %in% c("Agriculture forestry and fishing" , "Gross Domestic Product" )) %>%
  select(prov, year, statistic, price_current) %>%
  spread(statistic, price_current)

names(grdp) <- c("prov", "year", "grdp_ag", "grdp")



## Clean names of covariates
gso_covs <- bind_rows(gso_covs_2015, gso_covs_2010, gso_covs_2006) %>%
  mutate(prov = vietnamcode(prov, "province_name", "province_name")) %>%
  drop_na(prov) %>%
  select(prov, year, 
         area_2015, population_2015, population_density_2015,
         monthly_income_2015, share_ag_land_2015, employment_rate_above15_2015,
         infant_mortality_2015, schools_2015, schools_primary_2015,
         public_hosp_beds_2015,
         monthly_income_2010, employment_rate_above15_2010,
         infant_mortality_2010, schools_2010, schools_primary_2010,
         public_hosp_beds_2010,
         employment_rate_above15_2005, schools_2006, schools_primary_2006,
         public_hosp_beds_2006)


## Merge both together

covs <- full_join(grdp, gso_covs, by = c("prov", "year"))
