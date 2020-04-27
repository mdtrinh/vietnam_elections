#### THIS FILE CONTAINS THE BALANCE TESTS THAT WILL APPEAR IN THE FINAL PAPER ####
library(ri)


#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2020/Merge_All.R")
source("../../Code/Spring 2020/Clean_Covariates.R")


#### Variables

list_vars <- c("total.rev", "total.exp",
               "num.seats.2016", "num.candidates.2016", "num.centralnominees.2016",
               "area_2015", "population_2015", "population_density_2015",
               "grdp", "monthly_income_2015","employment_rate_above15_2015",
               "share_ag_land_2015",
               "infant_mortality_2015", "public_hosp_beds_2015", "schools_2015", "schools_primary_2015")

#### Balance Table for Linear Fixed Effects Model

dat_balance_lme <- plan %>% 
  filter(year == 2015) %>%
  filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  select(prov, total.rev, total.exp,
         num.candidates.2016, num.seats.2016, num.centralnominees.2016, 
         defeat.true.2016) %>%
  inner_join(covariates %>% filter(year == 2015), by = "prov") %>%
  mutate(total.rev = total.rev/1000,
         total.exp = total.exp/1000)


treat_vector_lme <- dat_balance_lme[["defeat.true.2016"]]
table(treat_vector_lme)

set.seed(12345)
perms_lme <- genperms(treat_vector_lme)

balance_lme <- sapply(list_vars, function(c) {
  y <- dat_balance_lme[[c]]
  z <- treat_vector_lme
  
  mean_treated <- mean(dat_balance_lme[[c]][which(z == 1)])
  mean_control <- mean(dat_balance_lme[[c]][which(z == 0)])
  
  var_treated <- var(dat_balance_lme[[c]][which(z == 1)])
  var_control <- var(dat_balance_lme[[c]][which(z == 0)])
  
  diff = mean_treated - mean_control
  diff_std = diff/sqrt(var_control)
  
  ri_test <- omni.ate(Y = y, Z = z, perms = perms_lme)
  lm_test <- lm(y ~ z)
  
  return(c(mean_control = mean_control,
           mean_treated = mean_treated,
           diff = diff_std,
           se_ri = ri_test$se,
           p_ri = ri_test$p.value,
           se_lm = summary(lm_test)$coefficients["z",2],
           p_lm = summary(lm_test)$coefficients["z",4]))
})

#### Balance Table for Local Randomization RDD

treatment.2016.observed <- treatment_generate(candidates2016rdd)

dat_balance_rdd <- plan %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
  mutate(defeat.2016 = treatment.2016.observed) %>%
  filter(year == 2015) %>%
  filter(prov!="Binh Duong") %>%
  filter(!is.na(defeat.2016)) %>%
  select(prov, total.rev, total.exp,
         num.candidates.2016, num.seats.2016, num.centralnominees.2016, 
         defeat.2016) %>%
  inner_join(covariates %>% filter(year == 2015), by = "prov") %>%
  mutate(total.rev = total.rev/1000,
         total.exp = total.exp/1000)

treat_vector_rdd <- dat_balance_rdd[["defeat.2016"]]
table(treat_vector_rdd)

set.seed(12345)
perms_rdd <- genperms(treat_vector_rdd)

balance_rdd <- sapply(list_vars, function(c) {
  y <- dat_balance_rdd[[c]]
  z <- treat_vector_rdd
  
  mean_treated = mean(dat_balance_rdd[[c]][which(z == 1)])
  mean_control = mean(dat_balance_rdd[[c]][which(z == 0)])
  
  var_treated <- var(dat_balance_rdd[[c]][which(z == 1)])
  var_control <- var(dat_balance_rdd[[c]][which(z == 0)])
  
  diff = mean_treated - mean_control
  diff_std = diff/sqrt(var_control)
  
  ri_test <- omni.ate(Y = y, Z = z, perms = perms_rdd)
  lm_test <- lm(y ~ z)
  
  return(c(mean_control = mean_control,
           mean_treated = mean_treated,
           diff = diff_std,
           se_ri = ri_test$se,
           p_ri = ri_test$p.value,
           se_lm = summary(lm_test)$coefficients["z",2],
           p_lm = summary(lm_test)$coefficients["z",4]))
})

