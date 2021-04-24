library(ri)
library(wfe)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Fall 2020/Merge_All.R")

#######

## 10 Mar 2019: This is a branch out from Models_LFE_lm version 10Mar2019
## In this version all analyses are taken without taking the log

## Also experiment using province-specific time trend instead of first-differenced
## outcomes since the two make similar assumptions
## (testing this only for the 2016 results we know to be good...)

## 14 Mar 2019: not using this even though most of the results are good,
## since doing the standard outcome version kills the synthetic control results

## 29 May 2019: most results no longer significant once standard errors
## are clustered at province level

# dat_lme <- plan %>% 
#   ## Indicator for post-election year
#   mutate(post_election.2007 = as.numeric(year == 2008),
#          post_election.2011 = as.numeric(year == 2012),
#          post_election.2016 = as.numeric(year == 2017),
#          post_election = as.numeric(post_election.2007 | post_election.2011 | post_election.2016)) %>%
#   ## Indicator for pre-and-post election years combined
#   mutate(election.2007 = as.numeric(year == 2008 | year == 2007),
#          election.2011 = as.numeric(year == 2012 | year == 2011),
#          election.2016 = as.numeric(year == 2017 | year == 2016),
#          election = as.numeric(election.2007 | election.2011 | election.2016)) %>%
#   ## Indicator for defeat status that applies only to pre-election years and one post-eleciton year
#   mutate(ever.defeat.2007 = as.numeric((year <= 2008 | year > 2003) & defeat.2007 == 1),
#          ever.defeat.2011 = as.numeric((year <= 2012 | year > 2008) & defeat.2011 == 1),
#          ever.defeat.2016 = as.numeric((year <= 2017 | year > 2013) & defeat.2016 == 1),
#          ever.defeat = as.numeric(ever.defeat.2007 | ever.defeat.2011 | ever.defeat.2016)) 


## 22 Dec 2018 latest update: Switching back to doing the first difference now

dat_lme <- plan

## One-year change, linear fixed effects panel regression

# note that in most of the below regressions, adding covariates
# pertaining to competitiveness of elections does not change
# the results

# all three elections in one regression -- no effect
dat_all_1 <- dat_lme %>%
  filter(year < 2018) %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

lm_all_1 <- lm(net.trans.change ~ defeat + defeat.2007 + defeat.2011 + defeat.2016 +
                 factor(prov) + factor(year),
               data = dat_all_1) # just lm
summary(lm_all_1)

# 2016 only -- positive effect << USE THIS!
dat_2016_1 <- dat_lme %>%
  filter(year < 2018 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

# good results both with and without added covariates

lm_2016_1 <- lm(net.trans.change ~ defeat + defeat.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1)

lm_2016_1 <- lm(net.trans.change ~ defeat + defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1)

# with time trend
lm_2016_1 <- lm(net.trans ~ defeat + defeat.2016 +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat_2016_1) # just lm
summary(lm_2016_1)

# 2011 only -- no effect
dat_2011_1 <- dat_lme %>%
  filter(year < 2013 & year > 2008) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

lm_2011_1 <- lm(net.trans.change ~ defeat + defeat.2011 +
                  num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 + num.candidates.2011 +
                  factor(prov) + factor(year),
                data = dat_2011_1) # just lm
summary(lm_2011_1)

# 2007 only -- no effect
dat_2007_1 <- dat_lme %>%
  filter(year <= 2008) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

lm_2007_1 <- lm(net.trans.change ~ defeat + defeat.2007 +
                  num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 + num.candidates.2007 +
                  factor(prov) + factor(year),
                data = dat_2007_1) # just lm
summary(lm_2007_1)

## persistent change

# 2016 only -- positive effect << USE THIS
dat_2016_p <- dat_lme %>%
  mutate(defeat = defeat.2016*(year >= 2017)) %>%
  filter(year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

# good results both with and without added covariates

lm_2016_p <- lm(net.trans.change ~ defeat + defeat.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_p)

lm_2016_p <- lm(net.trans.change ~ defeat + defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_p)

# with time trend
lm_2016_p <- lm(net.trans ~ defeat + defeat.2016 +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat_2016_p) # just lm
summary(lm_2016_p)

# 2011 only -- no effect
dat_2011_p <- dat_lme %>%
  filter(year > 2008 & year < 2017) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

lm_2011_p <- lm(net.trans.change ~ defeat + defeat.2011 +
                  num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 + num.candidates.2011 +
                  factor(prov) + factor(year),
                data = dat_2011_p) # just lm
summary(lm_2011_p)

# 2007 only -- no effect
dat_2007_p <- dat_lme %>%
  filter(year < 2011) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

lm_2007_p <- lm(net.trans.change ~ defeat + defeat.2007 +
                  num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 + num.candidates.2007 +
                  factor(prov) + factor(year),
                data = dat_2007_p) # just lm
summary(lm_2007_p)


#### PLACEBO CHECKS

## one-year effect -- good results for 2016!

# 2016 only 
dat_2016_1_placebo <- dat_lme %>%
  mutate(defeat_placebo = defeat.2016 * as.numeric(year == 2014)) %>%
  filter(year < 2015 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

# good results both with and without added covariates

lm_2016_1_placebo <- lm(net.trans.change ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_1_placebo) # just lm
summary(lm_2016_1_placebo)

lm_2016_1_placebo <- lm(net.trans.change ~ defeat_placebo + defeat.2016 +
                          num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_1_placebo) # just lm
summary(lm_2016_1_placebo)

# with time trend -- not possible due to insufficient dof
lm_2016_1_placebo <- lm(net.trans ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year) + factor(prov)*year,
                        data = dat_2016_1_placebo) # just lm
summary(lm_2016_1_placebo)

## persistent change

# 2016 only -- positive effect << USE THIS
dat_2016_p_placebo <- dat_lme %>%
  mutate(defeat_placebo = defeat.2016*(year >= 2014)) %>%
  filter(year > 2012 & year < 2016) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change))

# good results both with and without added covariates

lm_2016_p_placebo <- lm(net.trans.change ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_p_placebo) # just lm
summary(lm_2016_p_placebo)

lm_2016_p_placebo <- lm(net.trans.change ~ defeat_placebo + defeat.2016 +
                          num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_p_placebo) # just lm
summary(lm_2016_p_placebo)

# with time trend
lm_2016_p_placebo <- lm(net.trans ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year) + factor(prov)*year,
                        data = dat_2016_p_placebo) # just lm
summary(lm_2016_p_placebo)
