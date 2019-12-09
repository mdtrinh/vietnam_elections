library(ri)
library(wfe)

#setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2018/Merge_All.R")

#######

## 22 Dec 2018 Update: This is a simplified modification to Models_LFE_Main
## The analyses in this version is restricted to standard i.e. Neyman inference only
## and use as the main analysis all three elections i.e. the pooled sample.
## Randomization inference version is left for later, such that we won't have to
## think about how to make RI works for standard regressions for now

## In this version I will also commit to the DiDiD framewwork, but will use triple
## interaction instead of bothering with the first differences (to avoid the complication
## with log of the difference vs. difference of the log)

## I'm being lazy here, so I will create the indicators needed to run the regressions
## right inside this file. Ideally they should be in Merge_All.R

## Since the goal is to estimate a DiDiD that compares (difference between pre- and
## post- election year) - (difference between pre- and non-election years) between
## provinces with defeats and provinces without, we need the following indicators

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
  filter(!is.na(net.trans.change.log))

lm_all_1 <- lm(net.trans.change.log ~ defeat + defeat.2007 + defeat.2011 + defeat.2016 +
                 factor(prov) + factor(year),
               data = dat_all_1) # just lm
summary(lm_all_1)

# 2016 only -- positive effect << USE THIS!
dat_2016_1 <- dat_lme %>%
  filter(year < 2018 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

# good results both with and without added covariates

lm_2016_1 <- lm(net.trans.change.log ~ defeat + defeat.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1)

lm_2016_1 <- lm(net.trans.change ~ defeat + defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1)

# 2011 only -- no effect
dat_2011_1 <- dat_lme %>%
  filter(year < 2013 & year > 2008) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

lm_2011_1 <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                  num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 + num.candidates.2011 +
                  factor(prov) + factor(year),
                data = dat_2011_1) # just lm
summary(lm_2011_1)

# 2007 only -- no effect
dat_2007_1 <- dat_lme %>%
  filter(year <= 2008) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

lm_2007_1 <- lm(net.trans.change.log ~ defeat + defeat.2007 +
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
  filter(!is.na(net.trans.change.log))

# good results both with and without added covariates

lm_2016_p <- lm(net.trans.change.log ~ defeat + defeat.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_p)

lm_2016_p <- lm(net.trans.change.log ~ defeat + defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_p)

# 2011 only -- no effect
dat_2011_p <- dat_lme %>%
  filter(year > 2008 & year < 2017) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

lm_2011_p <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                  num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 + num.candidates.2011 +
                  factor(prov) + factor(year),
                data = dat_2011_p) # just lm
summary(lm_2011_p)

# 2007 only -- no effect
dat_2007_p <- dat_lme %>%
  filter(year < 2011) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

lm_2007_p <- lm(net.trans.change.log ~ defeat + defeat.2007 +
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
  filter(!is.na(net.trans.change.log))

# good results both with and without added covariates

lm_2016_1_placebo <- lm(net.trans.change.log ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_1_placebo) # just lm
summary(lm_2016_1_placebo)

lm_2016_1_placebo <- lm(net.trans.change.log ~ defeat_placebo + defeat.2016 +
                          num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_1_placebo) # just lm
summary(lm_2016_1_placebo)

## persistent change

# 2016 only -- positive effect << USE THIS
dat_2016_p_placebo <- dat_lme %>%
  mutate(defeat_placebo = defeat.2016*(year >= 2014)) %>%
  filter(year > 2012 & year < 2016) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

# good results both with and without added covariates

lm_2016_p_placebo <- lm(net.trans.change.log ~ defeat_placebo + defeat.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_p_placebo) # just lm
summary(lm_2016_p_placebo)

lm_2016_p_placebo <- lm(net.trans.change.log ~ defeat_placebo + defeat.2016 +
                          num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_p_placebo) # just lm
summary(lm_2016_p_placebo)
