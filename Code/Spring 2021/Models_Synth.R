library(gsynth)
library(panelView)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2021/Merge_All.R")

## This file builds upon the old Analyze_Synth file to conduct
## synthetic control analyses (now using Yiqing Xu's Generalized
## Synthetic Control approach) on 2016 election results

## 10 Mar 2019: After discussion with Sean (see Evernote), stick
## to using only the outcome variables (instead of first differenced)
## in Synthetic control models; also add past treatments

## 13 Mar 2019: go back to using net.trans.change.log to maintain
## consistency with lfe results. Apparently using the straight unlogged
## variable kills all the results even when it improves pre-treatment
## match between treated and control...

### prep data for Synthetic control
dat_synth <- plan %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  mutate(net.trans = net.trans/1000000) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")
panelView(net.trans.log ~ defeat, data = dat_synth, index = c("prov", "year"))

## One year change

dat_2016synth_1 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2017)) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) 
panelView(net.trans.log ~ defeat, data = dat_2016synth_1, index = c("prov", "year"))

# first differenced
system.time(
  synth_2016_1 <- gsynth(net.trans.change.log ~ treat + defeat, 
                         data = dat_2016synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_1)
plot(synth_2016_1)
plot(synth_2016_1, type = "counterfactual")

system.time(
  synth_2016_1 <- gsynth(net.trans.change ~ treat + defeat, 
                         data = dat_2016synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_1)
plot(synth_2016_1)
plot(synth_2016_1, type = "counterfactual")

# no first differenced

system.time(
  synth_2016_1 <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_1)
plot(synth_2016_1)
plot(synth_2016_1, type = "counterfactual")

system.time(
  synth_2016_1 <- gsynth(net.trans ~ treat + defeat, 
                         data = dat_2016synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6)
)

print(synth_2016_1)
plot(synth_2016_1)
plot(synth_2016_1, type = "counterfactual")

## Persistent change

dat_2016synth_p <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) 
panelView(net.trans.log ~ defeat, data = dat_2016synth_p, index = c("prov", "year"))
panelView(net.trans.log ~ defeat, data = dat_2016synth_p, index = c("prov", "year"), type = "raw")

# first differenced
system.time(
  synth_2016_p <- gsynth(net.trans.change.log ~ treat + defeat, 
                         data = dat_2016synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_p)
plot(synth_2016_p)
plot(synth_2016_p, type = "counterfactual")

system.time(
  synth_2016_p <- gsynth(net.trans.change ~ treat + defeat, 
                         data = dat_2016synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_p)
plot(synth_2016_p)
plot(synth_2016_p, type = "counterfactual")


# not first differenced
system.time(
  synth_2016_p <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6)
)

print(synth_2016_p)
plot(synth_2016_p)
plot(synth_2016_p, type = "counterfactual")

system.time(
  synth_2016_p <- gsynth(net.trans ~ treat + defeat, 
                         data = dat_2016synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6)
)

print(synth_2016_p)
plot(synth_2016_p)
plot(synth_2016_p, type = "counterfactual")

## Placebo 1: 2014 treatment, as if election is in 2013

# One year change

dat_2016synth_1_placebo2014 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2014)) %>%
  filter(year < 2015)

# first differenced  
system.time(
  synth_2016_1_placebo2014 <- gsynth(net.trans.change.log ~ treat + defeat, 
                                     data = dat_2016synth_1_placebo2014, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_1_placebo2014)
plot(synth_2016_1_placebo2014)
plot(synth_2016_1_placebo2014, type = "counterfactual")

system.time(
  synth_2016_1_placebo2014 <- gsynth(net.trans.change ~ treat + defeat, 
                                     data = dat_2016synth_1_placebo2014, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_1_placebo2014)
plot(synth_2016_1_placebo2014)
plot(synth_2016_1_placebo2014, type = "counterfactual")

# not first differenced

# Persistent change

dat_2016synth_p_placebo2014 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2014)) %>%
  filter(year < 2016)

system.time(
  synth_2016_p_placebo2014 <- gsynth(net.trans.change.log ~ defeat, 
                         data = dat_2016synth_p_placebo2014, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 5), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_p_placebo2014)
plot(synth_2016_p_placebo2014)

## Placebo 2: 2015 treatment, as if election is in 2014

# One year change

dat_2016synth_1_placebo2015 <- dat_synth %>%
  mutate(defeat = defeat.2016*as.numeric(year==2015)) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2016) %>%
  filter(!is.na(net.trans.log.lag))

system.time(
  synth_2016_1_placebo2015 <- gsynth(net.trans.change.log ~ defeat, 
                                     data = dat_2016synth_1_placebo2015, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6)
)

print(synth_2016_1_placebo2015)
plot(synth_2016_1_placebo2015)

# Persistent change

dat_2016synth_p_placebo2015 <- dat_synth %>%
  mutate(defeat = defeat.2016*as.numeric(year>=2015)) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2017) %>%
  filter(!is.na(net.trans.log.lag))

system.time(
  synth_2016_p_placebo2015 <- gsynth(net.trans.change.log ~ defeat, 
                                     data = dat_2016synth_p_placebo2015, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6)
)

print(synth_2016_p_placebo2015)
plot(synth_2016_p_placebo2015)

## Placebo 3: 2016 treatment, as if election is in 2015

# One year change

dat_2016synth_1_placebo2016 <- dat_synth %>%
  mutate(defeat = defeat.2016*as.numeric(year==2016)) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2017) %>%
  filter(!is.na(net.trans.log.lag))

system.time(
  synth_2016_1_placebo2016 <- gsynth(net.trans.change.log ~ defeat, 
                                     data = dat_2016synth_1_placebo2016, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6)
)

print(synth_2016_1_placebo2016)
plot(synth_2016_1_placebo2016)

# Persistent change -- does not make sense in final model

dat_2016synth_p_placebo2016 <- dat_synth %>%
  mutate(defeat = defeat.2016*as.numeric(year>=2016)) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) %>%
  filter(!is.na(net.trans.log.lag))

system.time(
  synth_2016_p_placebo2016 <- gsynth(net.trans.change.log ~ defeat, 
                                     data = dat_2016synth_p_placebo2016, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 5), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6)
)

print(synth_2016_p_placebo2016)
plot(synth_2016_p_placebo2016)
