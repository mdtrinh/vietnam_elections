library(Synth)
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

#######

### prep data for Synthetic control
plan.Synth <- plan %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>% ## smaller control
  filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
  filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
  group_by(prov) %>%
  mutate(defeat.lead = lead(defeat, order_by=year)) %>%
  mutate(defeat.lead = ifelse(is.na(defeat.lead), 0, defeat.lead)) %>%
  mutate(defeat.lead2 = lead(defeat, order_by=year, n = 2)) %>%
  mutate(defeat.lead2 = ifelse(is.na(defeat.lead2), 0, defeat.lead2)) %>%
  ungroup %>%
  mutate(net.trans.change = neglog(net.trans.change)) %>%
  mutate(total.rev = neglog(total.rev)) %>%
  mutate(total.rev.lag = neglog(total.rev.lag)) %>%
  mutate(net.trans = neglog(net.trans)) %>%
  mutate(prov.num = as.numeric(as.factor(prov)))


## Match on exact histories

# 2016
Synth.att(data = plan.Synth  %>% filter(closewin.2016==1 | defeat.2016==1),
          outcome = "net.trans.change",
          treatment = "defeat",
          covs = c("total.rev", "total.rev.lag", "defeat.2007", "defeat.2011"),
          treatment.year = 2017,
          pretreatment.year = c(2007:2016),
          posttreatment.year = NULL,
          unit.variable = "prov.num",
          unit.names.variable = "prov",
          time.variable = "year",
          include.past.Y = TRUE,
          snowfall = TRUE)
Synth.att.2016 <- riSynth(data = plan.Synth %>% filter(closewin.2016==1 | defeat.2016==1),
                          outcome = "net.trans.change",
                          treatment = "defeat",
                          covs = c("total.rev", "total.rev.lag", "defeat.2007", "defeat.2011"),
                          treatment.year = 2017,
                          pretreatment.year = c(2007:2016),
                          posttreatment.year = NULL,
                          unit.variable = "prov.num",
                          unit.names.variable = "prov",
                          time.variable = "year",
                          include.past.Y = TRUE,
                          snowfall = TRUE)

# 2011
Synth.att(data = plan.Synth %>% filter(closewin.2011==1 | defeat.2011==1),
          outcome = "net.trans.change",
          treatment = "defeat",
          covs = c("total.rev", "total.rev.lag", "defeat.2007"),
          treatment.year = 2012,
          pretreatment.year = c(2007:2011),
          posttreatment.year = c(2013:2016),
          unit.variable = "prov.num",
          unit.names.variable = "prov",
          time.variable = "year",
          include.past.Y = TRUE,
          snowfall = TRUE)
Synth.att.2011 <- riSynth(data = plan.Synth %>% filter(closewin.2011==1 | defeat.2011==1),
                          outcome = "net.trans.change",
                          treatment = "defeat",
                          covs = c("total.rev", "total.rev.lag", "defeat.2007"),
                          treatment.year = 2012,
                          pretreatment.year = c(2007:2011),
                          posttreatment.year = c(2013:2017),
                          unit.variable = "prov.num",
                          unit.names.variable = "prov",
                          time.variable = "year",
                          include.past.Y = TRUE,
                          snowfall = TRUE)

## Demonstration for individual provinces

# Tra Vinh, 2016
dataprep.obj <- dataprep(foo = plan.Synth,
                         predictors = c("total.rev", "total.rev.lag", 
                                        "net.trans", # outcome history
                                        "defeat.2007", "defeat.2011"), # treatment history
                         predictors.op = "mean",
                         dependent = "net.trans",
                         unit.variable = "prov.num",
                         unit.names.variable = "prov",
                         time.variable = "year",
                         treatment.identifier = "Tra Vinh",
                         controls.identifier = unique(plan.Synth$prov.num[which(plan.Synth$defeat.2016==0)]),
                         time.predictors.prior = c(2007:2016),
                         time.optimize.ssr = c(2007:2016),
                         time.plot=c(2007:2017))

synth.out <- synth(dataprep.obj)

synth.tables <- synth.tab(
  dataprep.res = dataprep.obj,
  synth.res = synth.out)
print(synth.tables)

path.plot(dataprep.res = dataprep.obj, synth.res = synth.out)
gaps.plot(dataprep.res = dataprep.obj, synth.res = synth.out)

# Phu Yen, 2016
dataprep.obj <- dataprep(foo = plan.Synth,
                         predictors = c("total.rev", "total.rev.lag", 
                                        "net.trans", # outcome history
                                        "defeat.2007", "defeat.2011"), # treatment history
                         predictors.op = "mean",
                         dependent = "net.trans",
                         unit.variable = "prov.num",
                         unit.names.variable = "prov",
                         time.variable = "year",
                         treatment.identifier = "Phu Yen",
                         controls.identifier = unique(plan.Synth$prov.num[which(plan.Synth$defeat.2016==0)]),
                         time.predictors.prior = c(2007:2016),
                         time.optimize.ssr = c(2007:2016),
                         time.plot=c(2007:2017))

synth.out <- synth(dataprep.obj)

synth.tables <- synth.tab(
  dataprep.res = dataprep.obj,
  synth.res = synth.out)
print(synth.tables)

path.plot(dataprep.res = dataprep.obj, synth.res = synth.out)
gaps.plot(dataprep.res = dataprep.obj, synth.res = synth.out)

# Bac Kan, 2011
dataprep.obj <- dataprep(foo = plan.Synth,
                         predictors = c("total.rev", "total.rev.lag", 
                                        "net.trans", # outcome history
                                        "defeat.2007"), # treatment history
                         predictors.op = "mean",
                         dependent = "net.trans",
                         unit.variable = "prov.num",
                         unit.names.variable = "prov",
                         time.variable = "year",
                         treatment.identifier = "Bac Kan",
                         controls.identifier = unique(plan.Synth$prov.num[which(plan.Synth$defeat.2011==0)]),
                         time.predictors.prior = c(2007:2011),
                         time.optimize.ssr = c(2007:2011),
                         time.plot=c(2007:2017))

synth.out <- synth(dataprep.obj)

synth.tables <- synth.tab(
  dataprep.res = dataprep.obj,
  synth.res = synth.out)
print(synth.tables)

path.plot(dataprep.res = dataprep.obj, synth.res = synth.out)
gaps.plot(dataprep.res = dataprep.obj,synth.res = synth.out)
