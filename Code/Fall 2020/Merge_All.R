library(dplyr)
select <- dplyr::select
library(stringdist)
library(vietnamcode)

setwd("../Working Data")

#source("../../Code/Fall 2020/Functions.R") # already called within each file below
source("../../Code/Fall 2020/Clean_Budget.R")
source("../../Code/Fall 2020/Clean_Profile.R")
source("../../Code/Fall 2020/Clean_Results.R")
source("../../Code/Fall 2020/Clean_Leaders.R")

###### 2007 ######

## Merge Results with Profiles

candidates2007 <- profile2007

# standardize province names to facilitate merge
candidates2007$prov <- vietnamcode(candidates2007$prov, origin = "province_name", destination = "province_name")
result2007$prov <- vietnamcode(result2007$prov, origin = "province_name", destination = "province_name")

# create group variables based on province and district
#candidates2007$prov.dist <- interaction(candidates2007$prov, candidates2007$district)
#result2007$prov.dist <- interaction(result2007$prov, result2007$district)

# merge all percentages from results
candidates2007$percentage <- sapply(1:nrow(candidates2007), function (i) {
  x <- candidates2007[i,]
  y <- result2007[result2007$prov==x$prov & result2007$district==x$district,]

  y[bestmatch(x$name,y$name), "percentage"]
})

# merge transfer revenues from budget
#candidates2007 <- merge(candidates2007, final %>% filter(year==2007), by="prov", all.x=T)

## Generate variables of interest
candidates2007$result <- as.numeric(!is.na(candidates2007$percentage))

candidates2007$defeat <- as.numeric(candidates2007$centralnominated==1 &
                                      candidates2007$result==0)
candidates2007$closewin <- as.numeric(candidates2007$centralnominated==1 &
                                        candidates2007$percentage < 60)

# For 2011, calculate max and min range for unobserved vote share of defeated candidates
candidates2007 <- candidates2007 %>%
  group_by(prov, district) %>%
  mutate(percentage.winner = ifelse(result == 1, percentage, NA),
         #percentage.loser = ifelse(result == 0, percentage, NA),
         #percentage.central = ifelse(centralnominated == 1, percentage, NA),
         #percentage.local = ifelse(centralnominated == 0, percentage, NA),
         #share.central.min = min(percentage.central, na.rm=T),
         #share.loser.max = max(percentage.loser, na.rm=T),
         #share.winner.min = min(percentage.winner, na.rm=T),
         num.candidates = as.numeric(max(id)),
         num.seats = case_when(num.candidates == 3 ~ 2,
                               num.candidates == 4 ~ 2,
                               num.candidates == 5 ~ 3,
                               num.candidates == 6 ~ 3),
         num.elected = sum(result),
         num.unelected = num.candidates - num.elected,
         percentage.winner.total = sum(percentage.winner, na.rm = TRUE),
         percentage.loser.total = 100*num.seats - sum(percentage.winner, na.rm = TRUE),
         # possible to calculate max and min vote share of best-performing losers
         percentage.toploser.max = min(percentage.winner, na.rm = TRUE),
         percentage.toploser.min = percentage.loser.total/(num.candidates - num.seats))

###### 2011 ######

## Merge Results with Profiles

candidates2011 <- profile2011

# standardize province names to facilitate merge
candidates2011$prov <- vietnamcode(candidates2011$prov, origin = "province_name", destination = "province_name")
result2011$prov <- vietnamcode(result2011$prov, origin = "province_name", destination = "province_name")

# merge all percentages from results
candidates2011$percentage <- sapply(1:nrow(candidates2011), function (i) {
  x <- candidates2011[i,]
  y <- result2011[result2011$prov==x$prov & result2011$birthyear==x$birthyear,]

  y[bestmatch(x$name,y$name), "percentage"]
})

# merge transfer revenues from budget
#candidates2011 <- merge(candidates2011, final %>% filter(year==2012), by="prov", all.x=T)

## Generate variables of interest
candidates2011$result <- as.numeric(!is.na(candidates2011$percentage))

candidates2011$defeat <- as.numeric(candidates2011$centralnominated==1 &
                                      candidates2011$result==0)
candidates2011$closewin <- as.numeric(candidates2011$centralnominated==1 &
                                        candidates2011$percentage < 60)

# For 2011, calculate max and min range for unobserved vote share of defeated candidates
candidates2011 <- candidates2011 %>%
  group_by(prov, district) %>%
  mutate(percentage.winner = ifelse(result == 1, percentage, NA),
         #percentage.loser = ifelse(result == 0, percentage, NA),
         #percentage.central = ifelse(centralnominated == 1, percentage, NA),
         #percentage.local = ifelse(centralnominated == 0, percentage, NA),
         #share.central.min = min(percentage.central, na.rm=T),
         #share.loser.max = max(percentage.loser, na.rm=T),
         #share.winner.min = min(percentage.winner, na.rm=T),
         num.candidates = as.numeric(max(id)),
         num.seats = case_when(num.candidates == 3 ~ 2,
                               num.candidates == 4 ~ 2,
                               num.candidates == 5 ~ 3,
                               num.candidates == 6 ~ 3),
         # Some districts with 3 seats actually have only 4 candidates
         num.seats = ifelse(num.candidates == 4 &
                              prov %in% c("Hai Duong", "Lao Cai"), 3, num.seats),
         num.elected = sum(result),
         num.unelected = num.candidates - num.elected,
         percentage.winner.total = sum(percentage.winner, na.rm = TRUE),
         percentage.loser.total = 100*num.seats - sum(percentage.winner, na.rm = TRUE),
         # possible to calculate max and min vote share of best-performing losers
         percentage.toploser.max = min(percentage.winner, na.rm = TRUE),
         percentage.toploser.min = percentage.loser.total/(num.candidates - num.seats)) %>%
  drop_na(prov)

###### 2016 ######

## Merge Results with Profiles

candidates2016 <- profile2016

# standardize province names to facilitate merge
candidates2016$prov <- vietnamcode(candidates2016$prov, origin = "province_name", destination = "province_name")
result2016$prov <- vietnamcode(result2016$prov, origin = "province_name", destination = "province_name")

# merge results over -- result file uses same id
candidates2016 <- merge(candidates2016, result2016, by = c("prov", "district", "id"), suffixes = c("",".result"))

# merge transfer revenues from budget
#candidates2016 <- merge(candidates2016, plan %>% filter(year==2016), by="prov", all.x=T)

## Generate variables of interest
candidates2016$result <- as.numeric(candidates2016$result)

candidates2016$defeat <- as.numeric(candidates2016$centralnominated==1 &
                                      candidates2016$result==0)
candidates2016$closewin <- as.numeric(candidates2016$centralnominated==1 &
                                        candidates2016$result==1 &
                                        candidates2016$percentage < 60) # In 2016 percentage is available for defeated ppl

# For 2016, possible to calculate true margins and hence "true" close wins and close defeats
candidates2016 <- candidates2016 %>%
  group_by(prov, district) %>%
  mutate(percentage.winner = ifelse(result == 1, percentage, NA),
         percentage.loser = ifelse(result == 0, percentage, NA),
         percentage.central = ifelse(centralnominated == 1, percentage, NA),
         percentage.local = ifelse(centralnominated == 0, percentage, NA),
         share.central.min = min(percentage.central, na.rm=T),
         share.loser.max = max(percentage.loser, na.rm=T),
         share.winner.min = min(percentage.winner, na.rm=T),
         num.candidates = as.numeric(max(id)),
         num.seats = case_when(num.candidates == 3 ~ 2,
                               num.candidates == 4 ~ 2,
                               num.candidates == 5 ~ 3,
                               num.candidates == 6 ~ 3),
         # In Ha Nam districts with 4 candidates actually have 3 seats
         num.seats = ifelse(num.candidates == 4 &
                              prov %in% c("Ha Nam"), 3, num.seats),
         # In Soc Trang 2 districts with 5 candidates have 2 seats
         num.seats = ifelse(num.candidates == 5 &
                              prov %in% c("Soc Trang") &
                              district %in% c(2,3), 2, num.seats),
         num.elected = sum(result),
         num.unelected = num.candidates - num.elected) %>%
  # individual margin
  mutate(margin = (1-defeat)*(percentage - pmax(share.loser.max, 50)) +
           (defeat)*ifelse(num.seats > num.elected,
                           percentage - 50,
                           percentage - share.winner.min)) %>%
  # candidate ranking among winners and losers 
  # s.t. close wins and defeats are only counted among those closest to winning and losing
  group_by(prov, district, result) %>%
  mutate(rank = rank(percentage)) %>%
  # a closewin.true variable to test coding of closewin
  mutate(closewin.true = as.numeric(centralnominated == 1 & result == 1 & margin < 10 
                                    & (rank == min(rank) | percentage < 60))) %>%
  # a defeat.true variable to test coding of defeat
  mutate(defeat.true = as.numeric(centralnominated == 1 & result == 0 & margin > -10)) %>%
  ungroup

##### Summaries #####

#### district-level summaries ####

# note: share.loser needs to be calculated as (total possible share - share.winner),
districts2007 <- candidates2007 %>%
  group_by(prov, district) %>%
  summarise(num.candidates = as.numeric(max(id)),
            num.elected = sum(result),
            num.unelected = max(num.unelected),
            num.seats = case_when(num.candidates == 4 ~ 2,
                                  num.candidates == 5 ~ 3,
                                  num.candidates == 6 ~ 3),
            percentage.loser.total = max(percentage.loser.total),
            percentage.toploser.max = max(percentage.toploser.max),
            percentage.toploser.min = max(percentage.toploser.min),
            num.centralnominees = sum(centralnominated),
            any.centralnominees = max(centralnominated),
            share.winner = sum(percentage * result, na.rm=T),
            share.central = ifelse(sum(percentage * centralnominated, na.rm=T) > 0,
                                   sum(percentage * centralnominated, na.rm=T),
                                   NA),
            share.local = sum(percentage * (1-centralnominated), na.rm=T),
            share.winner.mean = share.winner/num.elected,
            share.central.mean = share.central/num.centralnominees,
            share.local.mean = share.central/(num.candidates - num.centralnominees),
            power.central = sum(power*centralnominated, na.rm=T)/sum(centralnominated),
            defeat = max(defeat, na.rm=T),
            closewin = max(closewin, na.rm=T),
            num.defeat = sum(defeat, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            year = 2007)

districts2007 %>%
  filter(defeat==1) %>%
  as.data.frame

districts2011 <- candidates2011 %>%
  group_by(prov, district) %>%
  summarise(num.candidates = as.numeric(max(id)),
            num.elected = sum(result),
            num.unelected = max(num.unelected),
            num.seats = max(num.seats),
            percentage.loser.total = max(percentage.loser.total),
            percentage.toploser.max = max(percentage.toploser.max),
            percentage.toploser.min = max(percentage.toploser.min),
            num.centralnominees = sum(centralnominated),
            any.centralnominees = max(centralnominated),
            share.winner = sum(percentage * result, na.rm=T),
            share.central = ifelse(sum(percentage * centralnominated, na.rm=T) > 0,
                                   sum(percentage * centralnominated, na.rm=T),
                                   NA),
            share.local = sum(percentage * (1-centralnominated), na.rm=T),
            share.winner.mean = share.winner/num.elected,
            share.central.mean = share.central/num.centralnominees,
            share.local.mean = share.central/(num.candidates - num.centralnominees),
            power.central = sum(power*centralnominated, na.rm=T)/sum(centralnominated),
            defeat = max(defeat, na.rm=T),
            closewin = max(closewin, na.rm=T),
            num.defeat = sum(defeat, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            year = 2011)

districts2011 %>%
  filter(defeat==1) %>%
  as.data.frame

districts2016 <- candidates2016 %>%
  group_by(prov, district) %>%
  summarise(num.candidates = as.numeric(max(id)),
            num.elected = sum(result),
            num.seats = max(num.seats),
            num.unelected = max(num.unelected),
            num.centralnominees = sum(centralnominated),
            any.centralnominees = max(centralnominated),
            share.winner = sum(percentage * result, na.rm=T),
            share.loser = sum(percentage * (1-result), na.rm=T),
            share.central = ifelse(sum(percentage * centralnominated, na.rm=T) > 0,
                                   sum(percentage * centralnominated, na.rm=T),
                                   NA),
            share.local = sum(percentage * (1-centralnominated), na.rm=T),
            share.writein = num.seats*100 - (share.winner + share.loser),
            share.winner.mean = share.winner/num.elected,
            share.central.mean = share.central/num.centralnominees,
            share.local.mean = share.central/(num.candidates - num.centralnominees),
            power.central = sum(power*centralnominated, na.rm=T)/sum(centralnominated),
            # at district level, consider only defeat margin in case there's both a defeat and a close win
            # i.e. district with any defeat is considered treated, even if it also has a close win
            # a district's margin is the margin of the district's worst central nominees
            # i.e. how close a district is to completely averting any defeat
            margin = min(margin*ifelse(centralnominated, 1, NA), na.rm = T), # 2016 only
            defeat = max(defeat, na.rm=T),
            defeat.true = max(defeat.true, na.rm = T), # 2016 only
            closewin = max(closewin, na.rm=T),
            closewin.true = max(closewin.true, na.rm = T), # 2016 only
            num.defeat = sum(defeat, na.rm=T),
            num.defeat.true = sum(defeat.true, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            num.closewin.true = sum(closewin.true, na.rm=T), # 2016 only
            year = 2016)

districts2016 %>%
  filter(defeat==1) %>%
  as.data.frame


#### province-level summaries

## 26 feb 18 update: create province level summaries from district-level summaries now
provinces2007 <- districts2007 %>%
  group_by(prov) %>%
  summarise(num.districts = n(),
            num.districts.3 = sum(num.candidates == 3),
            num.districts.4 = sum(num.candidates == 4),
            num.districts.5 = sum(num.candidates == 5),
            num.districts.6 = sum(num.candidates == 6),
            any.districts.3 = max(num.candidates == 3),
            any.districts.4 = max(num.candidates == 4),
            any.districts.5 = max(num.candidates == 5),
            any.districts.6 = max(num.candidates == 6),
            share.central.mean = weighted.mean(share.central.mean, w = num.centralnominees, na.rm = T),
            share.local.mean = weighted.mean(share.local.mean, w = num.candidates - num.centralnominees, na.rm = T),
            num.candidates = sum(num.candidates),
            num.elected = sum(num.elected),
            num.seats = sum(num.seats),
            num.centralnominees = sum(num.centralnominees),
            any.centralnominees = max(any.centralnominees),
            defeat = max(defeat, na.rm=T),
            closewin = max(closewin, na.rm=T),
            num.defeat = sum(defeat, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            year = 2007)

provinces2011 <- districts2011 %>%
  group_by(prov) %>%
  summarise(num.districts = n(),
            num.districts.3 = sum(num.candidates == 3),
            num.districts.4 = sum(num.candidates == 4),
            num.districts.5 = sum(num.candidates == 5),
            num.districts.6 = sum(num.candidates == 6),
            any.districts.3 = max(num.candidates == 3),
            any.districts.4 = max(num.candidates == 4),
            any.districts.5 = max(num.candidates == 5),
            any.districts.6 = max(num.candidates == 6),
            share.central.mean = weighted.mean(share.central.mean, w = num.centralnominees, na.rm = T),
            share.local.mean = weighted.mean(share.local.mean, w = num.candidates - num.centralnominees, na.rm = T),
            num.candidates = sum(num.candidates),
            num.elected = sum(num.elected),
            num.seats = sum(num.seats),
            num.centralnominees = sum(num.centralnominees),
            any.centralnominees = max(any.centralnominees),
            defeat = max(defeat, na.rm=T),
            closewin = max(closewin, na.rm=T),
            num.defeat = sum(defeat, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            year = 2011)

provinces2016 <- districts2016 %>%
  group_by(prov) %>%
  summarise(num.districts.3 = sum(num.candidates == 3),
            num.districts.4 = sum(num.candidates == 4),
            num.districts.5 = sum(num.candidates == 5),
            num.districts.6 = sum(num.candidates == 6),
            any.districts.3 = max(num.candidates == 3),
            any.districts.4 = max(num.candidates == 4),
            any.districts.5 = max(num.candidates == 5),
            any.districts.6 = max(num.candidates == 6),
            share.central.mean = weighted.mean(share.central.mean, w = num.centralnominees, na.rm = T),
            share.local.mean = weighted.mean(share.local.mean, w = num.candidates - num.centralnominees, na.rm = T),
            num.centralnominees = sum(num.centralnominees),
            any.centralnominees = max(any.centralnominees),
            num.candidates = sum(num.candidates),
            num.elected = sum(num.elected),
            num.seats = sum(num.seats),
            num.districts = n(),
            # at district level, consider only defeat margin in case there's both a defeat and a close win
            # i.e. district with any defeat is considered treated, even if it also has a close win
            # a district's margin is the margin of the district's worst central nominees
            # i.e. how close a district is to completely averting any defeat
            margin = min(margin, na.rm = T), # 2016 only
            defeat = max(defeat, na.rm=T),
            defeat.true = max(defeat.true, na.rm = T), # 2016 only
            closewin = max(closewin, na.rm=T),
            closewin.true = max(closewin.true, na.rm = T), # 2016 only
            num.defeat = sum(defeat, na.rm=T),
            num.defeat.true = sum(defeat.true, na.rm=T),
            num.closewin = sum(closewin, na.rm=T),
            num.closewin.true = sum(closewin.true, na.rm=T), # 2016 only
            year = 2016)


##### Merge Treatment with Budget Data #####
plan <- plan %>%
  left_join(provinces2007 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
  left_join(provinces2011 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
  left_join(provinces2016 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
  left_join(bind_rows(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
  mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
  mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
  group_by(prov) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(lag(., n=1, order_by = year))) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(ifelse(is.na(.), 0, .))) %>%
  ungroup %>%
  as.data.frame

final <- final %>%
  left_join(provinces2007 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
  left_join(provinces2011 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
  left_join(provinces2016 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
  left_join(bind_rows(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
  mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
  mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
  group_by(prov) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(lag(., n=1, order_by = year))) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(ifelse(is.na(.), 0, .))) %>%
  ungroup %>%
  as.data.frame

prov <- prov %>%
  left_join(provinces2007 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
  left_join(provinces2011 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
  left_join(provinces2016 %>%
          setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
  left_join(bind_rows(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
  mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
  mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
  group_by(prov) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(lag(., n=1, order_by = year))) %>%
  mutate_at(vars(num.districts:num.closewin.true), funs(ifelse(is.na(.), 0, .))) %>%
  ungroup %>%
  as.data.frame

#### overview of treatment assignment

plan %>%
  filter(year==2008) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  arrange(defeat.2007,prov) %>%
  select(prov, defeat.2007, closewin.2007, net.trans.change, net.trans.change.pct)

plan %>%
  filter(year==2012) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  arrange(defeat.2011,prov) %>%
  select(prov, defeat.2011, closewin.2011, net.trans.change, net.trans.change.pct)

plan %>%
  filter(year==2018) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  arrange(defeat.2016,prov) %>%
  dplyr::select(prov, defeat.2016, closewin.2016, net.trans.change, net.trans.change.pct)

plan %>%
  filter(year==2018) %>%
  filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  arrange(defeat.true.2016,prov) %>%
  dplyr::select(prov, defeat.true.2016, closewin.true.2016, net.trans.change, net.trans.change.pct)

plan %>%
  filter(year==2007) %>%
  mutate(sum.defeat = defeat.2007 + defeat.2011 + defeat.2016) %>%
  mutate(sum.closewin = closewin.2007 + closewin.2011 + closewin.2016) %>%
  filter(sum.defeat > 0 | sum.closewin >0) %>%
  select(prov, defeat.2007, defeat.2011, defeat.2016, sum.defeat, sum.closewin)


#### Merge Treatment with promotion data
leaders2007 <- leaders2007 %>%
  merge(provinces2007 %>% select(-year), by = "prov", all.x=T) %>%
  arrange(prov, year)

leaders2011 <- leaders2011 %>%
  merge(provinces2011 %>% select(-year), by = "prov", all.x=T) %>%
  arrange(prov, year)

leaders2006 <- leaders2006 %>%
  merge(provinces2007 %>% select(-year), by = "prov", all.x=T) %>%
  arrange(prov, year)

leaders2010 <- leaders2010 %>%
  merge(provinces2011 %>% select(-year), by = "prov", all.x=T) %>%
  arrange(prov, year)

#### Return working data to original folder
setwd("../Working Data")
