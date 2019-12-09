library(dplyr)
select <- dplyr::select
library(stringdist)
library(vietnamcode)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Functions.R")
source("../../Code/SYP/Clean_Budget.R")
source("../../Code/SYP/Clean_Profile.R")
source("../../Code/SYP/Clean_Results.R")
source("../../Code/SYP/Clean_Leaders.R")
source("../../Code/SYP/Clean_PAPI.R")
source("../../Code/SYP/Clean_PCI.R")

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
candidates2007 <- merge(candidates2007, final %>% filter(year==2007), by="prov", all.x=T)

## Generate variables of interest
candidates2007$result <- as.numeric(!is.na(candidates2007$percentage))

candidates2007$defeat <- as.numeric(candidates2007$centralnominated==1 &
                                        candidates2007$result==0)
candidates2007$closewin <- as.numeric(candidates2007$centralnominated==1 &
                                          candidates2007$percentage < 55)

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
candidates2011 <- merge(candidates2011, final %>% filter(year==2012), by="prov", all.x=T)

## Generate variables of interest
candidates2011$result <- as.numeric(!is.na(candidates2011$percentage))

candidates2011$defeat <- as.numeric(candidates2011$centralnominated==1 &
                                        candidates2011$result==0)
candidates2011$closewin <- as.numeric(candidates2011$centralnominated==1 &
                                          candidates2011$percentage < 55)

###### 2016 ######

## Merge Results with Profiles

candidates2016 <- profile2016

# standardize province names to facilitate merge
candidates2016$prov <- vietnamcode(candidates2016$prov, origin = "province_name", destination = "province_name")
result2016$prov <- vietnamcode(result2016$prov, origin = "province_name", destination = "province_name")

# merge results over -- result file uses same id
candidates2016 <- merge(candidates2016, result2016, by = c("prov", "district", "id"), suffixes = c("",".result"))

# merge transfer revenues from budget
candidates2016 <- merge(candidates2016, plan %>% filter(year==2016), by="prov", all.x=T)

## Generate variables of interest
candidates2016$result <- as.numeric(candidates2016$result)

candidates2016$defeat <- as.numeric(candidates2016$centralnominated==1 &
                                        candidates2016$result==0)
candidates2016$closewin <- as.numeric(candidates2016$centralnominated==1 &
                                          candidates2016$result==1 &
                                          candidates2016$percentage < 55) # In 2016 percentage is available for defeated ppl


##### Summaries #####

#### district-level summaries ####
districts2007 <- candidates2007 %>%
    group_by(prov, district) %>%
    summarise(num.candidates = as.numeric(max(id)),
              share.winner = sum(percentage * result, na.rm=T),
              share.loser = sum(result * 100, na.rm=T) - share.winner,
              share.central = sum(percentage * centralnominated, na.rm=T),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
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
              share.winner = sum(percentage * result, na.rm=T),
              share.loser = sum(result * 100, na.rm=T) - share.winner,
              share.central = sum(percentage * centralnominated, na.rm=T),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
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
              share.winner = sum(percentage * result, na.rm=T),
              #share.loser = sum(result * 100, na.rm=T) - share.winner,
              share.central = sum(percentage * centralnominated, na.rm=T),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
              defeat = max(defeat, na.rm=T),
              closewin = max(closewin, na.rm=T),
              num.defeat = sum(defeat, na.rm=T),
              num.closewin = sum(closewin, na.rm=T),
              year = 2016)

districts2016 %>%
    filter(defeat==1) %>%
    as.data.frame

#### province-level summaries
provinces2007 <- candidates2007 %>%
    group_by(prov) %>%
    summarise(num.candidates = n(),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
              defeat = max(defeat, na.rm=T),
              closewin = max(closewin, na.rm=T),
              num.defeat = sum(defeat, na.rm=T),
              num.closewin = sum(closewin, na.rm=T),
              year = 2007)

provinces2011 <- candidates2011 %>%
    group_by(prov) %>%
    summarise(num.candidates = n(),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
              defeat = max(defeat, na.rm=T),
              closewin = max(closewin, na.rm=T),
              num.defeat = sum(defeat, na.rm=T),
              num.closewin = sum(closewin, na.rm=T),
              year = 2011)

provinces2016 <- candidates2016 %>%
    group_by(prov) %>%
    summarise(num.candidates = n(),
              num.elected = sum(result),
              centralnominated=sum(centralnominated),
              defeat = max(defeat, na.rm=T),
              closewin = max(closewin, na.rm=T),
              num.defeat = sum(defeat, na.rm=T),
              num.closewin = sum(closewin, na.rm=T),
              year = 2016)


##### Merge Treatment with Budget Data #####
plan <- plan %>%
    merge(provinces2007 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
    merge(provinces2011 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
    merge(provinces2016 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
    merge(rbind(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
    mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
    mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
    group_by(prov) %>%
    mutate_each(funs(lag(., n=1, order_by = year)), num.candidates:num.closewin) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
    ungroup %>%
    as.data.frame

final <- final %>%
    merge(provinces2007 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
    merge(provinces2011 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
    merge(provinces2016 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
    merge(rbind(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
    mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
    mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
    group_by(prov) %>%
    mutate_each(funs(lag(., n=1, order_by = year)), num.candidates:num.closewin) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
    ungroup %>%
    as.data.frame

prov <- prov %>%
    merge(provinces2007 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2007"))), by = "prov") %>%
    merge(provinces2011 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2011"))), by = "prov") %>%
    merge(provinces2016 %>%
              setNames(c(names(.)[1], paste0(names(.)[-1],".2016"))), by = "prov") %>%
    merge(rbind(provinces2007, provinces2011, provinces2016), by = c("prov", "year"), all.x = T) %>%
    mutate(defeat.lead = ifelse(year==2008, defeat.2011, ifelse(year==2012, defeat.2016, 0))) %>%
    mutate(defeat.lead2 = ifelse(year==2008, defeat.2016, 0)) %>%
    group_by(prov) %>%
    mutate_each(funs(lag(., n=1, order_by = year)), num.candidates:num.closewin) %>%
    mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
    ungroup %>%
    as.data.frame

#### overview of treatment assignment

plan %>%
    filter(year==2008) %>%
    filter(defeat.2007!=0 | closewin.2007!=0) %>%
    filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
    arrange(defeat.2007,prov) %>%
    select(prov, defeat.2007, closewin.2007, net.trans.change, net.trans.change.pct)

plan %>%
    filter(year==2012) %>%
    filter(defeat.2011!=0 | closewin.2011!=0) %>%
    filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
    arrange(defeat.2011,prov) %>%
    select(prov, defeat.2011, closewin.2011, net.trans.change, net.trans.change.pct)

plan %>%
    filter(year==2017) %>%
    filter(defeat.2016!=0 | closewin.2016!=0) %>%
    filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
    arrange(defeat.2016,prov) %>%
    dplyr::select(prov, defeat.2016, closewin.2016, net.trans.change, net.trans.change.pct)

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
