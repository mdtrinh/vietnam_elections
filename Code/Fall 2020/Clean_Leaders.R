library(haven)
library(dplyr)
library(ggplot2)
library(stringr)
library(vietnamcode)
library(vietnamdata)

setwd("../Politician Data/")
source("../../Code/Fall 2020/Functions.R")

#### Import leader profiles

leaders <- read_dta("20150925_promotions_v2.dta")

# clean up province names using a look up table
# clean up province names using the Vietnamcode function
leaders$prov <- vietnamcode(leaders$province, origin = "province_name", destination = "province_name")

# assign IDs to individual, group by ID
leaders <- leaders %>%
    mutate(name = str_trim(name)) %>%
    mutate(id = as.factor(paste(name, birth_year))) %>%
    mutate(stalled = as.numeric(Stalled=="X")) %>%
    mutate(demoted = as.numeric(Stalled=="X")) %>%
    mutate(promoted = as.numeric(dv_promote)) %>%
    filter(!is.na(year))

# percentage serving in hometown in 2015
leaders %>% 
  ungroup() %>% 
  filter(year == 2015) %>% 
  select(id, name, position, province, hometown, nha_que, nha_que2) %>% 
  summarize(hometown = mean(hometown, na.rm = T))

# identify politicians in power during election years
in2007 <- unique(leaders$id[leaders$year==2007]) %>% as.character
in2011 <- unique(leaders$id[leaders$year==2011]) %>% as.character
in2016 <- unique(leaders$id[leaders$year==2016]) %>% as.character # no obs for 2016

prov2007 <- leaders %>% # if politician holds 2 positions in 2007, keep only lesser position
    filter(year==2007) %>%
    dplyr::select(id, prov) %>%
    group_by(id) %>%
    filter(row_number()==1)
prov2011 <- leaders %>% # if politician holds 2 positions in 2011, keep only lesser position
    filter(year==2011) %>%
    select(id, prov) %>%
    group_by(id) %>%
    filter(row_number()==1)

# identify politicians in power during pre-election years
in2006 <- unique(leaders$id[leaders$year==2006]) %>% as.character
in2010 <- unique(leaders$id[leaders$year==2010]) %>% as.character
in2015 <- unique(leaders$id[leaders$year==2015]) %>% as.character

prov2006 <- leaders %>% # if politician holds 2 positions in 2006, keep only lesser position
    filter(year==2006) %>%
    dplyr::select(id, prov) %>%
    group_by(id) %>%
    filter(row_number()==1)
prov2010 <- leaders %>% # if politician holds 2 positions in 2010, keep only lesser position
    filter(year==2010) %>%
    select(id, prov) %>%
    group_by(id) %>%
    filter(row_number()==1)

# merge all in one single leader dataset
leaders <- leaders %>%
    mutate(power.2007 = as.numeric(id %in% in2007)) %>%
    mutate(power.2011 = as.numeric(id %in% in2011)) %>%
    mutate(power.2006 = as.numeric(id %in% in2006)) %>%
    mutate(power.2010 = as.numeric(id %in% in2010)) %>%
    merge(prov2007, by="id", suffixes=c("",".2007"), all.x=T) %>%
    merge(prov2011, by="id", suffixes=c("",".2011"), all.x=T) %>%
    merge(prov2006, by="id", suffixes=c("",".2006"), all.x=T) %>%
    merge(prov2010, by="id", suffixes=c("",".2010"), all.x=T) %>%
    group_by(id, year) %>%
    arrange(-promoted) %>% # if promoted in the year, mark as yes
    filter(row_number()==1)

# promotion history for 2007 leaders
leaders2007 <- leaders %>%
    filter(power.2007==1) %>%
    group_by(prov.2007, year) %>%
    summarise(num.leaders = n(),
              stalled = sum(stalled),
              demoted = sum(demoted),
              promoted = sum(promoted),
              retiring = sum(retire_must_FINAL)) %>%
    rename(prov = prov.2007)

# promotion history for 2011 leaders
leaders2011 <- leaders %>%
    filter(power.2011==1) %>%
    group_by(prov.2011, year) %>%
    summarise(num.leaders = n(),
              stalled = sum(stalled),
              demoted = sum(demoted),
              promoted = sum(promoted),
              retiring = sum(retire_must_FINAL)) %>%
    rename(prov = prov.2011)

# promotion history for 2006 leaders
leaders2006 <- leaders %>%
    filter(power.2006==1) %>%
    group_by(prov.2006, year) %>%
    summarise(num.leaders = n(),
              stalled = sum(stalled),
              demoted = sum(demoted),
              promoted = sum(promoted),
              retiring = sum(retire_must_FINAL)) %>%
    rename(prov = prov.2006)

# promotion history for 2010 leaders
leaders2010 <- leaders %>%
    filter(power.2010==1) %>%
    group_by(prov.2010, year) %>%
    summarise(num.leaders = n(),
              stalled = sum(stalled),
              demoted = sum(demoted),
              promoted = sum(promoted),
              retiring = sum(retire_must_FINAL)) %>%
    rename(prov = prov.2010)


