library(ri)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

####


### Total number of promotions

# 2007
summary(lm(promoted ~ defeat,
           data = leaders2007 %>%
             filter(defeat!=0 | closewin!=0) %>%
             filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
             group_by(prov) %>%
             summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
                       num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
                       promoted = sum(promoted*as.numeric(year>2007 & year <2012)),
                       num.leaders = max(num.leaders*as.numeric(year>2007 & year <2012)),
                       retiring = sum(retiring*as.numeric(year>2007 & year <2012)),
                       defeat=max(defeat),
                       num.candidates=max(num.candidates),
                       num.elected=max(num.elected),
                       centralnominated=max(centralnominated))))

rireg(data = leaders2007 %>%
        filter(defeat!=0 | closewin!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        group_by(prov) %>%
        summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
                  num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
                  promoted = sum(promoted*as.numeric(year>2007 & year <2012)),
                  num.leaders = max(num.leaders*as.numeric(year>2007 & year <2012)),
                  retiring = sum(retiring*as.numeric(year>2007 & year <2012)),
                  defeat=max(defeat),
                  num.candidates=max(num.candidates),
                  num.elected=max(num.elected),
                  centralnominated=max(centralnominated)),
      outcome = "promoted",
      treatment = "defeat", 
      covs = NULL) # lm under RI

# 2011
summary(lm(promoted ~ defeat,
           data = leaders2011 %>%
             filter(defeat!=0 | closewin!=0) %>%
             filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
             group_by(prov) %>%
             summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
                       num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
                       promoted = sum(promoted*as.numeric(year>2011 & year <2017)),
                       num.leaders = max(num.leaders*as.numeric(year>2007 & year <2017)),
                       defeat=max(defeat),
                       num.candidates=max(num.candidates),
                       num.elected=max(num.elected),
                       centralnominated=max(centralnominated))))

rireg(data = leaders2011 %>%
        filter(defeat!=0 | closewin!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        group_by(prov) %>%
        summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
                  num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
                  promoted = sum(promoted*as.numeric(year>2011 & year <2017)),
                  num.leaders = max(num.leaders*as.numeric(year>2007 & year <2017)),
                  defeat=max(defeat),
                  num.candidates=max(num.candidates),
                  num.elected=max(num.elected),
                  centralnominated=max(centralnominated)),
      outcome = "promoted",
      treatment = "defeat",
      covs = NULL) # lm under RI

### ANY promotion

# 2007
summary(lm(promoted ~ defeat,
           data = leaders2007 %>%
             filter(defeat!=0 | closewin!=0) %>%
             filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
             group_by(prov) %>%
             summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
                       num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
                       promoted = max(promoted*as.numeric(year>2007 & year <2012)),
                       num.leaders = max(num.leaders*as.numeric(year>2007 & year <2012)),
                       retiring = sum(retiring*as.numeric(year>2007 & year <2012)),
                       defeat=max(defeat),
                       num.candidates=max(num.candidates),
                       num.elected=max(num.elected),
                       centralnominated=max(centralnominated))))

rireg(data = leaders2007 %>%
        filter(defeat!=0 | closewin!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        group_by(prov) %>%
        summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
                  num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
                  promoted = max(promoted*as.numeric(year>2007 & year <2012)),
                  num.leaders = max(num.leaders*as.numeric(year>2007 & year <2012)),
                  retiring = sum(retiring*as.numeric(year>2007 & year <2012)),
                  defeat=max(defeat),
                  num.candidates=max(num.candidates),
                  num.elected=max(num.elected),
                  centralnominated=max(centralnominated)),
      outcome = "promoted",
      treatment = "defeat", 
      covs = NULL) # lm under RI

# 2011
summary(lm(promoted ~ defeat,
           data = leaders2011 %>%
             filter(defeat!=0 | closewin!=0) %>%
             filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
             group_by(prov) %>%
             summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
                       num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
                       promoted = max(promoted*as.numeric(year>2011 & year <2017)),
                       num.leaders = max(num.leaders*as.numeric(year>2007 & year <2017)),
                       defeat=max(defeat),
                       num.candidates=max(num.candidates),
                       num.elected=max(num.elected),
                       centralnominated=max(centralnominated))))

rireg(data = leaders2011 %>%
        filter(defeat!=0 | closewin!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        group_by(prov) %>%
        summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
                  num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
                  promoted = sum(promoted*as.numeric(year>2011 & year <2017)),
                  num.leaders = max(num.leaders*as.numeric(year>2007 & year <2017)),
                  defeat=max(defeat),
                  num.candidates=max(num.candidates),
                  num.elected=max(num.elected),
                  centralnominated=max(centralnominated)),
      outcome = "promoted",
      treatment = "defeat",
      covs = NULL) # lm under RI