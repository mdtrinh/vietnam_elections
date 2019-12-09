library(ri)
library(wfe)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

## One-year change, linear fixed effects panel regression

# all three elections in one regression -- actually only 2
rireg(data = plan %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
        filter(year < 2013) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2011 treatment, 2007 outcomes
rireg(data = plan %>%
        mutate(defeat.lead = defeat.lead*as.numeric(year==2008)) %>%
        filter(year < 2009) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(defeat.2007 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 treatment, 2011 outcomes
rireg(data = plan %>%
        mutate(defeat.lead = defeat.lead*as.numeric(year==2012)) %>%
        filter(year < 2013) %>% # avoid post-treatment
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2011 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 treatment, 2007 outcomes -- have to drop covs b/c of insufficient data
rireg(data = plan %>%
        mutate(defeat.lead2 = defeat.lead2*as.numeric(year==2008)) %>%
        filter(year < 2009) %>% # avoid post-treatment
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead2",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)","factor(prov)", "factor(year)"),
      block = "year") # lm under RI

## persistent change, linear fixed effects panel regression

# all three elections in one regression -- actually only 2
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
        group_by(prov, period) %>%
        fill(num.candidates:defeat.lead2) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
        ungroup %>%
        filter(year < 2017) %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2011 treatment, 2007 outcomes
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
        group_by(prov, period) %>%
        fill(num.candidates:defeat.lead2) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
        ungroup %>%
        filter(year < 2012) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(defeat.2007 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 treatment, 2011 outcomes
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
        group_by(prov, period) %>%
        fill(num.candidates:defeat.lead2) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
        ungroup %>%
        filter(year < 2017) %>% # avoid post-treatment
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2011 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 treatment, 2007 outcomes -- have to drop covs b/c of insufficient data
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
        group_by(prov, period) %>%
        fill(num.candidates:defeat.lead2) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
        ungroup %>%
        filter(year < 2009) %>% # avoid post-treatment
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.lead2",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

## One-year change, weighted fixed effects regression

# all three elections in one regression -- actually only 2
riwfe(data = plan %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
        filter(year < 2013) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        mutate(net.trans.change = neglog(net.trans.change)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "net.trans.change",
      treatment = "defeat.lead",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      block = "year",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI

## Persistent change, weighted fixed effects regression

# all three elections in one regression -- actually only 2
riwfe(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
        group_by(prov, period) %>%
        fill(num.candidates:defeat.lead2) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
        ungroup %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
        filter(year < 2017) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        mutate(net.trans.change = neglog(net.trans.change)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "net.trans.change",
      treatment = "defeat.lead",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      clus = "period.prov",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      qoi = "att") # wfe under RI