library(ri)
library(wfe)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

#######

## One-year change, linear fixed effects panel regression

# all three elections in one regression -- actually only 2
rireg(data = plan %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2007 -- results not usable since there's no pre-treatment period
# rireg(data = plan %>%
#         mutate(defeat = defeat*as.numeric(year==2008)) %>%
#         filter(year < 2009) %>% # avoid post-treatment
#         filter(defeat.2007!=0 | closewin.2007!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
#       outcome = "neglog(net.trans.change.lag2)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
#       block = "year") # lm under RI

# 2011
rireg(data = plan %>%
        mutate(defeat = defeat*as.numeric(year==2012)) %>%
        filter(year < 2013) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016
rireg(data = plan %>%
        mutate(defeat = defeat*as.numeric(year==2017)) %>%
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "num.candidates", "num.elected", "centralnominated", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

## persistent change, linear fixed effects panel regression

# all three elections in one regression -- actually only 2...
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI


# 2011
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        mutate(defeat = defeat*as.numeric(period==3)) %>%
        mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
        mutate(num.elected = num.elected*as.numeric(period==3)) %>%
        mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
        filter(year < 2017) %>% 
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2016
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        mutate(defeat = defeat*as.numeric(period==4)) %>%
        mutate(num.candidates = num.candidates*as.numeric(period==4)) %>%
        mutate(num.elected = num.elected*as.numeric(period==4)) %>%
        mutate(centralnominated = centralnominated*as.numeric(period==4)) %>%
        filter(year < 2022) %>% 
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)),
      outcome = "neglog(net.trans.change.lag2)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

## One-year change, weighted fixed effects regression

# all three elections in one regression -- cannot do TW fixed effects
riwfe(data = plan %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)) %>%
        mutate(net.trans.change.lag2 = neglog(net.trans.change.lag2)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)) ,
      outcome = "net.trans.change.lag2",
      treatment = "defeat",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      block = "year",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI
## Persistent change, weighted fixed effects regression

# all three elections in one regression
riwfe(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(net.trans.change.lag2)) %>%
        mutate(net.trans.change.lag2 = neglog(net.trans.change.lag2)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "net.trans.change.lag2",
      treatment = "defeat",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      clus = "period.prov",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      qoi = "att") # wfe under RI