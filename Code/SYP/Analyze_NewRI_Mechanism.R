library(ri)
library(wfe)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

## One-year change, linear fixed effects panel regression

# all three elections in one regression
rireg(data = prov %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(dev.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

rireg(data = prov %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(admin.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2007 election only - have to drop covs due to insufficient data >> result not usable
# rireg(data = prov %>%
#         mutate(defeat = defeat*as.numeric(year==2008)) %>%
#         filter(year < 2009) %>% # avoid post-treatment
#         filter(defeat.2007!=0 | closewin.2007!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)),
#       outcome = "neglog(dev.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)", "factor(prov)", "factor(year)"),
#       block = "year") # lm under RI


# 2011 election only
rireg(data = prov %>%
        mutate(defeat = defeat*as.numeric(year==2012)) %>%
        filter(year < 2013) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(dev.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

rireg(data = prov %>%
        mutate(defeat = defeat*as.numeric(year==2012)) %>%
        filter(year < 2013) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(admin.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 election only - data not yet available >> result not usable
# summary(lm(neglog(dev.exp.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
#              num.candidates + num.elected + centralnominated + factor(prov) ,
#            data = prov %>%
#              mutate(defeat = defeat*as.numeric(year==2017)) %>%
#              filter(defeat.2016!=0 | closewin.2016!=0) %>%
#              filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#              filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
# rireg(data = prov %>%
#         mutate(defeat = defeat*as.numeric(year==2017)) %>%
#         filter(defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)),
#       outcome = "neglog(dev.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "num.candidates", "num.elected", "centralnominated", "factor(prov)", "factor(year)"),
#       block = "year") # lm under RI

## persistent change, linear fixed effects panel regression

# all three elections in one regression -- actually only two (2007 and 2011 since post-election data not available for 2016)
rireg(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(dev.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

rireg(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        mutate(defeat.cum = ifelse(year < 2012, defeat, 
                                   ifelse(year < 2017, defeat*(defeat + defeat.2007), 
                                          defeat*(defeat+defeat.2011+defeat.2011*defeat.2007)))) %>% 
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(admin.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2007 -- too many pre-treatment obs missing >> results not usable
# rireg(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         filter(year < 2012) %>% 
#         filter(defeat.2007!=0 | closewin.2007!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)),
#       outcome = "neglog(dev.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
#       clus = "period.prov") # lm under RI
# 
# rireg(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         filter(year < 2012) %>% 
#         filter(defeat.2007!=0 | closewin.2007!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(admin.exp.change)),
#       outcome = "neglog(admin.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
#       clus = "period.prov") # lm under RI

# 2011
rireg(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        mutate(defeat = defeat*as.numeric(period==3)) %>%
        filter(year < 2015) %>%  ## HAD TO CUT IT OFF AT 2015 -- WHY?
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)),
      outcome = "neglog(dev.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

rireg(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        mutate(defeat = defeat*as.numeric(period==3)) %>%
        filter(year < 2015) %>% 
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(admin.exp.change)),
      outcome = "neglog(admin.exp.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2016 -- data not yet available >> result unusable
# rireg(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         mutate(defeat = defeat*as.numeric(period==4)) %>%
#         filter(year < 2022) %>% 
#         filter(defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)),
#       outcome = "neglog(dev.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
#       clus = "period.prov") # lm under RI
# 
# rireg(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         mutate(defeat = defeat*as.numeric(period==4)) %>%
#         filter(year < 2022) %>% 
#         filter(defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(admin.exp.change)),
#       outcome = "neglog(admin.exp.change)",
#       treatment = "defeat",
#       covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
#                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
#       clus = "period.prov") # lm under RI

## One-year change, weighted fixed effects regression

# all three elections in one regression -- actually only two (2007 and 2011 since post-election data not available for 2016)
riwfe(data = prov %>%
        filter(year<2013) %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(prov!="Hai Phong") %>% # too many missing obs
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)) %>%
        mutate(dev.exp.change = neglog(dev.exp.change)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "dev.exp.change",
      treatment = "defeat",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      block = "year",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI

riwfe(data = prov %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(admin.exp.change)) %>%
        mutate(admin.exp.change = neglog(admin.exp.change)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "admin.exp.change",
      treatment = "defeat",
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      block = "year",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI

# alternative version that uses categorical vars as dummies
# riwfe(data = prov %>%
#         filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         mutate(dev.exp.change = neglog(dev.exp.change)) %>%
#         
#         mutate(total.rev = neglog(total.rev)) %>%
#         mutate(total.rev.lag = neglog(total.rev.lag)) %>%
#         mutate(num.candidates = as.factor(num.candidates)) %>%
#         mutate(num.elected = as.factor(num.elected)) %>%
#         mutate(centralnominated = as.factor(centralnominated)) %>%
#         mutate(prov = as.numeric(as.factor(prov))) %>%
#         model.matrix( ~ dev.exp.change + defeat + total.rev + total.rev.lag + 
#                         num.candidates + num.elected + centralnominated + prov + year, .) %>%
#         data.frame %>%
#         mutate(prov = as.factor(prov)) %>%
#         mutate(year = as.factor(year)),
#       outcome = "dev.exp.change",
#       treatment = "defeat",
#       covs = c("total.rev", "total.rev.lag",
#                "num.candidates10", "num.candidates11", "num.candidates13", "num.candidates14", "num.candidates15",
#                "num.candidates17", "num.candidates18", "num.candidates19", "num.candidates26", "num.candidates29",
#                "num.elected5", "num.elected6", "num.elected7", "num.elected8",
#                "num.elected9", "num.elected10", "num.elected11", "num.elected16",
#                "centralnominated1", "centralnominated2", "centralnominated3",
#                "centralnominated4", "centralnominated5", "centralnominated6"),
#       block = "year",
#       unit.index = "prov",
#       time.index = "year",
#       method = "unit",
#       unbiased.se = TRUE,
#       qoi="att") # wfe under RI

# riwfe(data = prov %>%
#         filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         mutate(dev.exp.change = neglog(dev.exp.change)) %>%
#         mutate(total.rev = neglog(total.rev)) %>%
#         mutate(total.rev.lag = neglog(total.rev.lag)) %>%
#         mutate(num.candidates = as.factor(num.candidates)) %>%
#         mutate(num.elected = as.factor(num.elected)) %>%
#         mutate(centralnominated = as.factor(centralnominated)) %>%
#         mutate(prov = as.numeric(as.factor(prov))) %>%
#         model.matrix( ~ dev.exp.change + defeat + total.rev + total.rev.lag + 
#                         num.candidates + num.elected + centralnominated + prov + year, .) %>%
#         data.frame %>%
#         mutate(prov = as.factor(prov)) %>%
#         mutate(year = as.factor(year)),
#       outcome = "admin.exp.change",
#       treatment = "defeat",
#       covs = c("total.rev", "total.rev.lag",
#                "num.candidates10", "num.candidates11", "num.candidates13", "num.candidates14", "num.candidates15",
#                "num.candidates17", "num.candidates18", "num.candidates19", "num.candidates26", "num.candidates29",
#                "num.elected5", "num.elected6", "num.elected7", "num.elected8",
#                "num.elected9", "num.elected10", "num.elected11", "num.elected16",
#                "centralnominated1", "centralnominated2", "centralnominated3",
#                "centralnominated4", "centralnominated5", "centralnominated6"),
#       block = "year",
#       unit.index = "prov",
#       time.index = "year",
#       method = "unit",
#       unbiased.se = TRUE,
#       qoi="att") # wfe under RI


## Persistent change, weighted fixed effects regression

# all three elections in one regression
riwfe(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(prov!="Hai Phong") %>% # too many missing obs
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)) %>%
        mutate(dev.exp.change = neglog(dev.exp.change)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "dev.exp.change",
      treatment = "defeat", # should use defeat.cum to account for 2 consecutive defeats, but wfe does not support non-binary treatments
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      clus = "period.prov",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI

riwfe(data = prov %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
        filter(prov!="Hai Phong") %>% # too many missing obs
        filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
        filter(!is.na(dev.exp.change)) %>%
        mutate(admin.exp.change = neglog(admin.exp.change)) %>%
        mutate(total.rev = neglog(total.rev)) %>%
        mutate(total.rev.lag = neglog(total.rev.lag)),
      outcome = "admin.exp.change",
      treatment = "defeat", # should use defeat.cum to account for 2 consecutive defeats, but wfe does not support non-binary treatments
      covs = c("total.rev", "total.rev.lag",
               "num.candidates", "num.elected", "centralnominated"),
      clus = "period.prov",
      unit.index = "prov",
      time.index = "year",
      method = "unit",
      unbiased.se = TRUE,
      qoi="att") # wfe under RI

# alternative version that uses categorical vars as dummies
# riwfe(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)) %>%
#         mutate(dev.exp.change = neglog(dev.exp.change)) %>%
#         mutate(total.rev = neglog(total.rev)) %>%
#         mutate(total.rev.lag = neglog(total.rev.lag)) %>%
#         mutate(num.candidates = as.factor(num.candidates)) %>%
#         mutate(num.elected = as.factor(num.elected)) %>%
#         mutate(centralnominated = as.factor(centralnominated)) %>%
#         mutate(prov = as.numeric(as.factor(prov))) %>%
#         mutate(period.prov = as.numeric(as.factor(period.prov))) %>%
#         model.matrix( ~ dev.exp.change + defeat + total.rev + total.rev.lag +
#                         num.candidates + num.elected + centralnominated + prov + year + period.prov, .) %>%
#         data.frame %>%
#         mutate(prov = as.factor(prov)) %>%
#         mutate(period.prov = as.factor(period.prov)) %>%
#         mutate(year = as.factor(year)),
#       outcome = "dev.exp.change",
#       treatment = "defeat",
#       covs = c("total.rev", "total.rev.lag",
#                "num.candidates10", "num.candidates11", "num.candidates13", "num.candidates14", "num.candidates15",
#                "num.candidates17", "num.candidates18", "num.candidates19", "num.candidates26", "num.candidates29",
#                "num.elected5", "num.elected6", "num.elected7", "num.elected8",
#                "num.elected9", "num.elected10", "num.elected11", "num.elected16",
#                "centralnominated1", "centralnominated2", "centralnominated3",
#                "centralnominated4", "centralnominated5", "centralnominated6"),
#       clus = "period.prov",
#       unit.index = "prov",
#       time.index = "year",
#       method = "unit",
#       unbiased.se = TRUE) # wfe under RI

# riwfe(data = prov %>%
#         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
#         mutate(period.prov = as.factor(paste(prov, period))) %>%
#         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
#         group_by(prov, period) %>%
#         fill(num.candidates:num.closewin) %>%
#         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
#         ungroup %>%
#         filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         filter(!is.na(dev.exp.change)) %>%
#         mutate(dev.exp.change = neglog(dev.exp.change)) %>%
#         mutate(total.rev = neglog(total.rev)) %>%
#         mutate(total.rev.lag = neglog(total.rev.lag)) %>%
#         mutate(num.candidates = as.factor(num.candidates)) %>%
#         mutate(num.elected = as.factor(num.elected)) %>%
#         mutate(centralnominated = as.factor(centralnominated)) %>%
#         mutate(prov = as.numeric(as.factor(prov))) %>%
#         mutate(period.prov = as.numeric(as.factor(period.prov))) %>%
#         model.matrix( ~ dev.exp.change + defeat + total.rev + total.rev.lag +
#                         num.candidates + num.elected + centralnominated + prov + year + period.prov, .) %>%
#         data.frame %>%
#         mutate(prov = as.factor(prov)) %>%
#         mutate(period.prov = as.factor(period.prov)) %>%
#         mutate(year = as.factor(year)),
#       outcome = "admin.exp.change",
#       treatment = "defeat",
#       covs = c("total.rev", "total.rev.lag",
#                "num.candidates10", "num.candidates11", "num.candidates13", "num.candidates14", "num.candidates15",
#                "num.candidates17", "num.candidates18", "num.candidates19", "num.candidates26", "num.candidates29",
#                "num.elected5", "num.elected6", "num.elected7", "num.elected8",
#                "num.elected9", "num.elected10", "num.elected11", "num.elected16",
#                "centralnominated1", "centralnominated2", "centralnominated3",
#                "centralnominated4", "centralnominated5", "centralnominated6"),
#       clus = "period.prov",
#       unit.index = "prov",
#       time.index = "year",
#       method = "unit",
#       unbiased.se = TRUE) # wfe under RI