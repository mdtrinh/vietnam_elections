library(ri)
library(wfe)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All_bound65.R")

#######

## One-year change, simple RI

ritest(plan %>%
         filter(defeat.2007!=0 | closewin.2007!=0) %>%
         filter(defeat.2007*closewin.2007==0) %>%
         filter(prov!="Ha Noi" & prov!="TP HCM"),
       2008, "defeat.2007", "net.trans.change")

ritest(plan %>%
         filter(defeat.2011!=0 | closewin.2011!=0) %>%
         filter(defeat.2011*closewin.2011==0) %>%
         filter(prov!="Ha Noi" & prov!="TP HCM"),
       2012, "defeat.2011", "net.trans.change")

ritest(plan %>%
         filter(defeat.2016!=0 | closewin.2016!=0) %>%
         filter(prov!="Ha Noi" & prov!="TP HCM"),
       2017, "defeat.2016", "net.trans.change")

## One-year change, cross-sectional regression

# 2007
summary(lm(neglog(net.trans.change) ~ defeat.2007 +  neglog(total.rev.change) + neglog(total.rev.lag) + 
             factor(num.candidates.2007) + factor(num.elected.2007) + factor(centralnominated.2007),
           data = plan %>%
             filter(year==2008) %>%
             filter(defeat.2007!=0 | closewin.2007!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM"))) # just lm
rireg(data = plan %>%
        filter(year==2008) %>%
        filter(defeat.2007!=0 | closewin.2007!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM"),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.2007",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates.2007)", "factor(num.elected.2007)", "factor(centralnominated.2007)")) # lm under RI
# 2011
summary(lm(neglog(net.trans.change) ~ defeat.2011 + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates.2011) + factor(num.elected.2011) + factor(centralnominated.2011),
           data = plan %>%
             filter(year==2012) %>%
             filter(defeat.2011!=0 | closewin.2011!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM"))) # just lm
rireg(data = plan %>%
        filter(year==2012) %>%
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM"),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.2011",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates.2011)", "factor(num.elected.2011)", "factor(centralnominated.2011)")) # lm under RI

# 2016
summary(lm(neglog(net.trans.change) ~ defeat.2016 + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates.2016) + factor(num.elected.2016) + factor(centralnominated.2016),
           data = plan %>%
             filter(year==2017) %>%
             filter(defeat.2016!=0 | closewin.2016!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM"))) # just lm
rireg(data = plan %>%
        filter(year==2017) %>%
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM"),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat.2016",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates.2016)", "factor(num.elected.2016)", "factor(centralnominated.2016)")) # lm under RI

## One-year change, linear fixed effects panel regression

# all three elections in one regression
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
             filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2007
summary(lm(neglog(net.trans.change) ~ defeat + defeat.2007 + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
             mutate(defeat = defeat*as.numeric(year==2008)) %>%
             filter(year < 2009) %>% # avoid post-treatment
             filter(defeat.2007!=0 | closewin.2007!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        mutate(defeat = defeat*as.numeric(year==2008)) %>%
        filter(year < 2009) %>% # avoid post-treatment
        filter(defeat.2007!=0 | closewin.2007!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2011 election only
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
             mutate(defeat = defeat*as.numeric(year==2012)) %>%
             filter(year < 2013) %>% # avoid post-treatment
             filter(defeat.2011!=0 | closewin.2011!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        mutate(defeat = defeat*as.numeric(year==2012)) %>%
        filter(year < 2013) %>% # avoid post-treatment
        filter(defeat.2011!=0 | closewin.2011!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

# 2016 election only
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             num.candidates + num.elected + centralnominated + factor(prov) + factor(year) ,
           data = plan %>%
             mutate(defeat = defeat*as.numeric(year==2017)) %>%
             filter(defeat.2016!=0 | closewin.2016!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        mutate(defeat = defeat*as.numeric(year==2017)) %>%
        filter(defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "num.candidates", "num.elected", "centralnominated", "factor(prov)", "factor(year)"),
      block = "year") # lm under RI

## persistent change, linear fixed effects panel regression

# all three elections in one regression
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
             group_by(prov, period) %>%
             fill(num.candidates:num.closewin) %>%
             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
             ungroup %>%
             mutate(defeat.cum = ifelse(year < 2012, defeat, 
                                        ifelse(year < 2017, defeat*(defeat + defeat.2007), 
                                               defeat*(defeat+defeat.2011+defeat.2011*defeat.2007)))) %>% 
             filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2007 NOTE: adding/removing year fixed effects change results very drastically... adding = positive, removing = negative
summary(lm(neglog(net.trans.change) ~ defeat + defeat.2007 + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
             mutate(period.prov = as.factor(paste(prov, period))) %>%
             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
             group_by(prov, period) %>%
             fill(num.candidates:num.closewin) %>%
             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
             ungroup %>%
             mutate(defeat = defeat*as.numeric(period==2)) %>%
             filter(year < 2012) %>%
             filter(defeat.2007!=0 | closewin.2007!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
rireg(data = plan %>%
        mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
        mutate(period.prov = as.factor(paste(prov, period))) %>%
        mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
        group_by(prov, period) %>%
        fill(num.candidates:num.closewin) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
        ungroup %>%
        filter(year < 2012) %>% 
        filter(defeat.2007!=0 | closewin.2007!=0) %>%
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2011 NOTE: adding/removing year fixed effects change results very drastically... adding = negative, removing = positive
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
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
             mutate(defeat = defeat*as.numeric(period==3)) %>%
             filter(year < 2017) %>%
             filter(defeat.2011!=0 | closewin.2011!=0) %>%
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
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
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

# 2016 -- basically just like one year change since we have only one post-election year
summary(lm(neglog(net.trans.change) ~ defeat + neglog(total.rev) + neglog(total.rev.lag) +
             factor(num.candidates) + factor(num.elected) + factor(centralnominated) + factor(prov) + factor(year),
           data = plan %>%
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
             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
             filter(!is.na(total.rev) & !is.na(total.rev.lag)))) # just lm
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
        filter(prov!="Ha Noi" & prov!="TP HCM") %>%
        filter(!is.na(total.rev) & !is.na(total.rev.lag)),
      outcome = "neglog(net.trans.change)",
      treatment = "defeat",
      covs = c("neglog(total.rev)", "neglog(total.rev.lag)",
               "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
      clus = "period.prov") # lm under RI

## One-year change, weighted fixed effects regression

# all three elections in one regression -- cannot do TW fixed effects
summary(wfe(net.trans.change ~ defeat + total.rev + total.rev.lag + 
              num.candidates + num.elected + centralnominated,
            treat = "defeat",
            unit.index = "prov",
            time.index = "year",
            method = "unit",
            unbiased.se = TRUE,
            qoi="att",
            data = plan %>%
              filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
              filter(prov!="Ha Noi" & prov!="TP HCM") %>%
              filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
              mutate(net.trans.change = neglog(net.trans.change)) %>%
              mutate(total.rev = neglog(total.rev)) %>%
              mutate(total.rev.lag = neglog(total.rev.lag)))) # run wfe
keep.did <- riwfe(data = plan %>%
                    filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                    filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                    filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                    mutate(net.trans.change = neglog(net.trans.change)) %>%
                    mutate(total.rev = neglog(total.rev)) %>%
                    mutate(total.rev.lag = neglog(total.rev.lag)),
                  outcome = "net.trans.change",
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
# riwfe(data = plan %>%
#         filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
#         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
#         filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
#         mutate(net.trans.change = neglog(net.trans.change)) %>%
#         mutate(net.trans = neglog(net.trans)) %>%
#         mutate(total.rev = neglog(total.rev)) %>%
#         mutate(total.rev.lag = neglog(total.rev.lag)) %>%
#         mutate(num.candidates = as.factor(num.candidates)) %>%
#         mutate(num.elected = as.factor(num.elected)) %>%
#         mutate(centralnominated = as.factor(centralnominated)) %>%
#         mutate(prov = as.numeric(as.factor(prov))) %>%
#         model.matrix( ~ net.trans.change + defeat + total.rev + total.rev.lag + 
#                         num.candidates + num.elected + centralnominated + prov + year, .) %>%
#         data.frame %>%
#         mutate(prov = as.factor(prov)) %>%
#         mutate(year = as.factor(year)),
#       outcome = "net.trans.change",
#       treatment = "defeat",
#       covs = c("total.rev", "total.rev.lag",
#                "num.candidates10", "num.candidates11", "num.candidates13", "num.candidates14", 
#                "num.candidates15", "num.candidates17", "num.candidates18", "num.candidates19", 
#                "num.candidates21", "num.candidates24", "num.candidates26", "num.candidates29",
#                "num.elected5", "num.elected6", "num.elected7", "num.elected8", "num.elected9",
#                "num.elected10", "num.elected11", "num.elected14","num.elected16",
#                "centralnominated1", "centralnominated2", "centralnominated3",
#                "centralnominated4", "centralnominated5", "centralnominated6"),
#       block = "year",
#       unit.index = "prov",
#       time.index = "year",
#       method = "unit",
#       unbiased.se = TRUE) # wfe under RI

## Persistent change, weighted fixed effects regression

# all three elections in one regression
summary(wfe(net.trans.change ~ defeat + total.rev + total.rev.lag + 
              num.candidates + num.elected + centralnominated,
            treat = "defeat",
            unit.index = "prov",
            time.index = "year",
            method = "unit",
            unbiased.se = TRUE,
            qoi="att",
            data = plan %>%
              mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
              mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
              group_by(prov, period) %>%
              fill(num.candidates:num.closewin) %>%
              mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
              ungroup %>%
              filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
              filter(prov!="Ha Noi" & prov!="TP HCM") %>%
              filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
              mutate(net.trans.change = neglog(net.trans.change)) %>%
              mutate(net.trans = neglog(net.trans)) %>%
              mutate(total.rev = neglog(total.rev)) %>%
              mutate(total.rev.lag = neglog(total.rev.lag)))) # run wfe
keep <- riwfe(data = plan %>%
                mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                mutate(period.prov = as.factor(paste(prov, period))) %>%
                mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                group_by(prov, period) %>%
                fill(num.candidates:num.closewin) %>%
                mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                ungroup %>%
                filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                mutate(net.trans.change = neglog(net.trans.change)) %>%
                mutate(total.rev = neglog(total.rev)) %>%
                mutate(total.rev.lag = neglog(total.rev.lag)),
              outcome = "net.trans.change",
              treatment = "defeat",
              covs = c("total.rev", "total.rev.lag",
                       "num.candidates", "num.elected", "centralnominated"),
              clus = "period.prov",
              unit.index = "prov",
              time.index = "year",
              method = "unit",
              qoi = "att") # wfe under RI