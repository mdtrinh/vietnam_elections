library(ri)
library(wfe)
library(grid)
library(gridExtra)
library(ggplot2)
library(vietnamdata)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/SYP/Merge_All.R")

#######

### helper function

## draw a simple y axe
y.axe <- function(min, max, flip=TRUE) {
    # flip = TRUE for sideway graphs
    if(flip){
        coord <- coord_flip(xlim = c(xmin = min,xmax = max))
    } else {
        coord <- coord_cartesian(ylim = c(ymin = min,ymax = max))
    }

    ggplot(data.frame(x=1,y=1), aes(x,y)) +
        geom_blank() +
        ggtitle(" ") +
        theme_bw() +
        theme(axis.line.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              axis.line.y=element_line(colour = "black"),
              axis.title.y=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        coord
}

#######

#### Fixed Effects Regression Results ####

## Panel one year
panel1.2007 <- rireg(data = plan %>%
                         filter(year < 2009) %>% # avoid post-treatment
                         filter(defeat.2007!=0 | closewin.2007!=0) %>%
                         mutate(defeat = defeat*as.numeric(year==2008)) %>%
                         # mutate(num.candidates = num.candidates*as.numeric(year==2008)) %>%
                         # mutate(num.elected = num.elected*as.numeric(year==2008)) %>%
                         # mutate(centralnominated = centralnominated*as.numeric(year==2008)) %>%
                         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                     outcome = "neglog(net.trans.change)",
                     treatment = "defeat",
                     covs = c("neglog(total.rev.lag)", "defeat.2007",
                              "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     blockvar = "year") # lm under RI

# 2011 election only
panel1.2011 <- rireg(data = plan %>%
                         filter(year < 2013) %>% # avoid post-treatment
                         filter(defeat.2011!=0 | closewin.2011!=0) %>%
                         mutate(defeat = defeat*as.numeric(year==2012)) %>%
                         # mutate(num.candidates = num.candidates*as.numeric(year==2012)) %>%
                         # mutate(num.elected = num.elected*as.numeric(year==2012)) %>%
                         # mutate(centralnominated = centralnominated*as.numeric(year==2012)) %>%
                         mutate(defeat.past = defeat.2007*as.numeric(year==2012)) %>%
                         mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                     outcome = "neglog(net.trans.change)",
                     treatment = "defeat",
                     covs = c("neglog(total.rev.lag)",
                              "defeat.past", "defeat.2007", "defeat.2011",
                              "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     blockvar = "year") # lm under RI

# 2016 election only
panel1.2016 <- rireg(data = plan %>%
                         filter(defeat.2016!=0 | closewin.2016!=0) %>%
                         mutate(defeat = defeat*as.numeric(year==2017)) %>%
                         # mutate(num.candidates = num.candidates*as.numeric(year==2017)) %>%
                         # mutate(num.elected = num.elected*as.numeric(year==2017)) %>%
                         # mutate(centralnominated = centralnominated*as.numeric(year==2017)) %>%
                         mutate(defeat.past = (defeat.2007+defeat.2011)*as.numeric(year==2017)) %>%
                         mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                         mutate(defeat.2011 = defeat.2011*as.numeric(year==2012)) %>%
                         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                     outcome = "neglog(net.trans.change)",
                     treatment = "defeat",
                     covs = c( "neglog(total.rev.lag)",
                               "defeat.past", "defeat.2007", "defeat.2011", "defeat.2016",
                               "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     blockvar = "year") # lm under RI
rireg.plot(panel1.2011, xmin=-10,xmax=20)

## Panel persistent

# 2007
panell.2007 <- rireg(data = plan %>%
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
                     covs = c( "neglog(total.rev.lag)",
                               "defeat.2007",
                               "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     clus = "period.prov",
                     block = "period") # lm under RI
rireg.plot(panell.2007, xmin=-10,xmax=20)

# 2011
panell.2011 <- rireg(data = plan %>%
                         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                         mutate(period.prov = as.factor(paste(prov, period))) %>%
                         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                         group_by(prov, period) %>%
                         fill(num.candidates:num.closewin) %>%
                         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                         ungroup %>%
                         filter(year < 2017) %>%
                         filter(defeat.2011!=0 | closewin.2011!=0) %>%
                         mutate(defeat = defeat*as.numeric(period==3)) %>%
                         # mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
                         # mutate(num.elected = num.elected*as.numeric(period==3)) %>%
                         # mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
                         mutate(defeat.past = defeat.2007*as.numeric(period==3)) %>%
                         mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                     outcome = "neglog(net.trans.change)",
                     treatment = "defeat",
                     covs = c( "neglog(total.rev.lag)",
                               "defeat.past", "defeat.2007", "defeat.2011",
                               "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     clus = "period.prov",
                     block = "period") # lm under RI
rireg.plot(panell.2011, xmin=-10,xmax=20)

# 2016
panell.2016 <- rireg(data = plan %>%
                         mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                         mutate(period.prov = as.factor(paste(prov, period))) %>%
                         mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                         group_by(prov, period) %>%
                         fill(num.candidates:num.closewin) %>%
                         mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                         ungroup %>%
                         filter(year < 2022) %>%
                         filter(defeat.2016!=0 | closewin.2016!=0) %>%
                         mutate(defeat = defeat*as.numeric(period==4)) %>%
                         # mutate(num.candidates = num.candidates*as.numeric(period==4)) %>%
                         # mutate(num.elected = num.elected*as.numeric(period==4)) %>%
                         # mutate(centralnominated = centralnominated*as.numeric(period==4)) %>%
                         mutate(defeat.past = (defeat.2007+defeat.2011)*as.numeric(period==4)) %>%
                         mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                         mutate(defeat.2011 = defeat.2011*as.numeric(period==3)) %>%
                         filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                         filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                     outcome = "neglog(net.trans.change)",
                     treatment = "defeat",
                     covs = c( "neglog(total.rev.lag)",
                               "defeat.past", "defeat.2007", "defeat.2011", "defeat.2016",
                               "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                     clus = "period.prov",
                     block = "period") # lm under RI
rireg.plot(panell.2016, xmin=-10,xmax=20)

## Pooled one year
pool.one <- rireg(data = plan %>%
                      filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                      filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                      filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                  outcome = "neglog(net.trans.change)",
                  treatment = "defeat",
                  covs = c( "neglog(total.rev.lag)",
                            "defeat.2007", "defeat.2011", "defeat.2016",
                            "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                  blockvar = "year") # lm under RI

## Pooled persistent
pool.long <- rireg(data = plan %>%
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
                   covs = c( "neglog(total.rev.lag)",
                             "defeat.2007", "defeat.2011", "defeat.2016",
                             "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                   clus = "period.prov",
                   block = "period") # lm under RI

## WLE one year
weighted.one <- riwfe(data = plan %>%
                          filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                          filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                          mutate(net.trans.change = neglog(net.trans.change)) %>%
                          mutate(net.trans = neglog(net.trans)) %>%
                          mutate(total.rev = neglog(total.rev)) %>%
                          mutate(total.rev.lag = neglog(total.rev.lag)),
                      outcome = "net.trans.change",
                      treatment = "defeat",
                      covs = c("total.rev.lag",
                               "defeat.2007", "defeat.2011", "defeat.2016",
                               "power.central", "num.candidates", "num.elected", "centralnominated"),
                      blockvar = "year",
                      unit.index = "prov",
                      time.index = "year",
                      method = "unit",
                      qoi="att") # wfe under RI

## WLE persistent
weighted.long <- riwfe(data = plan %>%
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
                           mutate(net.trans = neglog(net.trans)) %>%
                           mutate(total.rev = neglog(total.rev)) %>%
                           mutate(total.rev.lag = neglog(total.rev.lag)),
                       outcome = "net.trans.change",
                       treatment = "defeat",
                       covs = c("total.rev.lag",
                                "defeat.2007", "defeat.2011", "defeat.2016",
                                "power.central", "num.candidates", "num.elected", "centralnominated"),
                       clus = "period.prov",
                       block = "period",
                       unit.index = "prov",
                       time.index = "year",
                       method = "unit",
                       qoi="att") # wfe under RI

### tabulate main results ###
lay <- rbind(c(NA,1,1,1,2,2,2,3,3,4,4, NA),
             seq(5,16))
syp.fe <- grid.arrange(layout_matrix = lay,
                       heights=c(1,10),
                       widths=c(1,rep(3,10), 1),
                       textGrob("Separate Panels, One-year"),
                       textGrob("Separate Panels, Persistent"),
                       textGrob("Pooled Panel, FE"),
                       textGrob("Pooled Panel, WFE"),
                       y.axe(-10,20),
                       rireg.plot(panel1.2007, scale=F, xmin=-10, xmax=20, title = "2007"),
                       rireg.plot(panel1.2011, scale=F, xmin=-10, xmax=20, title = "2011"),
                       rireg.plot(panel1.2016, scale=F, xmin=-10, xmax=20, title = "2016"),
                       rireg.plot(panell.2007, scale=F, xmin=-10, xmax=20, title = "2007"),
                       rireg.plot(panell.2011, scale=F, xmin=-10, xmax=20, title = "2011"),
                       rireg.plot(panell.2016, scale=F, xmin=-10, xmax=20, title = "2016"),
                       rireg.plot(pool.one, scale=F, xmin=-10, xmax=20, title = "One-year"),
                       rireg.plot(pool.long, scale=F, xmin=-10, xmax=20, title = "Persistent"),
                       rireg.plot(weighted.one, scale=F, xmin=-10, xmax=20, title = "One-year"),
                       rireg.plot(weighted.long, scale=F, xmin=-10, xmax=20, title = "Persistent"),
                       y.axe(-10,20) + scale_x_continuous(position = "top"))
ggsave("SYP_FE.png", plot=syp.fe, path="../../figure", width = 10, height = 3.5, units="in")

#### Placebo -- evidence of dynamic causality ####

## Panel one year

# 2011 treatment, 2007 outcomes
panel1.2007.place <- rireg(data = plan %>%
                               filter(year < 2009) %>% # avoid post-treatment
                               filter(defeat.2011!=0 | closewin.2011!=0) %>%
                               mutate(defeat.lead = defeat.lead*as.numeric(year==2008)) %>%
                               mutate(defeat = defeat*as.numeric(year==2008)) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(year==2008)) %>%
                               # mutate(num.elected = num.elected*as.numeric(year==2008)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(year==2008)) %>%
                               mutate(defeat.past = defeat.2007*as.numeric(year==2008)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                           outcome = "neglog(net.trans.change)",
                           treatment = "defeat.lead",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011",
                                     "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           blockvar = "year") # lm under RI

# 2016 treatment, 2011 outcomes
panel1.2011.place <- rireg(data = plan %>%
                               filter(year < 2013) %>% # avoid post-treatment
                               filter(defeat.2016!=0 | closewin.2016!=0) %>%
                               #filter(defeat.2011 != 1) %>%
                               mutate(defeat.lead = defeat.lead*as.numeric(year==2012)) %>%
                               mutate(defeat = defeat*as.numeric(year==2012)) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(year==2012)) %>%
                               # mutate(num.elected = num.elected*as.numeric(year==2012)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(year==2012)) %>%
                               mutate(defeat.past = (defeat.2007+defeat.2011)*as.numeric(year==2012)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                               mutate(defeat.2011 = defeat.2011*as.numeric(year==2012)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                           outcome = "neglog(net.trans.change)",
                           treatment = "defeat.lead",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011" , "defeat.2016",
                                     "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           blockvar = "year") # lm under RI

## Panel persistent

# 2011 treatment, 2007 outcomes
panell.2007.place <- rireg(data = plan %>%
                               mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                               mutate(period.prov = as.factor(paste(prov, period))) %>%
                               mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
                               group_by(prov, period) %>%
                               fill(num.candidates:defeat.lead2) %>%
                               mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
                               ungroup %>%
                               filter(year < 2012) %>% # avoid post-treatment
                               filter(defeat.2011!=0 | closewin.2011!=0) %>%
                               #filter(defeat.2007 != 1) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(period==2)) %>%
                               # mutate(num.elected = num.elected*as.numeric(period==2)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(period==2)) %>%
                               mutate(defeat.past = defeat.2007*as.numeric(period==2)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                           outcome = "neglog(net.trans.change)",
                           treatment = "defeat.lead",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011",
                                     "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           blockvar = "year") # lm under RI

# 2016 treatment, 2011 outcomes
panell.2011.place <- rireg(data = plan %>%
                               mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                               mutate(period.prov = as.factor(paste(prov, period))) %>%
                               mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
                               group_by(prov, period) %>%
                               fill(num.candidates:defeat.lead2) %>%
                               mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
                               ungroup %>%
                               filter(year < 2017) %>% # avoid post-treatment
                               filter(defeat.2016!=0 | closewin.2016!=0) %>%
                               #filter(defeat.2011 != 1) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
                               # mutate(num.elected = num.elected*as.numeric(period==3)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
                               mutate(defeat.past = (defeat.2011+defeat.2007)*as.numeric(period==3)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                               mutate(defeat.2011 = defeat.2011*as.numeric(period==3)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                           outcome = "neglog(net.trans.change)",
                           treatment = "defeat.lead",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011", "defeat.2016",
                                     "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           blockvar = "year") # lm under RI

## Pooled one year
pool.one.place <- rireg(data = plan %>%
                            filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                            #filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
                            filter(year < 2013) %>%
                            filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                            filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                        outcome = "neglog(net.trans.change)",
                        treatment = "defeat.lead",
                        covs = c( "neglog(total.rev.lag)",
                                  "defeat.2007", "defeat.2011", "defeat.2016",
                                  "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                        blockvar = "year") # lm under RI

## Pooled persistent
pool.long.place <- rireg(data = plan %>%
                             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                             mutate(period.prov = as.factor(paste(prov, period))) %>%
                             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
                             group_by(prov, period) %>%
                             fill(num.candidates:defeat.lead2) %>%
                             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
                             ungroup %>%
                             filter(year < 2017) %>%
                             filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                             #filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)),
                         outcome = "neglog(net.trans.change)",
                         treatment = "defeat.lead",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.2007", "defeat.2011", "defeat.2016",
                                   "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         clus = "period.prov",
                         block = "period") # lm under RI

## WLE one year
weighted.one.place <- riwfe(data = plan %>%
                                filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                                #filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
                                filter(year < 2013) %>%
                                filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                                mutate(net.trans.change = neglog(net.trans.change)) %>%
                                mutate(total.rev.lag = neglog(total.rev.lag)),
                            outcome = "net.trans.change",
                            treatment = "defeat.lead",
                            covs = c("total.rev.lag",
                                     "defeat.2007", "defeat.2011", "defeat.2016",
                                     "power.central", "num.candidates", "num.elected", "centralnominated"),
                            blockvar = "year",
                            unit.index = "prov",
                            time.index = "year",
                            method = "unit",
                            unbiased.se = TRUE,
                            qoi="att") # wfe under RI

## WLE persistent
weighted.long.place <- riwfe(data = plan %>%
                                 mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                                 mutate(period.prov = as.factor(paste(prov, period))) %>%
                                 mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:defeat.lead2) %>%
                                 group_by(prov, period) %>%
                                 fill(num.candidates:defeat.lead2) %>%
                                 mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:defeat.lead2) %>%
                                 ungroup %>%
                                 filter(year < 2017) %>%
                                 filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                                 #filter(defeat.2007 != 1 & !(defeat.2011 == 1 & (defeat.2016!=0 | closewin.2016!=0))) %>%
                                 filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                 filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                                 mutate(net.trans.change = neglog(net.trans.change)) %>%
                                 mutate(total.rev.lag = neglog(total.rev.lag)),
                             outcome = "net.trans.change",
                             treatment = "defeat.lead",
                             covs = c("total.rev.lag",
                                      "defeat.2007", "defeat.2011", "defeat.2016",
                                      "power.central", "num.candidates", "num.elected", "centralnominated"),
                             clus = "period.prov",
                             block = "period",
                             unit.index = "prov",
                             time.index = "year",
                             method = "unit",
                             qoi = "att") # wfe under RI

### tabulate placebo results ###
lay <- rbind(c(NA,1,1,2,2,3,3,4,4, NA),
             seq(5,14))
syp.fe.place <- grid.arrange(layout_matrix = lay,
                             heights=c(1,10),
                             widths=c(1,rep(3,8), 1),
                             textGrob("Separate Panels, One-year"),
                             textGrob("Separate Panels, Persistent"),
                             textGrob("Pooled Panel, FE"),
                             textGrob("Pooled Panel, WFE"),
                             y.axe(-20,25),
                             rireg.plot(panel1.2007.place, scale=F, xmin=-20, xmax=25, title = "2007"),
                             rireg.plot(panel1.2011.place, scale=F, xmin=-20, xmax=25, title = "2011"),
                             rireg.plot(panell.2007.place, scale=F, xmin=-20, xmax=25, title = "2007"),
                             rireg.plot(panell.2011.place, scale=F, xmin=-20, xmax=25, title = "2011"),
                             rireg.plot(pool.one.place, scale=F, xmin=-20, xmax=25, title = "One-year"),
                             rireg.plot(pool.long.place, scale=F, xmin=-20, xmax=25, title = "Persistent"),
                             rireg.plot(weighted.one.place, scale=F, xmin=-20, xmax=25, title = "One-year"),
                             rireg.plot(weighted.long.place, scale=F, xmin=-20, xmax=25, title = "Persistent"),
                             y.axe(-20,25) + scale_x_continuous(position = "top"))
ggsave("SYP_FE_PLACE.png", plot=syp.fe.place, path="../../figure", width = 10, height = 3.5, units="in")

#### More placebo -- lagged outcomes ####
## Panel one year

# 2011
panel1.2011.lag <- rireg(data = plan %>%
                             filter(year < 2013) %>% # avoid post-treatment
                             filter(defeat.2011!=0 | closewin.2011!=0) %>%
                             mutate(defeat = defeat*as.numeric(year==2012)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(year==2012)) %>%
                             # mutate(num.elected = num.elected*as.numeric(year==2012)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(year==2012)) %>%
                             mutate(defeat.past = defeat.2007*as.numeric(year==2012)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                             filter(!is.na(net.trans.change.lag2)),
                         outcome = "neglog(net.trans.change.lag2)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011",
                                   "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         blockvar = "year") # lm under RI

# 2016
panel1.2016.lag <- rireg(data = plan %>%
                             filter(defeat.2016!=0 | closewin.2016!=0) %>%
                             mutate(defeat = defeat*as.numeric(year==2017)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(year==2017)) %>%
                             # mutate(num.elected = num.elected*as.numeric(year==2017)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(year==2017)) %>%
                             mutate(defeat.past = (defeat.2007+defeat.2011)*as.numeric(year==2017)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                             mutate(defeat.2011 = defeat.2011*as.numeric(year==2012)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                             filter(!is.na(net.trans.change.lag2)),
                         outcome = "neglog(net.trans.change.lag2)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011", "defeat.2016",
                                   "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         blockvar = "year") # lm under RI


## Panel persistent

# 2011
panell.2011.lag <- rireg(data = plan %>%
                             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                             mutate(period.prov = as.factor(paste(prov, period))) %>%
                             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                             group_by(prov, period) %>%
                             fill(num.candidates:num.closewin) %>%
                             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                             ungroup %>%
                             filter(year < 2017) %>%
                             filter(defeat.2011!=0 | closewin.2011!=0) %>%
                             mutate(defeat = defeat*as.numeric(period==3)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
                             # mutate(num.elected = num.elected*as.numeric(period==3)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
                             mutate(defeat.past = defeat.2007*as.numeric(period==3)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag))%>%
                             filter(!is.na(net.trans.change.lag2)),
                         outcome = "neglog(net.trans.change.lag2)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011",
                                   "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         clus = "period.prov",
                         block = "period") # lm under RI

# 2016
panell.2016.lag <- rireg(data = plan %>%
                             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                             mutate(period.prov = as.factor(paste(prov, period))) %>%
                             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                             group_by(prov, period) %>%
                             fill(num.candidates:num.closewin) %>%
                             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                             ungroup %>%
                             filter(defeat.2016!=0 | closewin.2016!=0) %>%
                             mutate(defeat = defeat*as.numeric(period==4)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(period==4)) %>%
                             # mutate(num.elected = num.elected*as.numeric(period==4)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(period==4)) %>%
                             mutate(defeat.past = (defeat.2007+defeat.2011)*as.numeric(period==4)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                             mutate(defeat.2011 = defeat.2011*as.numeric(period==3)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                             filter(!is.na(net.trans.change.lag2)),
                         outcome = "neglog(net.trans.change.lag2)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011", "defeat.2016",
                                   "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         clus = "period.prov",
                         block = "period") # lm under RI

## Pooled one year
pool.one.lag <- rireg(data = plan %>%
                          filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                          filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                          filter(!is.na(net.trans.change.lag2)),
                      outcome = "neglog(net.trans.change.lag2)",
                      treatment = "defeat",
                      covs = c( "neglog(total.rev.lag)",
                                "defeat.2007", "defeat.2011", "defeat.2016",
                                "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                      blockvar = "year") # lm under RI

## Pooled persistent
pool.long.lag <- rireg(data = plan %>%
                           mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                           mutate(period.prov = as.factor(paste(prov, period))) %>%
                           mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                           group_by(prov, period) %>%
                           fill(num.candidates:num.closewin) %>%
                           mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                           ungroup %>%
                           filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                           filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                           filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                           filter(!is.na(net.trans.change.lag2)),
                       outcome = "neglog(net.trans.change.lag2)",
                       treatment = "defeat",
                       covs = c( "neglog(total.rev.lag)",
                                 "defeat.2007", "defeat.2011", "defeat.2016",
                                 "power.central", "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                       clus = "period.prov",
                       block = "period") # lm under RI

## WLE one year
weighted.one.lag <- riwfe(data = plan %>%
                              filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                              filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                              filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                              filter(!is.na(net.trans.change.lag2)) %>%
                              mutate(net.trans.change.lag2 = neglog(net.trans.change.lag2)) %>%
                              mutate(total.rev = neglog(total.rev)) %>%
                              mutate(total.rev.lag = neglog(total.rev.lag)) ,
                          outcome = "net.trans.change.lag2",
                          treatment = "defeat",
                          covs = c("total.rev.lag",
                                   "defeat.2007", "defeat.2011", "defeat.2016",
                                   "power.central", "num.candidates", "num.elected", "centralnominated"),
                          blockvar = "year",
                          unit.index = "prov",
                          time.index = "year",
                          method = "unit",
                          unbiased.se = TRUE,
                          qoi="att") # wfe under RI

## WLE persistent
weighted.long.lag <- riwfe(data = plan %>%
                               mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                               mutate(period.prov = as.factor(paste(prov, period))) %>%
                               mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                               group_by(prov, period) %>%
                               fill(num.candidates:num.closewin) %>%
                               mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                               ungroup %>%
                               filter(defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                               filter(!is.na(net.trans.change.lag2)) %>%
                               mutate(net.trans.change.lag2 = neglog(net.trans.change.lag2)) %>%
                               mutate(total.rev = neglog(total.rev)) %>%
                               mutate(total.rev.lag = neglog(total.rev.lag)),
                           outcome = "net.trans.change.lag2",
                           treatment = "defeat",
                           covs = c("total.rev.lag",
                                    "defeat.2007", "defeat.2011", "defeat.2016",
                                    "power.central", "num.candidates", "num.elected", "centralnominated"),
                           clus = "period.prov",
                           block = "period",
                           unit.index = "prov",
                           time.index = "year",
                           method = "unit",
                           qoi = "att") # wfe under RI

### tabulate placebo results ###
lay <- rbind(c(NA,1,1,2,2,3,3,4,4, NA),
             seq(5,14))
syp.fe.lag <- grid.arrange(layout_matrix = lay,
                           heights=c(1,10),
                           widths=c(1,rep(3,8), 1),
                           textGrob("Separate Panels, One-year"),
                           textGrob("Separate Panels, Persistent"),
                           textGrob("Pooled Panel, FE"),
                           textGrob("Pooled Panel, WFE"),
                           y.axe(-20,25),
                           rireg.plot(panel1.2011.lag, scale=F, xmin=-20, xmax=25, title = "2011"),
                           rireg.plot(panel1.2016.lag, scale=F, xmin=-20, xmax=25, title = "2016"),
                           rireg.plot(panell.2011.lag, scale=F, xmin=-20, xmax=25, title = "2011"),
                           rireg.plot(panell.2016.lag, scale=F, xmin=-20, xmax=25, title = "2016"),
                           rireg.plot(pool.one.lag, scale=F, xmin=-20, xmax=25, title = "One-year"),
                           rireg.plot(pool.long.lag, scale=F, xmin=-20, xmax=25, title = "Persistent"),
                           rireg.plot(weighted.one.lag, scale=F, xmin=-20, xmax=25, title = "One-year"),
                           rireg.plot(weighted.long.lag, scale=F, xmin=-20, xmax=25, title = "Persistent"),
                           y.axe(-20,25) + scale_x_continuous(position = "top"))
ggsave("SYP_FE_LAG.png", plot=syp.fe.lag, path="../../figure", width = 10, height = 3.5, units="in")

#### Synthetic Control ####

## 2016
Synth.att.2016 <- riSynth(data = plan.Synth %>% filter(closewin.2016==1 | defeat.2016==1),
                          outcome = "net.trans.change",
                          treatment = "defeat",
                          covs = c("total.rev", "defeat.2007", "defeat.2011", "power.central"),
                          treatment.year = 2017,
                          pretreatment.year = c(2007:2016),
                          posttreatment.year = NULL,
                          unit.variable = "prov.num",
                          unit.names.variable = "prov",
                          time.variable = "year",
                          include.past.Y = TRUE,
                          snowfall = TRUE)

Synth.att.2016.lead <- riSynth(data = plan.Synth %>% filter(closewin.2016==1 | defeat.2016==1),
                               outcome = "net.trans.change",
                               treatment = "defeat.lead",
                               covs = c("total.rev", "defeat.2007", "defeat.2011", "power.central"),
                               treatment.year = 2016,
                               pretreatment.year = c(2007:2015),
                               posttreatment.year = NULL,
                               unit.variable = "prov.num",
                               unit.names.variable = "prov",
                               time.variable = "year",
                               include.past.Y = TRUE,
                               snowfall = TRUE)

Synth.att.2016.lead2 <- riSynth(data = plan.Synth %>% filter(closewin.2016==1 | defeat.2016==1),
                                outcome = "net.trans.change",
                                treatment = "defeat.lead2",
                                covs = c("total.rev", "defeat.2007", "defeat.2011", "power.central"),
                                treatment.year = 2015,
                                pretreatment.year = c(2007:2014),
                                posttreatment.year = c(2016),
                                unit.variable = "prov.num",
                                unit.names.variable = "prov",
                                time.variable = "year",
                                include.past.Y = TRUE,
                                snowfall = TRUE)

### tabulate all results ###
lay <- rbind(c(NA, 1,2,2, NA),
             c(3,4,5,6,7))
syp.Synth <- grid.arrange(layout_matrix = lay,
                          heights=c(1,10),
                          widths=c(.5,rep(3,3), .5),
                          textGrob("Synthetic Control ATT"),
                          textGrob("Synthetic Control ATT, Placebo Treatments"),
                          y.axe(-20,25),
                          riSynth.plot(Synth.att.2016, xmin=-20, xmax=25, title="2016"),
                          riSynth.plot(Synth.att.2016.lead, xmin=-20, xmax=25, title="2015"),
                          riSynth.plot(Synth.att.2016.lead2, xmin=-20, xmax=25, title="2014"),
                          y.axe(-20,25) + scale_x_continuous(position = "top"))
ggsave("SYP_Synth.png", plot=syp.Synth, width=6, height=3.5, unit="in", path="../../figure")

#### Fixed Effects Regression for Development Expenditure ####

## Panel one year

# 2011
panel1.2011.dev <- rireg(data = prov %>%
                             filter(year < 2013 & year > 2008) %>% # avoid post-treatment
                             filter(defeat.2011!=0 | closewin.2011!=0) %>%
                             mutate(defeat = defeat*as.numeric(year==2012)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(year==2012)) %>%
                             # mutate(num.elected = num.elected*as.numeric(year==2012)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(year==2012)) %>%
                             mutate(defeat.past = defeat.2007*as.numeric(year==2012)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(dev.exp.change)),
                         outcome = "neglog(dev.exp.change)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011",
                                   "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         blockvar = "year") # lm under RI

panel1.2011.admin <- rireg(data = prov %>%
                               filter(year < 2013 & year > 2008) %>% # avoid post-treatment
                               filter(defeat.2011!=0 | closewin.2011!=0) %>%
                               mutate(defeat = defeat*as.numeric(year==2012)) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(year==2012)) %>%
                               # mutate(num.elected = num.elected*as.numeric(year==2012)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(year==2012)) %>%
                               mutate(defeat.past = defeat.2007*as.numeric(year==2012)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(year==2008)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                               filter(!is.na(admin.exp.change)),
                           outcome = "neglog(admin.exp.change)",
                           treatment = "defeat",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011",
                                     "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           blockvar = "year") # lm under RI

## Panel persistent

# 2011
panell.2011.dev <- rireg(data = prov %>%
                             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                             mutate(period.prov = as.factor(paste(prov, period))) %>%
                             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                             group_by(prov, period) %>%
                             fill(num.candidates:num.closewin) %>%
                             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                             ungroup %>%
                             filter(year < 2017 & year > 2008) %>%
                             filter(defeat.2011!=0 | closewin.2011!=0) %>%
                             mutate(defeat = defeat*as.numeric(period==3)) %>%
                             # mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
                             # mutate(num.elected = num.elected*as.numeric(period==3)) %>%
                             # mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
                             mutate(defeat.past = defeat.2007*as.numeric(period==3)) %>%
                             mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                             filter(!is.na(dev.exp.change)),
                         outcome = "neglog(dev.exp.change)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.past", "defeat.2007", "defeat.2011",
                                   "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         clus = "period.prov",
                         block = "period") # lm under RI

panell.2011.admin <- rireg(data = prov %>%
                               mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                               mutate(period.prov = as.factor(paste(prov, period))) %>%
                               mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                               group_by(prov, period) %>%
                               fill(num.candidates:num.closewin) %>%
                               mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                               ungroup %>%
                               filter(year < 2017 & year > 2008) %>%
                               filter(defeat.2011!=0 | closewin.2011!=0) %>%
                               mutate(defeat = defeat*as.numeric(period==3)) %>%
                               # mutate(num.candidates = num.candidates*as.numeric(period==3)) %>%
                               # mutate(num.elected = num.elected*as.numeric(period==3)) %>%
                               # mutate(centralnominated = centralnominated*as.numeric(period==3)) %>%
                               mutate(defeat.past = defeat.2007*as.numeric(period==3)) %>%
                               mutate(defeat.2007 = defeat.2007*as.numeric(period==2)) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                               filter(!is.na(dev.exp.change)),
                           outcome = "neglog(admin.exp.change)",
                           treatment = "defeat",
                           covs = c( "neglog(total.rev.lag)",
                                     "defeat.past", "defeat.2007", "defeat.2011",
                                     "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                           clus = "period.prov",
                           block = "period") # lm under RI

## Pooled one year

pool.one.dev <- rireg(data = prov %>%
                          filter(year<2013 & year > 2008) %>%
                          filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                          filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                          filter(!is.na(dev.exp.change)),
                      outcome = "neglog(dev.exp.change)",
                      treatment = "defeat",
                      covs = c( "neglog(total.rev.lag)",
                                "defeat.2007", "defeat.2011",
                                "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                      blockvar = "year") # lm under RI

pool.one.admin <- rireg(data = prov %>%
                            filter(year<2013 & year > 2008) %>%
                            filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                            filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                            filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                            filter(!is.na(admin.exp.change)),
                        outcome = "neglog(admin.exp.change)",
                        treatment = "defeat",
                        covs = c( "neglog(total.rev.lag)",
                                  "defeat.2007", "defeat.2011",
                                  "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                        blockvar = "year") # lm under RI

## Pooled persistent

pool.long.dev <- rireg(data = prov %>%
                           mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                           mutate(period.prov = as.factor(paste(prov, period))) %>%
                           mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                           group_by(prov, period) %>%
                           fill(num.candidates:num.closewin) %>%
                           mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                           ungroup %>%
                           filter(year > 2008) %>%
                           filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                           filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                           filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                           filter(!is.na(dev.exp.change)),
                       outcome = "neglog(dev.exp.change)",
                       treatment = "defeat",
                       covs = c( "neglog(total.rev.lag)",
                                 "defeat.2007", "defeat.2011",
                                 "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                       clus = "period.prov",
                       block = "period") # lm under RI

pool.long.admin <- rireg(data = prov %>%
                             mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>%
                             mutate(period.prov = as.factor(paste(prov, period))) %>%
                             mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                             group_by(prov, period) %>%
                             fill(num.candidates:num.closewin) %>%
                             mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                             ungroup %>%
                             filter(year > 2008) %>%
                             filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                             filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                             filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                             filter(!is.na(admin.exp.change)),
                         outcome = "neglog(admin.exp.change)",
                         treatment = "defeat",
                         covs = c( "neglog(total.rev.lag)",
                                   "defeat.2007", "defeat.2011",
                                   "factor(num.candidates)", "factor(num.elected)", "factor(centralnominated)", "factor(prov)", "factor(year)"),
                         clus = "period.prov",
                         block = "period") # lm under RI

## WFE one year

weighted.one.dev <- riwfe(data = prov %>%
                              filter(year<2013 & year>2008) %>%
                              filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                              filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                              filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                              filter(!is.na(dev.exp.change)) %>%
                              mutate(dev.exp.change = neglog(dev.exp.change)) %>%
                              mutate(total.rev = neglog(total.rev)) %>%
                              mutate(total.rev.lag = neglog(total.rev.lag)),
                          outcome = "dev.exp.change",
                          treatment = "defeat",
                          covs = c("total.rev.lag",
                                   "defeat.2007", "defeat.2011",
                                   "num.candidates", "num.elected", "centralnominated"),
                          blockvar = "year",
                          unit.index = "prov",
                          time.index = "year",
                          method = "unit",
                          unbiased.se = TRUE,
                          qoi="att") # wfe under RI

weighted.one.admin <- riwfe(data = prov %>%
                                filter(year<2013 & year>2008) %>%
                                filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                                filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                                filter(!is.na(dev.exp.change)) %>%
                                mutate(admin.exp.change = neglog(admin.exp.change)) %>%
                                mutate(total.rev = neglog(total.rev)) %>%
                                mutate(total.rev.lag = neglog(total.rev.lag)),
                            outcome = "admin.exp.change",
                            treatment = "defeat",
                            covs = c("total.rev.lag",
                                     "defeat.2007", "defeat.2011",
                                     "num.candidates", "num.elected", "centralnominated"),
                            blockvar = "year",
                            unit.index = "prov",
                            time.index = "year",
                            method = "unit",
                            unbiased.se = TRUE,
                            qoi="att") # wfe under RI

## WFE persistent
weighted.long.dev <- riwfe(data = prov %>%
                               mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                               mutate(period.prov = as.factor(paste(prov, period))) %>%
                               mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                               group_by(prov, period) %>%
                               fill(num.candidates:num.closewin) %>%
                               mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                               ungroup %>%
                               filter(year > 2008) %>%
                               filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                               filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                               filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                               filter(!is.na(dev.exp.change)) %>%
                               mutate(dev.exp.change = neglog(dev.exp.change)) %>%
                               mutate(total.rev = neglog(total.rev)) %>%
                               mutate(total.rev.lag = neglog(total.rev.lag)),
                           outcome = "dev.exp.change",
                           treatment = "defeat",
                           covs = c("total.rev.lag",
                                    "defeat.2007", "defeat.2011",
                                    "num.candidates", "num.elected", "centralnominated"),
                           clus = "period.prov",
                           block = "period",
                           unit.index = "prov",
                           time.index = "year",
                           method = "unit",
                           unbiased.se = TRUE,
                           qoi="att") # wfe under RI

weighted.long.admin <- riwfe(data = prov %>%
                                 mutate(period = as.numeric(cut(year, breaks = c(-Inf, 2007, 2011, 2016, Inf), right = T))) %>% ## "filing down" treatment
                                 mutate(period.prov = as.factor(paste(prov, period))) %>%
                                 mutate_each(funs(ifelse(.==0, NA, .)), num.candidates:num.closewin) %>%
                                 group_by(prov, period) %>%
                                 fill(num.candidates:num.closewin) %>%
                                 mutate_each(funs(ifelse(is.na(.), 0, .)), num.candidates:num.closewin) %>%
                                 ungroup %>%
                                 filter(year > 2008) %>%
                                 filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0) %>%
                                 filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                 filter(!is.na(total.rev) & !is.na(total.rev.lag)) %>%
                                 filter(!is.na(dev.exp.change)) %>%
                                 mutate(admin.exp.change = neglog(admin.exp.change)) %>%
                                 mutate(total.rev = neglog(total.rev)) %>%
                                 mutate(total.rev.lag = neglog(total.rev.lag)),
                             outcome = "admin.exp.change",
                             treatment = "defeat", # should use defeat.cum to account for 2 consecutive defeats, but wfe does not support non-binary treatments
                             covs = c("total.rev.lag",
                                      "defeat.2007", "defeat.2011",
                                      "num.candidates", "num.elected", "centralnominated"),
                             clus = "period.prov",
                             block = "period",
                             unit.index = "prov",
                             time.index = "year",
                             method = "unit",
                             unbiased.se = TRUE,
                             qoi="att") # wfe under RI

### tabulate all results ###
lay <- rbind(c(NA,1,2,2,NA),
             c(NA,3,4,5,NA),
             c(6:10),
             c(NA,11,11,11,NA),
             c(12:16),
             c(NA,17,17,17,NA))
syp.fe.mech <- grid.arrange(layout_matrix = lay,
                            heights=c(2,1,10,1,10,1),
                            widths =c(.5,rep(3,3),.5),
                            textGrob("Separate Panels, One-year"),
                            textGrob("Pooled Panel, FE"),
                            textGrob("2011"),
                            textGrob("One-year"),
                            textGrob("Persistent"),
                            y.axe(-10,10),
                            rireg.plot(panel1.2011.dev, scale=F, xmin=-10, xmax=10, title = " "),
                            rireg.plot(pool.one.dev, scale=F, xmin=-10, xmax=10, title = " "),
                            rireg.plot(pool.long.dev, scale=F, xmin=-10, xmax=10, title = " "),
                            y.axe(-10,10) + scale_x_continuous(position = "top"),
                            textGrob("Dependent variable: Development Expenditures"),
                            y.axe(-10,10),
                            rireg.plot(panel1.2011.admin, scale=F, xmin=-10, xmax=10, title = " "),
                            rireg.plot(pool.one.admin, scale=F, xmin=-10, xmax=10, title = " "),
                            rireg.plot(pool.long.admin, scale=F, xmin=-10, xmax=10, title = " "),
                            y.axe(-10,10)+ scale_x_continuous(position = "top"),
                            textGrob("Dependent variable: Administrative Expenditures"))
ggsave("SYP_FE_MECH.png", plot=syp.fe.mech, path="../../figure", width=6, height=5, unit="in")

#### Alternative hypothesis #1: punish bureaucrats managing elections ####

## Total number of promotions
# 2007
leadert.2007 <- rireg(data = leaders2007 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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
leadert.2011 <- rireg(data = leaders2011 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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

## ANY promotion

# 2007
leadera.2007 <- rireg(data = leaders2007 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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
leadera.2011 <- rireg(data = leaders2011 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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

### tabulate alternative hypothesis #1 results ###
lay <- rbind(c(NA,1,1,2,2,NA),
             seq(3,8))
syp.ri.lead <- grid.arrange(layout_matrix = lay,
                            heights=c(1,10),
                            widths=c(1,rep(3,4), 1),
                            textGrob("DV: Number of Promotions"),
                            textGrob("DV: Any promotion"),
                            y.axe(-1,1),
                            rireg.plot(leadert.2007, scale=F, xmin=-1, xmax=1, title = "2007"),
                            rireg.plot(leadert.2011, scale=F, xmin=-1, xmax=1, title = "2011"),
                            rireg.plot(leadera.2007, scale=F, xmin=-1, xmax=1, title = "2007"),
                            rireg.plot(leadera.2011, scale=F, xmin=-1, xmax=1, title = "2011"),
                            y.axe(-1,1) + scale_x_continuous(position = "top"))
ggsave("SYP_RI_LEAD.png", plot=syp.ri.lead, path="../../figure", width = 8, height = 3.5, units="in")

#### Alternative hypothesis #2: punish bureaucrats managing province before elections ####

## Total number of promotions
# 2006
leadert.2006 <- rireg(data = leaders2006 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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

# 2010
leadert.2010 <- rireg(data = leaders2010 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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

## ANY promotion

# 2006
leadera.2006 <- rireg(data = leaders2006 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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
leadera.2010 <- rireg(data = leaders2010 %>%
                          filter(defeat!=0 | closewin!=0) %>%
                          filter(prov!="Ha Noi" & prov!="TP HCM") %>%
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

### tabulate alternative hypothesis #1 results ###
lay <- rbind(c(NA,1,1,2,2,NA),
             seq(3,8))
syp.ri.lead.lag <- grid.arrange(layout_matrix = lay,
                            heights=c(1,10),
                            widths=c(1,rep(3,4), 1),
                            textGrob("DV: Number of Promotions"),
                            textGrob("DV: Any promotion"),
                            y.axe(-1,1),
                            rireg.plot(leadert.2006, scale=F, xmin=-1, xmax=1, title = "2007"),
                            rireg.plot(leadert.2010, scale=F, xmin=-1, xmax=1, title = "2011"),
                            rireg.plot(leadera.2006, scale=F, xmin=-1, xmax=1, title = "2007"),
                            rireg.plot(leadera.2010, scale=F, xmin=-1, xmax=1, title = "2011"),
                            y.axe(-1,1) + scale_x_continuous(position = "top"))
ggsave("SYP_RI_LEAD_LAG.png", plot=syp.ri.lead.lag, path="../../figure", width = 8, height = 3.5, units="in")

#### Appendix: No high-level manipulation ####
lay <- rbind(c(NA,1,1,1,NA),
             c(2,3,4,5,6),
             c(NA,7,7,7,NA),
             c(8,9,10,11,12),
             c(NA,13,13,13,NA),
             c(14,15,16,17,18))
benford.digit.test <- grid.arrange(layout_matrix = lay,
                                   heights=c(1,10,1,10,1,10),
                                   widths=c(.5,3,3,3,.5),
                                   textGrob("2011 Turnout"),
                                   y.axe(0,.4,flip=F),
                                   digit.test(turnout2011$turnout.voters, digit=1, title="1st Digit"),
                                   digit.test(turnout2011$turnout.voters, digit=2, title="2nd Digit"),
                                   digit.test(turnout2011$turnout.voters, digit=3, title="3rd Digit"),
                                   y.axe(0,.4,flip=F) + scale_y_continuous(position = "right"),
                                   textGrob("2011 Invalid Votes"),
                                   y.axe(0,.4,flip=F),
                                   digit.test(turnout2011$invalid.votes, digit=1, title="1st Digit"),
                                   digit.test(turnout2011$invalid.votes, digit=2, title="2nd Digit"),
                                   digit.test(turnout2011$invalid.votes, digit=3, title="3rd Digit"),
                                   y.axe(0,.4,flip=F) + scale_y_continuous(position = "right"),
                                   textGrob("2016 Vote Counts"),
                                   y.axe(0,.4,flip=F),
                                   digit.test(result2016$vote, digit=1, title="1st Digit"),
                                   digit.test(result2016$vote, digit=2, title="2nd Digit"),
                                   digit.test(result2016$vote, digit=3, title="3rd Digit"),
                                   y.axe(0,.4,flip=F) + scale_y_continuous(position = "right"))
ggsave("BENFORD_DIGIT_TEST.png", plot=benford.digit.test, path="../../figure", width = 8, height = 10, units="in")
