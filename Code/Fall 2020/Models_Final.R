#### THIS FILE CONTAINS ALL THE MODELS THAT WILL APPEAR IN THE FINAL PAPER ####
library(gsynth)
library(panelView)
library(multiwayvcov)
library(lmtest)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Fall 2020/Merge_All.R")


#### Linear regressions results for 2016 elections ####

dat_lme <- plan %>%
  filter(year < 2020 & year > 2012) %>%
  filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  drop_na(net.trans.log, net.trans.lag)

## one-year change

# 2016 only -- positive and significant effect
dat_2016_1 <- dat_lme  %>%
  filter(year < 2018)

# without covariates
lm_2016_1a <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1a)
vcov_2016_1a <- cluster.vcov(lm_2016_1a, cluster = ~ prov)
coeftest(lm_2016_1a, vcov = vcov_2016_1a)

# adding time-variant covariate: lagged total revenue
lm_2016_1b <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2016_1) # just lm
summary(lm_2016_1b)
vcov_2016_1b <- cluster.vcov(lm_2016_1b, cluster = ~ prov)
coeftest(lm_2016_1b, vcov = vcov_2016_1b)

# adding time-invariant covariates instead of prov FE
lm_2016_1c <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                  factor(year),
                data = dat_2016_1) # just lm
summary(lm_2016_1c)
vcov_2016_1c <- cluster.vcov(lm_2016_1c, cluster = ~ prov)
coeftest(lm_2016_1c, vcov = vcov_2016_1c)


## persistent change

# 2016 only -- positive effect
dat_2016_p <- dat_lme %>%
  mutate(defeat.true = defeat.true.2016*(year >= 2017)) 

# without covariates
lm_2016_pa <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_pa)
vcov_2016_pa <- cluster.vcov(lm_2016_pa, cluster = ~ prov)
coeftest(lm_2016_pa, vcov = vcov_2016_pa)

# adding time-variant covariate: lagged total revenue
lm_2016_pb <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat_2016_p) # just lm
summary(lm_2016_pb)
vcov_2016_pb <- cluster.vcov(lm_2016_pb, cluster = ~ prov)
coeftest(lm_2016_pb, vcov = vcov_2016_pb)

# adding time-invariant covariates instead of prov FE
lm_2016_pc <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                   factor(year),
                 data = dat_2016_p) # just lm
summary(lm_2016_pc)
vcov_2016_pc <- cluster.vcov(lm_2016_pc, cluster = ~ prov)
coeftest(lm_2016_pc, vcov = vcov_2016_pc)

## placebo 1: one-year change as if election was held in 2013 st outcome is in 2014

# 2016 results under placebo1
dat_2016_1_placebo2014 <- dat_lme %>%
  mutate(defeat_placebo2014 = defeat.true.2016 * as.numeric(year == 2014)) %>%
  filter(year < 2015)

# without covariates
lm_2016_1_placebo2014a <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                          factor(prov) + factor(year),
                        data = dat_2016_1_placebo2014) # just lm
summary(lm_2016_1_placebo2014a)
vcov_2016_1_placebo2014a <- cluster.vcov(lm_2016_1_placebo2014a, cluster = ~ prov)
coeftest(lm_2016_1_placebo2014a, vcov = vcov_2016_1_placebo2014a)

# adding time-variant covariate: lagged total revenue
lm_2016_1_placebo2014b <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2016_1_placebo2014) # just lm
summary(lm_2016_1_placebo2014b)
vcov_2016_1_placebo2014b <- cluster.vcov(lm_2016_1_placebo2014b, cluster = ~ prov)
coeftest(lm_2016_1_placebo2014b, vcov = vcov_2016_1_placebo2014b)

# adding time-invariant covariates instead of province FEs
lm_2016_1_placebo2014c <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                               factor(year),
                             data = dat_2016_1_placebo2014) # just lm
summary(lm_2016_1_placebo2014c)
vcov_2016_1_placebo2014c <- cluster.vcov(lm_2016_1_placebo2014c, cluster = ~ prov)
coeftest(lm_2016_1_placebo2014c, vcov = vcov_2016_1_placebo2014c)

## placebo 1: persistent change as if election was held in 2013 st outcome is in 2014

# 2016 results under placebo1
dat_2016_p_placebo2014 <- dat_lme %>%
  mutate(defeat_placebo2014 = defeat.true.2016 * as.numeric(year >= 2014)) %>%
  filter(year < 2017)

# without covariates
lm_2016_p_placebo2014a <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2014) # just lm
summary(lm_2016_p_placebo2014a)
vcov_2016_p_placebo2014a <- cluster.vcov(lm_2016_p_placebo2014a, cluster = ~ prov)
coeftest(lm_2016_p_placebo2014a, vcov = vcov_2016_p_placebo2014a)

# adding time-variant covariate: lagged total revenue
lm_2016_p_placebo2014b <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2014) # just lm
summary(lm_2016_p_placebo2014b)
vcov_2016_p_placebo2014b <- cluster.vcov(lm_2016_p_placebo2014b, cluster = ~ prov)
coeftest(lm_2016_p_placebo2014b, vcov = vcov_2016_p_placebo2014b)

# adding time-invariant covariates instead of province FEs
lm_2016_p_placebo2014c <- lm(net.trans.change.log ~ defeat_placebo2014 + defeat.true.2016 +
                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                               factor(year),
                             data = dat_2016_p_placebo2014) # just lm
summary(lm_2016_p_placebo2014c)
vcov_2016_p_placebo2014c <- cluster.vcov(lm_2016_p_placebo2014c, cluster = ~ prov)
coeftest(lm_2016_p_placebo2014c, vcov = vcov_2016_p_placebo2014c)

## placebo 2: one-year change as if election was held in 2014 st outcome is in 2015

# 2016 results under placebo 2
dat_2016_1_placebo2015 <- dat_lme %>%
  mutate(defeat_placebo2015 = defeat.true.2016 * as.numeric(year == 2015)) %>%
  filter(year < 2016)

# without covariates
lm_2016_1_placebo2015a <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                              factor(prov) + factor(year),
                            data = dat_2016_1_placebo2015) # just lm
summary(lm_2016_1_placebo2015a)
vcov_2016_1_placebo2015a <- cluster.vcov(lm_2016_1_placebo2015a, cluster = ~ prov)
coeftest(lm_2016_1_placebo2015a, vcov = vcov_2016_1_placebo2015a)

# adding time-invariant covariates don't change things since model is close to saturated
# lm_2016_1_placebo2015 <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.2016 +
#                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
#                               factor(prov) + factor(year),
#                             data = dat_2016_1_placebo2015) # just lm
# summary(lm_2016_1_placebo2015)

# adding time-variant covariate: lagged total revenue
lm_2016_1_placebo2015b <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                              total.rev.log.lag +
                              factor(prov) + factor(year),
                            data = dat_2016_1_placebo2015) # just lm
summary(lm_2016_1_placebo2015b)
vcov_2016_1_placebo2015b <- cluster.vcov(lm_2016_1_placebo2015b, cluster = ~ prov)
coeftest(lm_2016_1_placebo2015b, vcov = vcov_2016_1_placebo2015b)

# adding time-invariant covariates instead of prov FEs
lm_2016_1_placebo2015c <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                              num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                              factor(year),
                            data = dat_2016_1_placebo2015) # just lm
summary(lm_2016_1_placebo2015c)
vcov_2016_1_placebo2015c <- cluster.vcov(lm_2016_1_placebo2015c, cluster = ~ prov)
coeftest(lm_2016_1_placebo2015c, vcov = vcov_2016_1_placebo2015c)

## placebo 2: persistent change as if election was held in 2014 st outcome is in 2015

# 2016 results under placebo 2
dat_2016_p_placebo2015 <- dat_lme %>%
  mutate(defeat_placebo2015 = defeat.true.2016 * as.numeric(year >= 2015)) %>%
  filter(year < 2017)

# without covariates
lm_2016_p_placebo2015a <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2015) # just lm
summary(lm_2016_p_placebo2015a)
vcov_2016_p_placebo2015a <- cluster.vcov(lm_2016_p_placebo2015a, cluster = ~ prov)
coeftest(lm_2016_p_placebo2015a, vcov = vcov_2016_p_placebo2015a)

# adding time-invariant covariates don't change things since model is close to saturated
# lm_2016_p_placebo2015 <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.2016 +
#                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
#                               factor(prov) + factor(year),
#                             data = dat_2016_p_placebo2015) # just lm
# summary(lm_2016_p_placebo2015)

# adding time-variant covariate: lagged total revenue
lm_2016_p_placebo2015b <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2015) # just lm
summary(lm_2016_p_placebo2015b)
vcov_2016_p_placebo2015b <- cluster.vcov(lm_2016_p_placebo2015b, cluster = ~ prov)
coeftest(lm_2016_p_placebo2015b, vcov = vcov_2016_p_placebo2015b)

# adding time-invariant covariates instead of prov FEs
lm_2016_p_placebo2015c <- lm(net.trans.change.log ~ defeat_placebo2015 + defeat.true.2016 +
                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                               factor(year),
                             data = dat_2016_p_placebo2015) # just lm
summary(lm_2016_p_placebo2015c)
vcov_2016_p_placebo2015c <- cluster.vcov(lm_2016_p_placebo2015c, cluster = ~ prov)
coeftest(lm_2016_p_placebo2015c, vcov = vcov_2016_p_placebo2015c)

## placebo 3: one-year change as if election was held in 2015 st outcome is in 2016

# 2016 results under placebo 3
dat_2016_1_placebo2016 <- dat_lme %>%
  mutate(defeat_placebo2016 = defeat.true.2016 * as.numeric(year == 2016)) %>%
  filter(year < 2017)

# without covariates
lm_2016_1_placebo2016a <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                              factor(prov) + factor(year),
                            data = dat_2016_1_placebo2016) # just lm
summary(lm_2016_1_placebo2016a)
vcov_2016_1_placebo2016a <- cluster.vcov(lm_2016_1_placebo2016a, cluster = ~ prov)
coeftest(lm_2016_1_placebo2016a, vcov = vcov_2016_1_placebo2016a)

# adding time-variant covariate: lagged total revenue
lm_2016_1_placebo2016b <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                              total.rev.log.lag +
                              factor(prov) + factor(year),
                            data = dat_2016_1_placebo2016) # just lm
summary(lm_2016_1_placebo2016b)
vcov_2016_1_placebo2016b <- cluster.vcov(lm_2016_1_placebo2016b, cluster = ~ prov)
coeftest(lm_2016_1_placebo2016b, vcov = vcov_2016_1_placebo2016b)

# adding time-invariant covariates instead of prov FEs
lm_2016_1_placebo2016c <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                              num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                              factor(year),
                            data = dat_2016_1_placebo2016) # just lm
summary(lm_2016_1_placebo2016c)
vcov_2016_1_placebo2016c <- cluster.vcov(lm_2016_1_placebo2016c, cluster = ~ prov)
coeftest(lm_2016_1_placebo2016c, vcov = vcov_2016_1_placebo2016c)

## placebo 3: persistent change as if election was held in 2015 st outcome is in 2016
## the same as one-year change

# 2016 results under placebo 3
dat_2016_p_placebo2016 <- dat_lme %>%
  mutate(defeat_placebo2016 = defeat.true.2016 * as.numeric(year >= 2016)) %>%
  filter(year < 2017)

# without covariates
lm_2016_p_placebo2016a <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2016) # just lm
summary(lm_2016_p_placebo2016a)
vcov_2016_p_placebo2016a <- cluster.vcov(lm_2016_p_placebo2016a, cluster = ~ prov)
coeftest(lm_2016_p_placebo2016a, vcov = vcov_2016_p_placebo2016a)

# adding time-variant covariate: lagged total revenue
lm_2016_p_placebo2016b <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2016_p_placebo2016) # just lm
summary(lm_2016_p_placebo2016b)
vcov_2016_p_placebo2016b <- cluster.vcov(lm_2016_p_placebo2016b, cluster = ~ prov)
coeftest(lm_2016_p_placebo2016b, vcov = vcov_2016_p_placebo2016b)

# adding time-invariant covariates instead of prov FEs
lm_2016_p_placebo2016c <- lm(net.trans.change.log ~ defeat_placebo2016 + defeat.true.2016 +
                               num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                               factor(year),
                             data = dat_2016_p_placebo2016) # just lm
summary(lm_2016_p_placebo2016c)
vcov_2016_p_placebo2016c <- cluster.vcov(lm_2016_p_placebo2016c, cluster = ~ prov)
coeftest(lm_2016_p_placebo2016c, vcov = vcov_2016_p_placebo2016c)

#### RDD results for 2016 elections ####

# refer to Models_RDD.R for bandwidth selection process

# remove Hanoi and Ho Chi Minh city
candidates2016rdd <- candidates2016 %>% 
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong")

lower.final <- -11.5
upper.final <- 7.25

candidates2016rdd$closedefeat <- as.numeric(candidates2016rdd$centralnominated==1 &
                                              candidates2016rdd$margin >= lower.final &
                                              candidates2016rdd$defeat == 1)
candidates2016rdd$closewin <- as.numeric(candidates2016rdd$centralnominated==1 &
                                           candidates2016rdd$margin <= upper.final &
                                           candidates2016rdd$defeat != 1)

# function to generate province summaries from candidate-level data

treatment_generate_2016 <- function(candidates, years = c(2013:2019)) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(closedefeat = max(closedefeat, na.rm = T),
      closewin = max(closewin, na.rm=T),
      num.closedefeat = sum(closedefeat, na.rm=T),
      num.closewin = sum(closewin, na.rm=T),
      year = 2016)
  
  # province-level vector
  provinces_treatment <- provinces$closedefeat
  provinces_treatment[provinces$closedefeat == 0 & provinces$closewin == 0] <- NA
  
  # province-year vector
  provinces_year_treatment <- rep(provinces_treatment, each = length(years))
  
  return(provinces_year_treatment)
}

## Randomization distribution of candidate-level treatment vector
index <- which(candidates2016rdd$closedefeat == 1 | candidates2016rdd$closewin == 1)

set.seed(02139)
nsim <- 10000
candidates2016.closedefeat.randomized <- replicate(nsim, rbinom(length(index), 1, .5))


## Create province-year-level treatment vectors
treatment.2016.randomized <- apply(candidates2016.closedefeat.randomized, 2, function(t) {
  candidates2016rdd_rand <- candidates2016rdd
  
  candidates2016rdd_rand$closedefeat <- 0
  candidates2016rdd_rand$closedefeat[index] <- t
  
  candidates2016rdd_rand$closewin <- 0
  candidates2016rdd_rand$closewin[index] <- 1-t
  
  treatment_rand <- treatment_generate_2016(candidates2016rdd_rand)
})

## Observed treatment effects

treatment.2016.observed <- treatment_generate_2016(candidates2016rdd)

dat_rdd <- plan %>%
  filter(year > 2012 & year < 2020) %>% # number of provinces were different before 2004
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  drop_na(net.trans.log, net.trans.lag)

# One year effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2016_1 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2017)) %>%
    filter(year < 2018) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 + 
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_1$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_1$beta[1] > rdd_2016_1$beta[-1])
mean(rdd_2016_1$beta[1] < rdd_2016_1$beta[-1])

2*min(mean(rdd_2016_1$beta[1] > rdd_2016_1$beta[-1]),
      mean(rdd_2016_1$beta[1] < rdd_2016_1$beta[-1]))

plot(density(rdd_2016_1$beta))
abline(v = rdd_2016_1$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_1$wilcox[1] > rdd_2016_1$wilcox[-1])
mean(rdd_2016_1$wilcox[1] < rdd_2016_1$wilcox[-1])

2*min(mean(rdd_2016_1$wilcox[1] > rdd_2016_1$wilcox[-1]),
      mean(rdd_2016_1$wilcox[1] < rdd_2016_1$wilcox[-1]))

plot(density(rdd_2016_1$wilcox))
abline(v = rdd_2016_1$wilcox[1])


# multiple years effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2016_p <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year>=2017)) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_p$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_p$beta[1] > rdd_2016_p$beta[-1])
mean(rdd_2016_p$beta[1] < rdd_2016_p$beta[-1])

2*min(mean(rdd_2016_p$beta[1] > rdd_2016_p$beta[-1]),
      mean(rdd_2016_p$beta[1] < rdd_2016_p$beta[-1]))

plot(density(rdd_2016_p$beta))
abline(v = rdd_2016_p$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_p$wilcox[1] > rdd_2016_p$wilcox[-1])
mean(rdd_2016_p$wilcox[1] < rdd_2016_p$wilcox[-1])

2*min(mean(rdd_2016_p$wilcox[1] > rdd_2016_p$wilcox[-1]),
      mean(rdd_2016_p$wilcox[1] < rdd_2016_p$wilcox[-1]))

plot(density(rdd_2016_p$wilcox))
abline(v = rdd_2016_p$wilcox[1])


## placebo 1: one-year change as if election was held in 2013 st outcome is in 2014

# One year effect
rdd_2016_1_placebo2014 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                           wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2014)) %>%
    filter(year < 2015) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_1_placebo2014$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_1_placebo2014$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2016_1_placebo2014$beta[1] > rdd_2016_1_placebo2014$beta[-1]),
      mean(rdd_2016_1_placebo2014$beta[1] < rdd_2016_1_placebo2014$beta[-1]))

plot(density(rdd_2016_1_placebo2014$beta))
abline(v = rdd_2016_1_placebo2014$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2016_1_placebo2014$wilcox[1] > rdd_2016_1_placebo2014$wilcox[-1]),
      mean(rdd_2016_1_placebo2014$wilcox[1] < rdd_2016_1_placebo2014$wilcox[-1]))

plot(density(rdd_2016_1_placebo2014$wilcox))
abline(v = rdd_2016_1_placebo2014$wilcox[1])


# multiple year effect
rdd_2016_p_placebo2014 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                           wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year>=2014)) %>%
    filter(year < 2016) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_p_placebo2014$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_p_placebo2014$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2016_p_placebo2014$beta[1] > rdd_2016_p_placebo2014$beta[-1]),
      mean(rdd_2016_p_placebo2014$beta[1] < rdd_2016_p_placebo2014$beta[-1]))

plot(density(rdd_2016_p_placebo2014$beta))
abline(v = rdd_2016_p_placebo2014$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2016_p_placebo2014$wilcox[1] > rdd_2016_p_placebo2014$wilcox[-1]),
      mean(rdd_2016_p_placebo2014$wilcox[1] < rdd_2016_p_placebo2014$wilcox[-1]))

plot(density(rdd_2016_p_placebo2014$wilcox))
abline(v = rdd_2016_p_placebo2014$wilcox[1])


## placebo 2: one-year change as if election was held in 2014 st outcome is in 2015

# One year effect
rdd_2016_1_placebo2015 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2015)) %>%
    filter(year < 2016) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_1_placebo2015$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_1_placebo2015$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2016_1_placebo2015$beta[1] > rdd_2016_1_placebo2015$beta[-1]),
      mean(rdd_2016_1_placebo2015$beta[1] < rdd_2016_1_placebo2015$beta[-1]))

plot(density(rdd_2016_1_placebo2015$beta))
abline(v = rdd_2016_1_placebo2015$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2016_1_placebo2015$wilcox[1] > rdd_2016_1_placebo2015$wilcox[-1]),
      mean(rdd_2016_1_placebo2015$wilcox[1] < rdd_2016_1_placebo2015$wilcox[-1]))

plot(density(rdd_2016_1_placebo2015$wilcox))
abline(v = rdd_2016_1_placebo2015$wilcox[1])


# multiple year effect
rdd_2016_p_placebo2015 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year>=2015)) %>%
    filter(year < 2017) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_p_placebo2015$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_p_placebo2015$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2016_p_placebo2015$beta[1] > rdd_2016_p_placebo2015$beta[-1]),
      mean(rdd_2016_p_placebo2015$beta[1] < rdd_2016_p_placebo2015$beta[-1]))

plot(density(rdd_2016_p_placebo2015$beta))
abline(v = rdd_2016_p_placebo2015$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2016_p_placebo2015$wilcox[1] > rdd_2016_p_placebo2015$wilcox[-1]),
      mean(rdd_2016_p_placebo2015$wilcox[1] < rdd_2016_p_placebo2015$wilcox[-1]))

plot(density(rdd_2016_p_placebo2015$wilcox))
abline(v = rdd_2016_p_placebo2015$wilcox[1])

## placebo 3: one-year change as if election was held in 2015 st outcome is in 2016

# One year effect
rdd_2016_1_placebo2016 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2016)) %>%
    filter(year < 2017) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_1_placebo2016$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_1_placebo2016$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2016_1_placebo2016$beta[1] > rdd_2016_1_placebo2016$beta[-1]),
      mean(rdd_2016_1_placebo2016$beta[1] < rdd_2016_1_placebo2016$beta[-1]))

plot(density(rdd_2016_1_placebo2016$beta))
abline(v = rdd_2016_1_placebo2016$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2016_1_placebo2016$wilcox[1] > rdd_2016_1_placebo2016$wilcox[-1]),
      mean(rdd_2016_1_placebo2016$wilcox[1] < rdd_2016_1_placebo2016$wilcox[-1]))

plot(density(rdd_2016_1_placebo2016$wilcox))
abline(v = rdd_2016_1_placebo2016$wilcox[1])


# multiple year effect is the same as one-year effect as only one-year is in the treated
# if placebo treatment is in 2016

#### Synthetic control results ####

dat_synth <- plan %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(year < 2020) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong")
panelView(net.trans.log ~ defeat, data = dat_synth, index = c("prov", "year"))

## One year change

dat_2016synth_1 <- dat_synth %>%
  mutate(treat = defeat.true.2016*as.numeric(year==2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) %>%
  drop_na(net.trans.log)
panelView(net.trans.log ~ defeat, data = dat_2016synth_1, index = c("prov", "year"))

system.time(
  synth_2016_1 <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 10),
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_1)
plot(synth_2016_1)
plot(synth_2016_1, type = "counterfactual")

## Persistent change

dat_2016synth_p <- dat_synth %>%
  mutate(treat = defeat.true.2016*as.numeric(year>=2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  #mutate(net.trans.log = net.trans.log + log(1000000)) %>%
  drop_na(net.trans.log)
panelView(net.trans.log ~ treat, data = dat_2016synth_p, index = c("prov", "year"))

system.time(
  synth_2016_p <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 10), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_p)
plot(synth_2016_p)
plot(synth_2016_p, type = "counterfactual")


## Placebo 1: 2014 treatment, as if election is in 2013

# One year change

dat_2016synth_1_placebo2014 <- dat_synth %>%
  mutate(treat = defeat.true.2016*as.numeric(year==2014)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2015) %>%
  drop_na(net.trans.log)

system.time(
  synth_2016_1_placebo2014 <- gsynth(net.trans.log ~ treat + defeat, 
                                     data = dat_2016synth_1_placebo2014, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 9),
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_1_placebo2014)
plot(synth_2016_1_placebo2014)

# Persistent change

dat_2016synth_p_placebo2014 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2014)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2016) %>%
  drop_na(net.trans.log)

system.time(
  synth_2016_p_placebo2014 <- gsynth(net.trans.log ~ treat + defeat, 
                                     data = dat_2016synth_p_placebo2014, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 9), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_p_placebo2014)
plot(synth_2016_p_placebo2014)

## Placebo 2: 2015 treatment, as if election is in 2014

# One year change

dat_2016synth_1_placebo2015 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2015)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2016) %>%
  drop_na(net.trans.log)

system.time(
  synth_2016_1_placebo2015 <- gsynth(net.trans.log ~ treat + defeat, 
                                     data = dat_2016synth_1_placebo2015, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 10), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_1_placebo2015)
plot(synth_2016_1_placebo2015)

# Persistent change

dat_2016synth_p_placebo2015 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2015)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2017) %>%
  drop_na(net.trans.log)

system.time(
  synth_2016_p_placebo2015 <- gsynth(net.trans.log ~ treat + defeat , 
                                     data = dat_2016synth_p_placebo2015, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 10), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_p_placebo2015)
plot(synth_2016_p_placebo2015)

## Placebo 3: 2016 treatment, as if election is in 2015

# One year change

dat_2016synth_1_placebo2016 <- dat_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2016)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2017) %>%
  drop_na(net.trans.log)

system.time(
  synth_2016_1_placebo2016 <- gsynth(net.trans.log ~ treat + defeat, 
                                     data = dat_2016synth_1_placebo2016, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 10), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2016_1_placebo2016)
plot(synth_2016_1_placebo2016)

