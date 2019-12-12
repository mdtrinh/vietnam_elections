#### THIS FILE CONTAINS ALL THE MODELS THAT ARE DONE IN RESPONSE TO FIRST ROUND OF REVIEW ####

library(gsynth)
library(panelView)
library(multiwayvcov)
library(lmtest)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("C:/Users/Minh Trinh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Winter 2019/Merge_All.R")

##########################
# LFE RESULTS FOR 2011
##########################

#### 2011 Results

dat_lme <- plan

## one-year change

# 2011 only
dat_2011_1 <- dat_lme %>%
  filter(year < 2013 & year > 2008) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  drop_na(net.trans.log, net.trans.lag)

# without covariates
lm_2011_1a <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   factor(prov) + factor(year),
                 data = dat_2011_1) # just lm
summary(lm_2011_1a)
vcov_2011_1a <- cluster.vcov(lm_2011_1a, cluster = ~ prov)
coeftest(lm_2011_1a, vcov = vcov_2011_1a)

# adding time-variant covariate: lagged total revenue
lm_2011_1b <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2011_1) # just lm
summary(lm_2011_1b)
vcov_2011_1b <- cluster.vcov(lm_2011_1b, cluster = ~ prov)
coeftest(lm_2011_1b, vcov = vcov_2011_1b)

# adding time-invariant covariates instead of prov FE
lm_2011_1c <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                   factor(year),
                 data = dat_2011_1) # just lm
summary(lm_2011_1c)
vcov_2011_1c <- cluster.vcov(lm_2011_1c, cluster = ~ prov)
coeftest(lm_2011_1c, vcov = vcov_2011_1c)


## persistent change

# 2011 only
dat_2011_p <- dat_lme %>%
  mutate(defeat = defeat.2011*(year >= 2011)) %>%
  filter(year > 2008 & year < 2016) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(!is.na(net.trans.change.log))

# without covariates
lm_2011_pa <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   factor(prov) + factor(year),
                 data = dat_2011_p) # just lm
summary(lm_2011_pa)
vcov_2011_pa <- cluster.vcov(lm_2011_pa, cluster = ~ prov)
coeftest(lm_2011_pa, vcov = vcov_2011_pa)

# adding time-variant covariate: lagged total revenue
lm_2016_pb <- lm(net.trans.change.log ~ defeat + defeat.2016 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2016_p) # just lm
summary(lm_2016_pb)
vcov_2016_pb <- cluster.vcov(lm_2016_pb, cluster = ~ prov)
coeftest(lm_2016_pb, vcov = vcov_2016_pb)

# adding time-invariant covariates instead of prov FE
lm_2016_pc <- lm(net.trans.change.log ~ defeat + defeat.2016 +
                   num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                   factor(year),
                 data = dat_2016_p) # just lm
summary(lm_2016_pc)
vcov_2016_pc <- cluster.vcov(lm_2016_pc, cluster = ~ prov)
coeftest(lm_2016_pc, vcov = vcov_2016_pc)

##########################
# RDD RESULTS FOR 2011
##########################

#### Bandwidth selection using candidate-level data ####

# Note: Only possible to select bandwidth on one side
# i.e. search for maximum winning vote share such that
# candidates who won with fewer than this vote share
# are no different from candidates who lost

## Pretreatment covariates for window selector
covs <- c("age", "male", "party", "years_party", "degree2", "power", "num.candidates", "num.seats")

candidates2011$years_party[is.na(candidates2011$years_party)] <- 0

# set minimum threshold to be 57.5 s.t. there are9 treated and 14 control
# (Cattaneo et al 2013 recommends 10 on both sides but we can start from slightly lower)
candidates2011 %>% 
  filter(centralnominated == 1 & 
           (percentage < 57.5 | result == 0)) %>% 
  group_by(defeat) %>% summarise(n = n())

windows <- seq(57.5, 65, by = .25)

pvalues.windows <- sapply(windows, function(w) {
  index <- which(candidates2011$centralnominated == 1 &
                   (candidates2011$percentage < w | candidates2011$result == 0))
  perms <- ri::genperms(candidates2011$defeat[index])
  
  pvalues <- sapply(covs, function(c) {
    res <- omni.ate(as.numeric(candidates2011[[c]][index]),
                    candidates2011[["defeat"]][index], 
                    perms = perms)
    
    p <- res$p.value
    
    return(p)
  })
  
  pvalues.min <- min(pvalues)
  
  return(pvalues.min)
})

# Tabulate all the p-values
m <- pvalues.windows
names(m) <- windows
m <- data.frame(cbind(windows, pvalues.windows)) %>% spread(key = upper, value = pvalues.windows) %>% arrange(desc(lower)) %>% as.matrix

rownames(m) <- m[,1]
m <- m[,-1]

# Create a heatmap
image(1:length(m), 1, as.matrix(m), col = terrain.colors(60), axes = FALSE, xlab = "upper", ylab = "lower")
axis(1, 1:length(m), names(m))
for (x in 1:length(m))
    text(x, 1, as.numeric(m[x] < .15))

# Candidates thresholds:
# Smallset: 59.5 
# Larger, more treated: 60.25 << seems stable
# Largest, even more treated: 61.00 << most observations, most power

# Sample size for each threshold:
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 59.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 60.25 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 61.00 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())

# remove Hanoi and Ho Chi Minh city
candidates2011rdd <- candidates2011 %>% filter(prov!="Ha Noi" & prov!="TP HCM")

window.final <- 61.00

candidates2011rdd$closewin <- as.numeric(candidates2011rdd$centralnominated==1 &
                                           candidates2011rdd$percentage <= window.final &
                                           candidates2011rdd$defeat != 1)

# function to generate province summaries from candidate-level data

treatment_generate_2011 <- function(candidates) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(defeat = max(defeat, na.rm = T),
              closewin = max(closewin, na.rm=T),
              num.closewin = sum(closewin, na.rm=T),
              year = 2016)
  
  # province-level vector
  provinces_treatment <- provinces$defeat
  provinces_treatment[provinces$defeat == 0 & provinces$closewin == 0] <- NA
  
  # province-year vector
  provinces_year_treatment <- rep(provinces_treatment, each = 13)
  
  return(provinces_year_treatment)
}

## Randomization distribution of candidate-level treatment vector
index <- which(candidates2011rdd$defeat == 1 | candidates2011rdd$closewin == 1)

set.seed(02139)
nsim <- 1000
candidates2011.defeat.randomized <- replicate(nsim, rbinom(length(index), 1, .5))


## Create province-year-level treatment vectors
treatment.2011.randomized <- apply(candidates2011.defeat.randomized, 2, function(t) {
  candidates2011rdd_rand <- candidates2011rdd
  
  candidates2011rdd_rand$defeat <- 0
  candidates2011rdd_rand$defeat[index] <- t
  
  candidates2011rdd_rand$closewin <- 0
  candidates2011rdd_rand$closewin[index] <- 1-t
  
  treatment_rand <- treatment_generate_2011(candidates2011rdd_rand)
})

## Observed treatment effects

treatment.2011.observed <- treatment_generate_2011(candidates2011rdd)

dat_rdd <- plan %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
  mutate(defeat.2011 = treatment.2011.observed)

## One year effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2011_1 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year==2012)) %>%
    filter(year < 2013 & year > 2008) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2011 + 
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2011_1$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2011_1$beta[1] > rdd_2011_1$beta[-1])
mean(rdd_2011_1$beta[1] < rdd_2011_1$beta[-1])

2*min(mean(rdd_2011_1$beta[1] > rdd_2011_1$beta[-1]),
      mean(rdd_2011_1$beta[1] < rdd_2011_1$beta[-1]))

plot(density(rdd_2011_1$beta))
abline(v = rdd_2011_1$beta[1])

# Wilcoxon rank sum
mean(rdd_2011_1$wilcox[1] > rdd_2011_1$wilcox[-1])
mean(rdd_2011_1$wilcox[1] < rdd_2011_1$wilcox[-1])

2*min(mean(rdd_2011_1$wilcox[1] > rdd_2011_1$wilcox[-1]),
      mean(rdd_2011_1$wilcox[1] < rdd_2011_1$wilcox[-1]))

plot(density(rdd_2011_1$wilcox))
abline(v = rdd_2011_1$wilcox[1])


## Multiple years effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2011_p <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year>=2012)) %>%
    filter(year > 2008 & year < 2016) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2011_p$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2011_p$beta[1] > rdd_2011_p$beta[-1])
mean(rdd_2011_p$beta[1] < rdd_2011_p$beta[-1])

2*min(mean(rdd_2011_p$beta[1] > rdd_2011_p$beta[-1]),
      mean(rdd_2011_p$beta[1] < rdd_2011_p$beta[-1]))

plot(density(rdd_2011_p$beta))
abline(v = rdd_2011_p$beta[1])

# Wilcoxon rank sum
mean(rdd_2011_p$wilcox[1] > rdd_2011_p$wilcox[-1])
mean(rdd_2011_p$wilcox[1] < rdd_2011_p$wilcox[-1])

2*min(mean(rdd_2011_p$wilcox[1] > rdd_2011_p$wilcox[-1]),
      mean(rdd_2011_p$wilcox[1] < rdd_2011_p$wilcox[-1]))

plot(density(rdd_2011_p$wilcox))
abline(v = rdd_2011_p$wilcox[1])


## placebo 1: one-year change as if election was held in 2009 st outcome is in 2010

# One year effect
rdd_2011_1_placebo2010 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year==2010)) %>%
    filter(year > 2008 & year < 2011) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2011_1_placebo2010$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_1_placebo2010$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2011_1_placebo2010$beta[1] > rdd_2011_1_placebo2010$beta[-1]),
      mean(rdd_2011_1_placebo2010$beta[1] < rdd_2011_1_placebo2010$beta[-1]))

plot(density(rdd_2011_1_placebo2010$beta))
abline(v = rdd_2011_1_placebo2010$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2011_1_placebo2010$wilcox[1] > rdd_2011_1_placebo2010$wilcox[-1]),
      mean(rdd_2011_1_placebo2010$wilcox[1] < rdd_2011_1_placebo2010$wilcox[-1]))

plot(density(rdd_2011_1_placebo2010$wilcox))
abline(v = rdd_2011_1_placebo2010$wilcox[1])


# multiple year effect
rdd_2011_p_placebo2010 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year>=2010)) %>%
    filter(year > 2008 & year < 2016) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2011_p_placebo2010$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_p_placebo2010$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2011_p_placebo2010$beta[1] > rdd_2011_p_placebo2010$beta[-1]),
      mean(rdd_2011_p_placebo2010$beta[1] < rdd_2011_p_placebo2010$beta[-1]))

plot(density(rdd_2011_p_placebo2010$beta))
abline(v = rdd_2011_p_placebo2010$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2011_p_placebo2010$wilcox[1] > rdd_2011_p_placebo2010$wilcox[-1]),
      mean(rdd_2011_p_placebo2010$wilcox[1] < rdd_2011_p_placebo2010$wilcox[-1]))

plot(density(rdd_2011_p_placebo2010$wilcox))
abline(v = rdd_2011_p_placebo2010$wilcox[1])

## placebo 2: one-year change as if election was held in 2010 st outcome is in 2011

# One year effect
rdd_2011_1_placebo2011 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year==2011)) %>%
    filter(year > 2008 & year < 2012) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2011 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2011_1_placebo2011$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_1_placebo2011$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2011_1_placebo2011$beta[1] > rdd_2011_1_placebo2011$beta[-1]),
      mean(rdd_2011_1_placebo2011$beta[1] < rdd_2011_1_placebo2011$beta[-1]))

plot(density(rdd_2011_1_placebo2011$beta))
abline(v = rdd_2011_1_placebo2011$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2011_1_placebo2011$wilcox[1] > rdd_2011_1_placebo2011$wilcox[-1]),
      mean(rdd_2011_1_placebo2011$wilcox[1] < rdd_2011_1_placebo2011$wilcox[-1]))

plot(density(rdd_2011_1_placebo2011$wilcox))
abline(v = rdd_2011_1_placebo2011$wilcox[1])

# multiple year effect not applicable since post-treatment years would still be included

##########################
# BALANCE TABLE FOR 2011
##########################

source("../../Code/Winter 2019/Clean_Covariates.R")

#### Variables

list_vars <- c("total.rev", "total.exp",
               "num.seats.2011", "num.candidates.2011", "num.centralnominees.2011",
               #"area_2010", "population_2010", "population_density_2010",
               "grdp", "monthly_income_2010","employment_rate_above15_2010",
               #"share_ag_land_2015",
               "infant_mortality_2010", "public_hosp_beds_2010", "schools_2010", "schools_primary_2010")

#### Balance Table for Linear Fixed Effects Model

dat_balance_lme <- plan %>% 
  filter(year == 2010) %>%
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  select(prov, total.rev, total.exp,
         num.candidates.2011, num.seats.2011, num.centralnominees.2011, 
         defeat.2011) %>%
  inner_join(covs %>% filter(year == 2010), by = "prov") %>%
  mutate(total.rev = total.rev/1000,
         total.exp = total.exp/1000)


treat_vector_lme <- dat_balance_lme[["defeat.2011"]]
table(treat_vector_lme)

set.seed(12345)
perms_lme <- genperms(treat_vector_lme)

balance_lme <- sapply(list_vars, function(c) {
  y <- dat_balance_lme[[c]]
  z <- treat_vector_lme
  
  mean_treated <- mean(dat_balance_lme[[c]][which(z == 1)])
  mean_control <- mean(dat_balance_lme[[c]][which(z == 0)])
  
  var_treated <- var(dat_balance_lme[[c]][which(z == 1)])
  var_control <- var(dat_balance_lme[[c]][which(z == 0)])
  
  diff = mean_treated - mean_control
  diff_std = diff/sqrt(var_control)
  
  ri_test <- omni.ate(Y = y, Z = z, perms = perms_lme)
  lm_test <- lm(y ~ z)
  
  return(c(mean_control = mean_control,
           mean_treated = mean_treated,
           diff = diff_std,
           se_ri = ri_test$se,
           p_ri = ri_test$p.value,
           se_lm = summary(lm_test)$coefficients["z",2],
           p_lm = summary(lm_test)$coefficients["z",4]))
})

#### Balance Table for Local Randomization RDD

treatment.2011.observed <- treatment_generate_2011(candidates2011rdd)

dat_balance_rdd <- plan %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
  mutate(defeat.2011 = treatment.2011.observed) %>%
  filter(year == 2010) %>%
  filter(!is.na(defeat.2011)) %>%
  select(prov, total.rev, total.exp,
         num.candidates.2011, num.seats.2011, num.centralnominees.2011, 
         defeat.2011) %>%
  inner_join(covs %>% filter(year == 2010), by = "prov") %>%
  mutate(total.rev = total.rev/1000,
         total.exp = total.exp/1000)

treat_vector_rdd <- dat_balance_rdd[["defeat.2011"]]
table(treat_vector_rdd)

set.seed(12345)
perms_rdd <- genperms(treat_vector_rdd)

balance_rdd <- sapply(list_vars, function(c) {
  y <- dat_balance_rdd[[c]]
  z <- treat_vector_rdd
  
  mean_treated = mean(dat_balance_rdd[[c]][which(z == 1)])
  mean_control = mean(dat_balance_rdd[[c]][which(z == 0)])
  
  var_treated <- var(dat_balance_rdd[[c]][which(z == 1)])
  var_control <- var(dat_balance_rdd[[c]][which(z == 0)])
  
  diff = mean_treated - mean_control
  diff_std = diff/sqrt(var_control)
  
  ri_test <- omni.ate(Y = y, Z = z, perms = perms_rdd)
  lm_test <- lm(y ~ z)
  
  return(c(mean_control = mean_control,
           mean_treated = mean_treated,
           diff = diff_std,
           se_ri = ri_test$se,
           p_ri = ri_test$p.value,
           se_lm = summary(lm_test)$coefficients["z",2],
           p_lm = summary(lm_test)$coefficients["z",4]))
})
