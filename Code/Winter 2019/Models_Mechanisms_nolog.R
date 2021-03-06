setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2018/Merge_All.R")

## This file builds upon the old Analyze_NewRI_Mechanism file
## to use all three primary approaches (linear reg, RI-RDD, 
## Generalized Synthetic Control) to analyze the mechanism
## by which the treatment influences the outcome

#### First mechanism: Development expenditure ####

### Linear regressions

## Note that lm results disappear if using first differenced variables
## instead of stock variable... but for the synth results first differenced
## variables yield even stronger results

## I end up not using this even though there's some potential
## Apparently not taking the log ensures better linearity >> may be better
## but it also screws up the synthetic control results

## one-year effect

dat_dev_lme <- prov

# 2016 only
dat_dev_2016_1 <- dat_dev_lme %>%
  filter(year < 2018 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")

# without covariates
lm_dev_2016_1a <- lm(dev.exp ~ defeat + defeat.2016 +
                       factor(prov) + factor(year) + factor(prov)*year,
                     data = dat_dev_2016_1) # just lm
summary(lm_dev_2016_1a)

# adding time-variant covariate: lagged total revenue
lm_dev_2016_1b <- lm(dev.exp ~ defeat + defeat.2016 +
                       total.rev.lag +
                       factor(prov) + factor(year) + factor(prov)*year,
                     data = dat_dev_2016_1) # just lm
summary(lm_dev_2016_1b)

# adding time-invariant covariates instead of prov FE
lm_dev_2016_1c <- lm(dev.exp ~ defeat + defeat.2016 +
                       num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                       factor(year) + factor(prov)*year,
                     data = dat_dev_2016_1) # just lm
summary(lm_dev_2016_1c)

## persistent effect

dat_dev_2016_p <- dat_dev_lme %>%
  mutate(defeat = defeat.2016*(year >= 2017)) %>%
  filter(year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")

# without covariates
lm_dev_2016_pa <- lm(dev.exp ~ defeat + defeat.2016 +
                       factor(prov) + factor(year) + factor(prov)*year,
                     data = dat_dev_2016_p) # just lm
summary(lm_dev_2016_pa)

# adding time-variant covariate: lagged total revenue
lm_dev_2016_pb <- lm(dev.exp ~ defeat + defeat.2016 +
                       total.rev.lag +
                       factor(prov) + factor(year) + factor(prov)*year,
                     data = dat_dev_2016_p) # just lm
summary(lm_dev_2016_pb)

# adding time-invariant covariates instead of prov FE
lm_dev_2016_pc <- lm(dev.exp ~ defeat + defeat.2016 +
                       num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                       factor(year) + factor(prov)*year,
                     data = dat_dev_2016_p) # just lm
summary(lm_dev_2016_pc)

### RDD

prov_prov <- unique(prov$prov)
candidates2016rdd_dev <- candidates2016rdd %>% filter(prov %in% prov_prov)

## Randomization distribution of candidate-level treatment vector
index <- which(candidates2016rdd_dev$closedefeat == 1 | candidates2016rdd_dev$closewin == 1)

set.seed(02139)
nsim <- 1000
candidates2016_dev.closedefeat.randomized <- replicate(nsim, rbinom(length(index), 1, .5))


## Create province-year-level treatment vectors
treatment.2016.randomized <- apply(candidates2016_dev.closedefeat.randomized, 2, function(t) {
  candidates2016rdd_rand <- candidates2016rdd_dev
  
  candidates2016rdd_rand$closedefeat <- 0
  candidates2016rdd_rand$closedefeat[index] <- t
  
  candidates2016rdd_rand$closewin <- 0
  candidates2016rdd_rand$closewin[index] <- 1-t
  
  treatment_rand <- treatment_generate(candidates2016rdd_rand)
})

## Observed treatment effects

treatment.2016.observed <- treatment_generate(candidates2016rdd_dev)

dat_dev_rdd <- prov %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004) %>% # number of provinces were different before 2004
  mutate(defeat.2016 = treatment.2016.observed)

# One year effect 

rdd_dev_2016_1 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                       wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_dev_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2017)) %>%
    filter(year < 2018 & year > 2012) %>%
    drop_na(defeat, dev.exp)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(dev.exp ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_dev_2016_1$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_dev_2016_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_dev_2016_1$beta[1] > rdd_dev_2016_1$beta[-1], na.rm = T)
mean(rdd_dev_2016_1$beta[1] < rdd_dev_2016_1$beta[-1], na.rm = T)

2*min(mean(rdd_dev_2016_1$beta[1] > rdd_dev_2016_1$beta[-1], na.rm = T),
      mean(rdd_dev_2016_1$beta[1] < rdd_dev_2016_1$beta[-1], na.rm = T))

plot(density(na.omit(rdd_dev_2016_1$beta)))
abline(v = rdd_dev_2016_1$beta[1])

# Wilcoxon rank sum
mean(rdd_dev_2016_1$wilcox[1] > rdd_dev_2016_1$wilcox[-1])
mean(rdd_dev_2016_1$wilcox[1] < rdd_dev_2016_1$wilcox[-1])

2*min(mean(rdd_dev_2016_1$wilcox[1] > rdd_dev_2016_1$wilcox[-1]),
      mean(rdd_dev_2016_1$wilcox[1] < rdd_dev_2016_1$wilcox[-1]))

plot(density(rdd_dev_2016_1$wilcox))
abline(v = rdd_dev_2016_1$wilcox[1])

# Persistent effect 

rdd_dev_2016_p <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                       wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_dev_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year>=2017)) %>%
    filter(year > 2012) %>%
    drop_na(defeat, dev.exp)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(dev.exp ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_dev_2016_p$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_dev_2016_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_dev_2016_p$beta[1] > rdd_dev_2016_p$beta[-1], na.rm = T)
mean(rdd_dev_2016_p$beta[1] < rdd_dev_2016_p$beta[-1], na.rm = T)

2*min(mean(rdd_dev_2016_p$beta[1] > rdd_dev_2016_p$beta[-1], na.rm = T),
      mean(rdd_dev_2016_p$beta[1] < rdd_dev_2016_p$beta[-1], na.rm = T))

plot(density(na.omit(rdd_dev_2016_p$beta)))
abline(v = rdd_dev_2016_p$beta[1])

# Wilcoxon rank sum
mean(rdd_dev_2016_p$wilcox[1] > rdd_dev_2016_p$wilcox[-1])
mean(rdd_dev_2016_p$wilcox[1] < rdd_dev_2016_p$wilcox[-1])

2*min(mean(rdd_dev_2016_p$wilcox[1] > rdd_dev_2016_p$wilcox[-1]),
      mean(rdd_dev_2016_p$wilcox[1] < rdd_dev_2016_p$wilcox[-1]))

plot(density(rdd_dev_2016_p$wilcox))
abline(v = rdd_dev_2016_p$wilcox[1])

### Synthetic control

dat_dev_synth <- prov %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")
panelView(dev.exp ~ defeat, data = dat_dev_synth, index = c("prov", "year"))

## One year change -- no effect!

dat_dev_2016synth_1 <- dat_dev_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) %>%
  drop_na(dev.exp)
panelView(dev.exp ~ defeat, data = dat_dev_2016synth_1, index = c("prov", "year"))

system.time(
  synth_dev_2016_1 <- gsynth(dev.exp ~ treat + defeat + total.rev.lag, 
                             data = dat_dev_2016synth_1, 
                             index = c("prov", "year"), force = "two-way",
                             EM = TRUE,
                             CV = TRUE, r = c(0, 5), 
                             se = TRUE, 
                             inference = "parametric", nboots = 1000,
                             parallel = TRUE, cores = 6,
                             na.rm = TRUE)
)

print(synth_dev_2016_1)
plot(synth_dev_2016_1)
plot(synth_dev_2016_1, type = "counterfactual")

## Persistent change -- ignore cause mising 2018 budget data for Phu Yen

dat_dev_2016synth_p <- dat_dev_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  drop_na(dev.exp)
panelView(dev.exp ~ treat, data = dat_dev_2016synth_p, index = c("prov", "year"))

system.time(
  synth_dev_2016_p <- gsynth(dev.exp ~ treat + defeat + total.rev.lag, 
                             data = dat_dev_2016synth_p, 
                             index = c("prov", "year"), force = "two-way",
                             EM = TRUE,
                             CV = TRUE, r = c(0, 5), 
                             se = TRUE, 
                             inference = "parametric", nboots = 1000,
                             parallel = TRUE, cores = 6,
                             na.rm = TRUE)
)

print(synth_dev_2016_p)
plot(synth_dev_2016_p)
plot(synth_dev_2016_p, type = "counterfactual")

#### Second mechanism: Administrative expenditure ####

### Linear regressions

## one-year effect

dat_admin_lme <- prov

# 2016 only
dat_admin_2016_1 <- dat_admin_lme %>%
  filter(year < 2018 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")

# without covariates
lm_admin_2016_1a <- lm(admin.exp ~ defeat + defeat.2016 +
                         factor(prov) + factor(year) + factor(prov)*year,
                       data = dat_admin_2016_1) # just lm
summary(lm_admin_2016_1a)

# adding time-variant covariate: lagged total revenue
lm_admin_2016_1b <- lm(admin.exp ~ defeat + defeat.2016 +
                         total.rev.lag +
                         factor(prov) + factor(year) + factor(prov)*year,
                       data = dat_admin_2016_1) # just lm
summary(lm_admin_2016_1b)

# adding time-invariant covariates instead of prov FE
lm_admin_2016_1c <- lm(admin.exp ~ defeat + defeat.2016 +
                         num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                         factor(year) + factor(prov)*year,
                       data = dat_admin_2016_1) # just lm
summary(lm_admin_2016_1c)

## persistent effect

# 2016 only
dat_admin_2016_p <- dat_admin_lme %>%
  filter(year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")

# without covariates
lm_admin_2016_pa <- lm(admin.exp ~ defeat + defeat.2016 +
                         factor(prov) + factor(year) + factor(prov)*year,
                       data = dat_admin_2016_p) # just lm
summary(lm_admin_2016_pa)

# adding time-variant covariate: lagged total revenue
lm_admin_2016_pb <- lm(admin.exp ~ defeat + defeat.2016 +
                         total.rev.lag +
                         factor(prov) + factor(year) + factor(prov)*year,
                       data = dat_admin_2016_p) # just lm
summary(lm_admin_2016_pb)

# adding time-invariant covariates instead of prov FE
lm_admin_2016_pc <- lm(admin.exp ~ defeat + defeat.2016 +
                         num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                         factor(year) + factor(prov)*year,
                       data = dat_admin_2016_p) # just lm
summary(lm_admin_2016_pc)

### RDD

dat_admin_rdd <- prov %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004) %>% # number of provinces were different before 2004
  mutate(defeat.2016 = treatment.2016.observed)

# One year effect

rdd_admin_2016_1 <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                         wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_admin_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year==2017)) %>%
    filter(year > 2012) %>%
    drop_na(defeat, admin.exp)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(admin.exp ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year) + factor(prov)*year,
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_admin_2016_1$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_admin_2016_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_admin_2016_1$beta[1] > rdd_admin_2016_1$beta[-1], na.rm = T)
mean(rdd_admin_2016_1$beta[1] < rdd_admin_2016_1$beta[-1], na.rm = T)

2*min(mean(rdd_admin_2016_1$beta[1] > rdd_admin_2016_1$beta[-1], na.rm = T),
      mean(rdd_admin_2016_1$beta[1] < rdd_admin_2016_1$beta[-1], na.rm = T))

plot(density(na.omit(rdd_admin_2016_1$beta)))
abline(v = rdd_admin_2016_1$beta[1])

# Wilcoxon rank sum
mean(rdd_admin_2016_1$wilcox[1] > rdd_admin_2016_1$wilcox[-1])
mean(rdd_admin_2016_1$wilcox[1] < rdd_admin_2016_1$wilcox[-1])

2*min(mean(rdd_admin_2016_1$wilcox[1] > rdd_admin_2016_1$wilcox[-1]),
      mean(rdd_admin_2016_1$wilcox[1] < rdd_admin_2016_1$wilcox[-1]))

plot(density(rdd_admin_2016_1$wilcox))
abline(v = rdd_admin_2016_1$wilcox[1])

# Persistent effect

rdd_admin_2016_p <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
                         wilcox = rep(NA, ncol(treatment.2016.randomized) +1))
for (i in 0:ncol(treatment.2016.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.observed  
  } else {
    treatment <- treatment.2016.randomized[,i]
  }
  
  
  dat <- dat_admin_rdd %>%
    mutate(defeat.2016 = treatment) %>% 
    mutate(defeat = defeat.2016*as.numeric(year>=2017)) %>%
    filter(year > 2012) %>%
    drop_na(defeat, admin.exp)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(admin.exp ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  total.rev.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_admin_2016_p$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_admin_2016_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_admin_2016_p$beta[1] > rdd_admin_2016_p$beta[-1], na.rm = T)
mean(rdd_admin_2016_p$beta[1] < rdd_admin_2016_p$beta[-1], na.rm = T)

2*min(mean(rdd_admin_2016_p$beta[1] > rdd_admin_2016_p$beta[-1], na.rm = T),
      mean(rdd_admin_2016_p$beta[1] < rdd_admin_2016_p$beta[-1], na.rm = T))

plot(density(na.omit(rdd_admin_2016_p$beta)))
abline(v = rdd_admin_2016_p$beta[1])

# Wilcoxon rank sum
mean(rdd_admin_2016_p$wilcox[1] > rdd_admin_2016_p$wilcox[-1])
mean(rdd_admin_2016_p$wilcox[1] < rdd_admin_2016_p$wilcox[-1])

2*min(mean(rdd_admin_2016_p$wilcox[1] > rdd_admin_2016_p$wilcox[-1]),
      mean(rdd_admin_2016_p$wilcox[1] < rdd_admin_2016_p$wilcox[-1]))

plot(density(rdd_admin_2016_p$wilcox))
abline(v = rdd_admin_2016_p$wilcox[1])

### Synthetic control

dat_admin_synth <- prov %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM")
panelView(admin.exp ~ defeat, data = dat_admin_synth, index = c("prov", "year"))

## One year change 

dat_admin_2016synth_1 <- dat_admin_synth %>%
  mutate(treat = defeat.2016*as.numeric(year==2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(year < 2018) %>%
  drop_na(admin.exp)
panelView(admin.exp ~ treat, data = dat_admin_2016synth_1, index = c("prov", "year"))

system.time(
  synth_admin_2016_1 <- gsynth(admin.exp ~ treat + defeat + total.rev.lag, 
                               data = dat_admin_2016synth_1, 
                               index = c("prov", "year"), force = "two-way",
                               EM = TRUE,
                               CV = TRUE, r = c(0, 5), 
                               se = TRUE, 
                               inference = "parametric", nboots = 1000,
                               parallel = TRUE, cores = 6,
                               na.rm = TRUE)
)

print(synth_admin_2016_1)
plot(synth_admin_2016_1)
plot(synth_admin_2016_1, type = "counterfactual")

## Persistent change -- also ignore this result for the same reason we drop this result for dev exp

dat_admin_2016synth_p <- dat_admin_synth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2017)) %>%
  #filter(defeat.2016!=0 | closewin.2016!=0) %>%
  drop_na(admin.exp)
panelView(admin.exp ~ defeat, data = dat_admin_2016synth_p, index = c("prov", "year"))

system.time(
  synth_admin_2016_p <- gsynth(admin.exp ~ treat + defeat + total.rev.lag, 
                               data = dat_admin_2016synth_p, 
                               index = c("prov", "year"), force = "two-way",
                               EM = TRUE,
                               CV = TRUE, r = c(0, 5), 
                               se = TRUE, 
                               inference = "parametric", nboots = 1000,
                               parallel = TRUE, cores = 6,
                               na.rm = TRUE)
)

print(synth_admin_2016_p)
plot(synth_admin_2016_p)
plot(synth_admin_2016_p, type = "counterfactual", raw = "all")

#### Third mechanism: Bargaining power ####

# Looking at provinces where central nominees failed to clear
# the 50 percent threshold (s.t. no local nominees get elected instead)
# shows there's still positive effect..

plot(synth_2016_p, type = "counterfactual", id = "Soc Trang")
plot(synth_2016_p, type = "counterfactual", id = "Can Tho")

#### Fourth mechanism: Concurrent punishment ####

# governing in 2006
dat_promo_2006 <- leaders2006 %>%
  filter(defeat!=0 | closewin!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  group_by(prov) %>%
  summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
            num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
            num.promoted = sum(promoted*as.numeric(year>2007 & year <2011)),
            any.promoted = num.promoted > 0,
            num.leaders = max(num.leaders*as.numeric(year>2007 & year <2011)),
            retiring = sum(retiring*as.numeric(year>2007 & year <2011)),
            defeat=max(defeat),
            num.candidates=max(num.candidates),
            num.elected=max(num.elected),
            any.centralnominees=max(any.centralnominees))

table(promoted = dat_promo_2006$num.promoted,
      defeat = dat_promo_2006$defeat)

# number of promotions
summary(lm(num.promoted ~ defeat,
           data = dat_promo_2006))

fisher.test(x = dat_promo_2006$num.promoted, 
            y = dat_promo_2006$defeat, alternative = "less")

# any promotion
summary(lm(any.promoted ~ defeat,
           data = dat_promo_2006))

fisher.test(x = dat_promo_2006$any.promoted, 
            y = dat_promo_2006$defeat, alternative = "less")

# governing in 2007
dat_promo_2007 <- leaders2007 %>%
  filter(defeat!=0 | closewin!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  group_by(prov) %>%
  summarise(promoted.prior = sum(promoted*as.numeric(year==2006)),
            num.leaders.prior = sum(num.leaders*as.numeric(year==2006)),
            num.promoted = sum(promoted*as.numeric(year>2007 & year <2011)),
            any.promoted = num.promoted > 0,
            num.leaders = max(num.leaders*as.numeric(year>2007 & year <2011)),
            retiring = sum(retiring*as.numeric(year>2007 & year <2011)),
            defeat=max(defeat),
            num.candidates=max(num.candidates),
            num.elected=max(num.elected),
            any.centralnominees=max(any.centralnominees))

table(promoted = dat_promo_2007$num.promoted,
      defeat = dat_promo_2007$defeat)

# number of promotions
summary(lm(num.promoted ~ defeat,
           data = dat_promo_2007))

fisher.test(x = dat_promo_2007$num.promoted, 
            y = dat_promo_2007$defeat, alternative = "less")

# any promotion
summary(lm(any.promoted ~ defeat,
           data = dat_promo_2007))

fisher.test(x = dat_promo_2007$any.promoted, 
            y = dat_promo_2007$defeat, alternative = "less")

# governing in 2010
dat_promo_2010 <- leaders2010 %>%
  filter(defeat!=0 | closewin!=0) %>%
  filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
  group_by(prov) %>%
  summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
            num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
            num.promoted = sum(promoted*as.numeric(year>2011 & year < 2016)),
            any.promoted = num.promoted > 0,
            num.leaders = max(num.leaders*as.numeric(year>2011 & year < 2016)),
            defeat=max(defeat),
            num.candidates=max(num.candidates),
            num.elected=max(num.elected),
            any.centralnominees=max(any.centralnominees))

table(promoted = dat_promo_2010$num.promoted,
      defeat = dat_promo_2010$defeat)

# number of promotions
summary(lm(num.promoted ~ defeat,
           data = dat_promo_2010))

fisher.test(x = dat_promo_2010$num.promoted, 
            y = dat_promo_2010$defeat, alternative = "less")

# any promotion
summary(lm(any.promoted ~ defeat,
           data = dat_promo_2010))

fisher.test(x = dat_promo_2010$any.promoted, 
            y = dat_promo_2010$defeat, alternative = "less")

# governing in 2011
dat_promo_2011 <- leaders2011 %>%
  filter(defeat!=0 | closewin!=0) %>%
  filter(prov!="Ha Noi" & prov!="Ho Chi Minh City") %>%
  group_by(prov) %>%
  summarise(promoted.prior = sum(promoted*as.numeric(year==2010)),
            num.leaders.prior = sum(num.leaders*as.numeric(year==2010)),
            num.promoted = sum(promoted*as.numeric(year>2011 & year < 2016)),
            any.promoted = num.promoted > 0,
            num.leaders = max(num.leaders*as.numeric(year>2011 & year < 2016)),
            defeat=max(defeat),
            num.candidates=max(num.candidates),
            num.elected=max(num.elected),
            any.centralnominees=max(any.centralnominees))

table(promoted = dat_promo_2011$num.promoted,
      defeat = dat_promo_2011$defeat)

# number of promotions
summary(lm(num.promoted ~ defeat,
           data = dat_promo_2011))

fisher.test(x = dat_promo_2011$num.promoted, 
            y = dat_promo_2011$defeat, alternative = "less")

# any promotion
summary(lm(any.promoted ~ defeat,
           data = dat_promo_2011))

fisher.test(x = dat_promo_2011$any.promoted, 
            y = dat_promo_2011$defeat, alternative = "less")
