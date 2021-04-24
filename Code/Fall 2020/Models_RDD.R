#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Fall 2020/Merge_All.R")

##### RDD-inspired approach #1: Select a window in which assignment of defeat at the candidate-level
##### is as-good-as-random (see Cataneo Frandsen Titiunik 2015). Then for candidates who fall within this window,
##### flip a coin to randomly decide whether candidate loses or wins. Then construct province-level
##### treatment vector from the coin flip. In this sense treatment assignment approaches a complete
##### randomization procedure at the candidate level.

##### Results so far: significant effect (at .1 level) using standard inference, also non-significant
##### effect using randomization inference, with Wilcoxon rank sum getting "better" p-values

##### New results as of March 05: Good Wilcox results if using net.trans.log instead of net.trans.change
##### Problem is that the placebo results are also significant as well... which should not be the case

##### As of March 14: Final verdict is too stick with net.trans.change.log to ensure consistency
##### with lfe results.

#### Bandwidth selection using candidate-level data ####

## Pretreatment covariates for window selector
covariates <- c("age", "male", "party", "years_party", "degree2", "power", "num.candidates", "num.seats")

candidates2016$years_party[is.na(candidates2016$years_party)] <- 0

#candidates2016 <- candidates2016 %>% filter(!(prov %in% c("Ha Noi", "TP HCM")))

# set minimum window to be (-9, 5) st there are 10 treated and 11 control
# (Cattaneo et al 2013 recommends 10 on both sides but we can start from lower)
candidates2016 %>% filter(centralnominated == 1 & (margin > -9 & margin < 5)) %>% group_by(defeat) %>% summarise(n = n())

lower <- seq(-7, -12, by = -.25)
upper <- seq(5, 9, by = .25)
windows <- expand.grid(lower = lower,upper = upper)

pvalues.windows <- apply(windows, 1, function(w) {
  index <- which(candidates2016$centralnominated == 1 &
                   (candidates2016$margin > w["lower"] & candidates2016$margin < w["upper"]))
  perms <- ri::genperms(candidates2016$defeat[index])
  
  pvalues <- sapply(covariates, function(c) {
    res <- omni.ate(as.numeric(candidates2016[[c]][index]),
                    candidates2016[["defeat"]][index], perms = perms)
    
    p <- res$p.value
    
    return(p)
  })
  
  pvalues.min <- min(pvalues)
  
  return(pvalues.min)
})

# Tabulate all the p-values
m <- data.frame(cbind(windows, pvalues.windows)) %>% spread(key = upper, value = pvalues.windows) %>% arrange(desc(lower)) %>% as.matrix

rownames(m) <- m[,1]
m <- m[,-1]

# Create a heatmap
image(1:ncol(m), 1:nrow(m), t(m), col = terrain.colors(60), axes = FALSE, xlab = "upper", ylab = "lower")
axis(1, 1:ncol(m), colnames(m))
axis(2, 1:nrow(m), rownames(m))
for (x in 1:ncol(m))
  for (y in 1:nrow(m))
    text(x, y, as.numeric(m[y,x] < .15))

# Candidates windows:
# Smallset: (-7.75, 7.75) << probably most conservative
# Larger, more treated: (-10.5, 7.75) 
# Largest, even more treated: (-11.5, 7.25) << most observations, most power
# Symmetric: (-7.75, 7.75)

# Sample size in each windows:
candidates2016 %>% filter(centralnominated == 1 & (margin > -7.75 & margin < 7.75)) %>% group_by(defeat) %>% summarise(n = n())
candidates2016 %>% filter(centralnominated == 1 & (margin > -10.5 & margin < 7.75)) %>% group_by(defeat) %>% summarise(n = n())
candidates2016 %>% filter(centralnominated == 1 & (margin > -11.5 & margin < 7.25)) %>% group_by(defeat) %>% summarise(n = n())
candidates2016 %>% filter(centralnominated == 1 & (margin > -12.0 & margin < 5.75)) %>% group_by(defeat) %>% summarise(n = n())


## Code close defeat and close win at individual levels based on smallest windows

# remove Hanoi and Ho Chi Minh city
candidates2016rdd <- candidates2016 %>% filter(prov!="Ha Noi" & prov!="TP HCM")

lower.final <- -11.5
upper.final <- 7.25

candidates2016rdd$closedefeat <- as.numeric(candidates2016rdd$centralnominated==1 &
                                              candidates2016rdd$margin >= lower.final &
                                              candidates2016rdd$defeat == 1)
candidates2016rdd$closewin <- as.numeric(candidates2016rdd$centralnominated==1 &
                                           candidates2016rdd$margin <= upper.final &
                                           candidates2016rdd$defeat != 1)

# function to generate province summaries from candidate-level data

treatment_generate <- function(candidates, n.year = 13) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(#defeat = max(defeat, na.rm=T),
      closedefeat = max(closedefeat, na.rm = T),
      closewin = max(closewin, na.rm=T),
      #closewin.true = max(closewin.true, na.rm = T), # 2016 only
      #num.defeat = sum(defeat, na.rm=T),
      num.closedefeat = sum(closedefeat, na.rm=T),
      num.closewin = sum(closewin, na.rm=T),
      #num.closewin = sum(closewin, na.rm=T),
      #num.closewin.true = sum(closewin.true, na.rm=T), # 2016 only
      year = 2016)
  
  # province-level vector
  provinces_treatment <- provinces$closedefeat
  provinces_treatment[provinces$closedefeat == 0 & provinces$closewin == 0] <- NA
  
  # province-year vector
  provinces_year_treatment <- rep(provinces_treatment, each = n.year)
  
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
  
  treatment_rand <- treatment_generate(candidates2016rdd_rand)
})

## Observed treatment effects

treatment.2016.observed <- treatment_generate(candidates2016rdd)

dat_rdd <- plan %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
  mutate(defeat.2016 = treatment.2016.observed)

# one year effect
dat_2016rdd_1 <- dat_rdd %>% 
  filter(year < 2018 & year > 2012) %>%
  mutate(defeat = defeat.2016*as.numeric(year==2017)) %>%
  drop_na(defeat, net.trans.log)

# difference in means
lm_2016rdd_1 <- lm(net.trans.log ~ defeat + defeat.2016 +
                     factor(prov) + factor(year),
                   data = dat_2016rdd_1) # just lm
summary(lm_2016rdd_1)
lm_2016rdd_1_beta <- coef(lm_2016rdd_1)["defeat"]

# difference in means is different from standard coefficient from lm()
# if we don't partial out the treatment variable
lm_2016rdd_1_purgey <- lm(net.trans.log ~ defeat.2016 +
                            factor(prov) + factor(year),
                          data = dat_2016rdd_1) # just lm
summary(lm(resid(lm_2016rdd_1_purgey) ~ dat_2016rdd_1$defeat))

# wilcoxian rank sum
lm_2016rdd_1_wilcox <- as.numeric(rank(-resid(lm(net.trans.log ~ defeat + defeat.2016 +
                                                   factor(prov) + factor(year),
                                                 data = dat_2016rdd_1))) %*% dat_2016rdd_1$defeat)

# multiple year effect
dat_2016rdd_p <- dat_rdd %>% 
  mutate(defeat = defeat.2016*as.numeric(year >= 2017)) %>%
  filter(year > 2012) %>%
  drop_na(defeat, net.trans.log)

# difference in means
lm_2016rdd_p <- lm(net.trans.log ~ defeat + defeat.2016 +
                     factor(prov) + factor(year),
                   data = dat_2016rdd_p) # just lm
summary(lm_2016rdd_p)
lm_2016rdd_p_beta <- coef(lm_2016rdd_p)["defeat"]

lm_2016rdd_p_purgey <- lm(net.trans.log ~ defeat.2016 +
                            factor(prov) + factor(year),
                          data = dat_2016rdd_p) # just lm
summary(lm(resid(lm_2016rdd_p_purgey) ~ dat_2016rdd_p$defeat))

# wilcoxian rank sum
lm_2016rdd_p_wilcox <- as.numeric(rank(-resid(lm(net.trans.log ~ defeat + defeat.2016 +
                                                   factor(prov) + factor(year),
                                                 data = dat_2016rdd_p))) %*% dat_2016rdd_p$defeat)

## Apply a regression to each of the randomized treatment vector
# One year effect
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
    filter(year < 2018 & year > 2012) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.log ~ defeat.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
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

# multiple year effect
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
    filter(year > 2012) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.log ~ defeat.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
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

#### PLACEBO CHECK ####

## Observed treatment effects

# one year effect
dat_2016rdd_1_placebo <- dat_rdd %>% 
  mutate(defeat = defeat.2016*as.numeric(year==2014)) %>%
  filter(year > 2012 & year < 2015) %>%
  drop_na(defeat, net.trans.log)

# difference in means
lm_2016rdd_1_placebo <- lm(net.trans.log ~ defeat + defeat.2016 +
                             factor(prov) + factor(year),
                           data = dat_2016rdd_1_placebo) # just lm
summary(lm_2016rdd_1_placebo)
lm_2016rdd_1_placebo_beta <- coef(lm_2016rdd_1_placebo)["defeat"]

# wilcoxian rank sum
lm_2016rdd_1_placebo_wilcox <- as.numeric(rank(-resid(lm(net.trans.log ~ defeat + defeat.2016 +
                                                           factor(prov) + factor(year),
                                                         data = dat_2016rdd_1_placebo))) %*% dat_2016rdd_1_placebo$defeat)

# multiple year effect
dat_2016rdd_p_placebo <- dat_rdd %>% 
  mutate(defeat = defeat.2016*as.numeric(year>=2014)) %>%
  filter(year > 2012 & year < 2016) %>%
  drop_na(defeat, net.trans.log)

# difference in means
lm_2016rdd_p_placebo <- lm(net.trans.log ~ defeat + defeat.2016 +
                             factor(prov) + factor(year),
                           data = dat_2016rdd_p_placebo) # just lm
summary(lm_2016rdd_p_placebo)
lm_2016rdd_p_placebo_beta <- coef(lm_2016rdd_p_placebo)["defeat"]

lm_2016rdd_p_placebo_purgey <- lm(net.trans.log ~ defeat.2016 +
                                    factor(prov) + factor(year),
                                  data = dat_2016rdd_p_placebo) # just lm
summary(lm(resid(lm_2016rdd_p_placebo_purgey) ~ dat_2016rdd_p_placebo$defeat))

# wilcoxian rank sum
lm_2016rdd_p_placebo_wilcox <- as.numeric(rank(-resid(lm(net.trans.log ~ defeat + defeat.2016 +
                                                           factor(prov) + factor(year),
                                                         data = dat_2016rdd_p_placebo))) %*% dat_2016rdd_p_placebo$defeat)

## Apply a regression to each of the randomized treatment vector
# One year effect
rdd_2016_1_placebo <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
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
    filter(year > 2012 & year < 2015) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.log ~ defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_1_placebo$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_1_placebo$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_1_placebo$beta[1] > rdd_2016_1_placebo$beta[-1])
mean(rdd_2016_1_placebo$beta[1] < rdd_2016_1_placebo$beta[-1])

2*min(mean(rdd_2016_1_placebo$beta[1] > rdd_2016_1_placebo$beta[-1]),
      mean(rdd_2016_1_placebo$beta[1] < rdd_2016_1_placebo$beta[-1]))

plot(density(rdd_2016_1_placebo$beta))
abline(v = rdd_2016_1_placebo$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_1_placebo$wilcox[1] > rdd_2016_1_placebo$wilcox[-1])
mean(rdd_2016_1_placebo$wilcox[1] < rdd_2016_1_placebo$wilcox[-1])

2*min(mean(rdd_2016_1_placebo$wilcox[1] > rdd_2016_1_placebo$wilcox[-1]),
      mean(rdd_2016_1_placebo$wilcox[1] < rdd_2016_1_placebo$wilcox[-1]))

plot(density(rdd_2016_1_placebo$wilcox))
abline(v = rdd_2016_1_placebo$wilcox[1])

# multiple year effect -- somehow results are still significant even though they should not be...
rdd_2016_p_placebo <- list(beta = rep(NA, ncol(treatment.2016.randomized) + 1),
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
    filter(year > 2012 & year < 2016) %>%
    drop_na(defeat, net.trans.log)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.log ~ defeat.2016 +
                  net.trans.lag + 
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2016 +
                  net.trans.lag + 
                  num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 + num.candidates.2016 +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2016_p_placebo$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_p_placebo$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_p_placebo$beta[1] > rdd_2016_p_placebo$beta[-1])
mean(rdd_2016_p_placebo$beta[1] < rdd_2016_p_placebo$beta[-1])

2*min(mean(rdd_2016_p_placebo$beta[1] > rdd_2016_p_placebo$beta[-1]),
      mean(rdd_2016_p_placebo$beta[1] < rdd_2016_p_placebo$beta[-1]))

plot(density(rdd_2016_p_placebo$beta))
abline(v = rdd_2016_p_placebo$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_p_placebo$wilcox[1] > rdd_2016_p_placebo$wilcox[-1])
mean(rdd_2016_p_placebo$wilcox[1] < rdd_2016_p_placebo$wilcox[-1])

2*min(mean(rdd_2016_p_placebo$wilcox[1] > rdd_2016_p_placebo$wilcox[-1]),
      mean(rdd_2016_p_placebo$wilcox[1] < rdd_2016_p_placebo$wilcox[-1]))

plot(density(rdd_2016_p_placebo$wilcox))
abline(v = rdd_2016_p_placebo$wilcox[1])


#### LEAVE BELOW FOR LATER ####

#### 2016 results only ####

#### RDD result ####

## One year change, cross-sectional only ##

# 2016
threshold <- 10
dat_2016rdd_c <- plan %>%
  filter(year==2017)
#filter(abs(margin.2016) < threshold) %>%
#filter(prov!="Ha Noi" & prov!="TP HCM")
perm_2016_c <- ri::genperms(dat_2016rdd_c$defeat)

ggplot(dat_2016rdd_c, aes(x = margin.2016, y = net.trans.log)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed")

ggplot(dat_2016rdd_c, aes(x = margin.2016, y = net.trans.change.log)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed")

ggplot(candidates2016 %>% filter(centralnominated == 1), aes(x = margin, y = result)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed")

rdplot(dat_2016rdd_c$net.trans.change.log, dat_2016rdd_c$margin.2016)

rdbwselect(dat_2016rdd_c$net.trans.log, dat_2016rdd_c$margin.2016, covs = dat_2016rdd_c$net.trans.lag)

rdrobust(dat_2016rdd_c$net.trans, dat_2016rdd_c$margin.2016)

lm_2016rdd_c <- lm(neglog(net.trans) ~ defeat.2016 + margin.2016 + I(margin.2016^2) + neglog(net.trans.lag),
                   data = dat_2016rdd_c) # just lm
lm_2016rdd_c <- lm(neglog(net.trans) ~ defeat.2016 + margin.2016 + neglog(net.trans.lag),
                   data = dat_2016rdd_c) # just lm
lm_2016rdd_c <- lm(neglog(net.trans) ~ defeat.2016*margin.2016 + neglog(net.trans.lag),
                   data = dat_2016rdd_c) # just lm
lm_2016rdd_c <- lm(neglog(net.trans) ~ defeat.2016*(margin.2016 + I(margin.2016^2) + I(margin.2016^3))+ neglog(net.trans.lag),
                   data = dat_2016rdd_c) # just lm

lm_2016rdd_c <- lm(net.trans.log ~ defeat.2016*net.recipient*(margin.2016 + I(margin.2016^2))+ net.trans.log.lag,
                   data = dat_2016rdd_c) # just lm

lm_2016rdd_c <- lm(net.trans.change.log ~ defeat.2016*net.recipient*(margin.2016 + I(margin.2016^2))+ net.trans.log.lag,
                   data = dat_2016rdd_c) # just lm

lm_2016rdd_c <- lm(net.trans.log ~ defeat.2016*(margin.2016 + I(margin.2016^2))+ net.trans.log.lag,
                   data = dat_2016rdd_c %>% filter(net.recipient == 1)) # just lm
lm_2016rdd_c <- lm(net.trans.log ~ defeat.2016*(margin.2016 + I(margin.2016^2))+ net.trans.log.lag,
                   data = dat_2016rdd_c %>% filter(net.recipient == 0)) # just lm

lfe_2016_c <- rireg(data = dat_2016_c,
                    outcome = "net.trans.change.log",
                    treatment = "defeat.2016",
                    covs = c("net.trans.lag", "num.districts", "num.districts.5", "num.candidates", "num.centralnominees"),
                    perm = perm_2016_c) # lm under RI

summary(lm_2016_c)
lfe_2016_c