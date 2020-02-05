#### THIS FILE CONTAINS ALL THE MODELS THAT ARE DONE IN RESPONSE TO FIRST ROUND OF REVIEW ####

library(gsynth)
library(panelView)
library(multiwayvcov)
library(lmtest)
library(ri)
library(grid)
library(gridExtra)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Minh Trinh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("~/vietnam_elections/Data/Working Data")

source("../../Code/Winter 2019/Merge_All.R")

##########################
# LFE RESULTS FOR 2011
##########################

#### 2011 Results

dat_lme <- plan %>%
  filter(year > 2007 & year < 2019) %>%
  mutate(defeat = ifelse(year == 2008, 0, defeat)) %>% 
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Long An") %>%
  drop_na(net.trans.log, net.trans.lag)

## one-year change

# 2011 only
dat_2011_1 <- dat_lme %>%
  filter(year < 2013) 

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
  mutate(defeat = defeat.2011*(year >= 2012)) %>%
  filter(year < 2016)

# without covariates
lm_2011_pa <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   factor(prov) + factor(year),
                 data = dat_2011_p) # just lm
summary(lm_2011_pa)
vcov_2011_pa <- cluster.vcov(lm_2011_pa, cluster = ~ prov)
coeftest(lm_2011_pa, vcov = vcov_2011_pa)

# adding time-variant covariate: lagged total revenue
lm_2011_pb <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2011_p) # just lm
summary(lm_2011_pb)
vcov_2011_pb <- cluster.vcov(lm_2011_pb, cluster = ~ prov)
coeftest(lm_2011_pb, vcov = vcov_2011_pb)

# adding time-invariant covariates instead of prov FE
lm_2011_pc <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                   num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                   factor(year),
                 data = dat_2011_p) # just lm
summary(lm_2011_pc)
vcov_2011_pc <- cluster.vcov(lm_2011_pc, cluster = ~ prov)
coeftest(lm_2011_pc, vcov = vcov_2011_pc)

## placebo 1: one-year change as if election was held in 2008 st outcome is in 2009

# 2011 results under placebo 1
dat_2011_1_placebo2009 <- dat_lme %>%
  mutate(defeat_placebo2009 = defeat.2011 * as.numeric(year == 2009)) %>%
  filter(year < 2010)

# without covariates
lm_2011_1_placebo2009a <- lm(net.trans.change.log ~ defeat_placebo2009 + defeat.2011 +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2009) # just lm
summary(lm_2011_1_placebo2009a)
vcov_2011_1_placebo2009a <- cluster.vcov(lm_2011_1_placebo2009a, cluster = ~ prov)
coeftest(lm_2011_1_placebo2009a, vcov = vcov_2011_1_placebo2009a)

# adding time-variant covariate: lagged total revenue
lm_2011_1_placebo2009b <- lm(net.trans.change.log ~ defeat_placebo2009 + defeat.2011 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2009) # just lm
summary(lm_2011_1_placebo2009b)
vcov_2011_1_placebo2009b <- cluster.vcov(lm_2011_1_placebo2009b, cluster = ~ prov)
coeftest(lm_2011_1_placebo2009b, vcov = vcov_2011_1_placebo2009b)

# adding time-invariant covariates instead of prov FEs
lm_2011_1_placebo2009c <- lm(net.trans.change.log ~ defeat_placebo2009 + defeat.2011 +
                               num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                               factor(year),
                             data = dat_2011_1_placebo2009) # just lm
summary(lm_2011_1_placebo2009c)
vcov_2011_1_placebo2009c <- cluster.vcov(lm_2011_1_placebo2009c, cluster = ~ prov)
coeftest(lm_2011_1_placebo2009c, vcov = vcov_2011_1_placebo2009c)

## placebo 2: one-year change as if election was held in 2009 st outcome is in 2010

# 2011 results under placebo 1
dat_2011_1_placebo2010 <- dat_lme %>%
  mutate(defeat_placebo2010 = defeat.2011 * as.numeric(year == 2010)) %>%
  filter(year < 2011)

# without covariates
lm_2011_1_placebo2010a <- lm(net.trans.change.log ~ defeat_placebo2010 + defeat.2011 +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2010) # just lm
summary(lm_2011_1_placebo2010a)
vcov_2011_1_placebo2010a <- cluster.vcov(lm_2011_1_placebo2010a, cluster = ~ prov)
coeftest(lm_2011_1_placebo2010a, vcov = vcov_2011_1_placebo2010a)

# adding time-variant covariate: lagged total revenue
lm_2011_1_placebo2010b <- lm(net.trans.change.log ~ defeat_placebo2010 + defeat.2011 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2010) # just lm
summary(lm_2011_1_placebo2010b)
vcov_2011_1_placebo2010b <- cluster.vcov(lm_2011_1_placebo2010b, cluster = ~ prov)
coeftest(lm_2011_1_placebo2010b, vcov = vcov_2011_1_placebo2010b)

# adding time-invariant covariates instead of prov FEs
lm_2011_1_placebo2010c <- lm(net.trans.change.log ~ defeat_placebo2010 + defeat.2011 +
                               num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                               factor(year),
                             data = dat_2011_1_placebo2010) # just lm
summary(lm_2011_1_placebo2010c)
vcov_2011_1_placebo2010c <- cluster.vcov(lm_2011_1_placebo2010c, cluster = ~ prov)
coeftest(lm_2011_1_placebo2010c, vcov = vcov_2011_1_placebo2010c)

## placebo 3: one-year change as if election was held in 2010 st outcome is in 2011

# 2011 results under placebo 2
dat_2011_1_placebo2011 <- dat_lme %>%
  mutate(defeat_placebo2011 = defeat.2011 * as.numeric(year == 2011)) %>%
  filter(year < 2012)

# without covariates
lm_2011_1_placebo2011a <- lm(net.trans.change.log ~ defeat_placebo2011 + defeat.2011 +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2011) # just lm
summary(lm_2011_1_placebo2011a)
vcov_2011_1_placebo2011a <- cluster.vcov(lm_2011_1_placebo2011a, cluster = ~ prov)
coeftest(lm_2011_1_placebo2011a, vcov = vcov_2011_1_placebo2011a)

# adding time-variant covariate: lagged total revenue
lm_2011_1_placebo2011b <- lm(net.trans.change.log ~ defeat_placebo2011 + defeat.2011 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2011_1_placebo2011) # just lm
summary(lm_2011_1_placebo2011b)
vcov_2011_1_placebo2011b <- cluster.vcov(lm_2011_1_placebo2011b, cluster = ~ prov)
coeftest(lm_2011_1_placebo2011b, vcov = vcov_2011_1_placebo2011b)

# adding time-invariant covariates instead of prov FEs
lm_2011_1_placebo2011c <- lm(net.trans.change.log ~ defeat_placebo2011 + defeat.2011 +
                               num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                               factor(year),
                             data = dat_2011_1_placebo2011) # just lm
summary(lm_2011_1_placebo2011c)
vcov_2011_1_placebo2011c <- cluster.vcov(lm_2011_1_placebo2011c, cluster = ~ prov)
coeftest(lm_2011_1_placebo2011c, vcov = vcov_2011_1_placebo2011c)

# multiple year effect not applicable since post-treatment years would still be included
# if placebo treatment is in 2011

#### Coefficient plot for main regression results + placebo results
lfe_2011_plot_dat <- data.frame(ATT = sapply(list(lm_2011_1a, lm_2011_1b, lm_2011_1c,
                                                  lm_2011_1_placebo2009a, lm_2011_1_placebo2009b, lm_2011_1_placebo2009c,
                                                  lm_2011_1_placebo2010a, lm_2011_1_placebo2010b, lm_2011_1_placebo2010c,
                                                  lm_2011_1_placebo2011a, lm_2011_1_placebo2011b, lm_2011_1_placebo2011c), 
                                             function(mod){
                                               coef(mod)[2]
                                             }),
                                se = sapply(list(vcov_2011_1a, vcov_2011_1b, vcov_2011_1c,
                                                 vcov_2011_1_placebo2009a, vcov_2011_1_placebo2009b, vcov_2011_1_placebo2009c,
                                                 vcov_2011_1_placebo2010a, vcov_2011_1_placebo2010b, vcov_2011_1_placebo2010c,
                                                 vcov_2011_1_placebo2011a, vcov_2011_1_placebo2011b, vcov_2011_1_placebo2011c), 
                                            function(vcov){
                                              sqrt(diag(vcov))[2]
                                            })) %>%
  mutate(lower = ATT - se*1.96, upper = ATT + se*1.96) %>%
  mutate(spec = rep(c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs"), 4),
         treat_year = rep(c(2011, 2008, 2009, 2010), each = 3),
         placebo = c(rep("Estimated Effect", 3), rep("Estimated Effect, Placebo Treatments", 9))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2011", "2008", "2009", "2010")),
         spec = factor(spec, levels = c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs")))

ggplot(lfe_2011_plot_dat, aes(x = as.factor(treat_year), y = ATT, ymin = lower, ymax = upper, shape = spec)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect") +
  scale_shape_discrete(name="") +
  scale_x_discrete(labels=c("2011" = "Election in 2011\n\nActual Treatment", 
                            "2008" = "Election in 2008",
                            "2009" = "Election in 2009\n\nPlacebo Treatments",
                            "2010" = "Election in 2010")) +
  facet_grid( ~ placebo, scales="free_x", space="free_x") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom") 
ggsave("../../figure/200202_lfe_placebo_2011.png", width = 8, height = 4)

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

# set minimum threshold to be 57.5 s.t. there are 8 treated and 14 control
# (Cattaneo et al 2013 recommends 10 on both sides but we can start from slightly lower)
candidates2011 %>% 
  filter(centralnominated == 1 & 
           (percentage < 57.5 | result == 0)) %>% 
  group_by(defeat) %>% summarise(n = n())

windows <- seq(57.5, 65, by = .25)

set.seed(02142)
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

# Create a heatmap
image(1:length(m), 1, as.matrix(m), col = terrain.colors(60), axes = FALSE, xlab = "upper", ylab = "lower")
axis(1, 1:length(m), names(m))
for (x in 1:length(m)) {
  text(x, 1, as.numeric(m[x] < .15))
}


# Candidates thresholds:
# Smallest: 59.5
# Most stable: 61.5 
# Largest possible: 62.5 << most observations, most power

# Sample size for each threshold:
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 59.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 61.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2011 %>% filter(centralnominated == 1 & (percentage <= 62.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())

# remove Hanoi and Ho Chi Minh city
candidates2011rdd <- candidates2011 %>% 
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Long An")

window.final <- 61.5

candidates2011rdd$closewin <- as.numeric(candidates2011rdd$centralnominated==1 &
                                           candidates2011rdd$percentage <= window.final &
                                           candidates2011rdd$defeat != 1)

# function to generate province summaries from candidate-level data
treatment_generate_2011 <- function(candidates, years = c(2008:2018)) {
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
  provinces_year_treatment <- rep(provinces_treatment, each = length(years))
  
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
  filter(year > 2007 & year < 2019) %>%
  mutate(defeat = ifelse(year == 2008, 0, defeat)) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Long An") %>%
  drop_na(net.trans.log, net.trans.lag)

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
    filter(year < 2013) %>%
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
    filter(year < 2016) %>%
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
rdd_2011_1_placebo2009 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year==2009)) %>%
    filter(year < 2010) %>%
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
  rdd_2011_1_placebo2009$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_1_placebo2009$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2011_1_placebo2009$beta[1] > rdd_2011_1_placebo2009$beta[-1]),
      mean(rdd_2011_1_placebo2009$beta[1] < rdd_2011_1_placebo2009$beta[-1]))

plot(density(rdd_2011_1_placebo2009$beta))
abline(v = rdd_2011_1_placebo2009$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2011_1_placebo2009$wilcox[1] > rdd_2011_1_placebo2009$wilcox[-1]),
      mean(rdd_2011_1_placebo2009$wilcox[1] < rdd_2011_1_placebo2009$wilcox[-1]))

plot(density(rdd_2011_1_placebo2009$wilcox))
abline(v = rdd_2011_1_placebo2009$wilcox[1])


# multiple year effect
rdd_2011_p_placebo2009 <- list(beta = rep(NA, ncol(treatment.2011.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2011.randomized) +1))
for (i in 0:ncol(treatment.2011.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2011.observed  
  } else {
    treatment <- treatment.2011.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2011 = treatment) %>% 
    mutate(defeat = defeat.2011*as.numeric(year>=2009)) %>%
    filter(year < 2016)  %>%
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
  rdd_2011_p_placebo2009$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2011_p_placebo2009$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2011_p_placebo2009$beta[1] > rdd_2011_p_placebo2009$beta[-1]),
      mean(rdd_2011_p_placebo2009$beta[1] < rdd_2011_p_placebo2009$beta[-1]))

plot(density(rdd_2011_p_placebo2009$beta))
abline(v = rdd_2011_p_placebo2009$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2011_p_placebo2009$wilcox[1] > rdd_2011_p_placebo2009$wilcox[-1]),
      mean(rdd_2011_p_placebo2009$wilcox[1] < rdd_2011_p_placebo2009$wilcox[-1]))

plot(density(rdd_2011_p_placebo2009$wilcox))
abline(v = rdd_2011_p_placebo2009$wilcox[1])

## placebo 2: one-year change as if election was held in 2009 st outcome is in 2010

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
    filter(year < 2011) %>%
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
    filter(year < 2016) %>%
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

## placebo 3: one-year change as if election was held in 2010 st outcome is in 2011

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
    filter(year > 2007 & year < 2012) %>%
    filter(prov!="Long An") %>%
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

#### Randomization inference results for RDD analyses

lay <- rbind(c(NA,1,2,2,2,NA),
             c(3:8),
             c(NA,9,10,11,12,NA))

rdd_2011_results <- grid.arrange(layout_matrix = lay,
                            heights = c(1,10,2),
                            widths = c(1,rep(3,4),1),
                            textGrob("Actual Treatment"),
                            textGrob("Placebo Treatments"),
                            y_axe(-20,20),
                            ri_plot(rdd_2011_1, scale=F, xmin=-20, xmax=20, title = "Election in 2011"),
                            ri_plot(rdd_2011_1_placebo2009, scale=F, xmin=-20, xmax=20, title = "Election in 2008"),
                            ri_plot(rdd_2011_1_placebo2010, scale=F, xmin=-20, xmax=20, title = "Election in 2009"),
                            ri_plot(rdd_2011_1_placebo2011, scale=F, xmin=-20, xmax=20, title = "Election in 2010"),
                            y_axe(-20,20) + scale_x_continuous(position = "top"),
                            ri_annotate(rdd_2011_1, show_wilcox = FALSE),
                            ri_annotate(rdd_2011_1_placebo2009, show_wilcox = FALSE),
                            ri_annotate(rdd_2011_1_placebo2010, show_wilcox = FALSE),
                            ri_annotate(rdd_2011_1_placebo2011, show_wilcox = FALSE))

ggsave("../../figure/200202_rdd_results_2011.png", plot = rdd_results, width = 8, height = 4)

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
  filter(prov!="Long An") %>%
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
  filter(prov!="Long An") %>%
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

t(balance_lme)
t(balance_rdd)

#############################
# SYNTHETIC CONTROL FOR 2011
#############################

dat_synth <- plan %>%
  filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0 ) %>%
  filter(year < 2016) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Long An")
panelView(net.trans.change.log ~ defeat, data = dat_synth, index = c("prov", "year"))

## One year change

dat_2011synth_1 <- dat_synth %>%
  mutate(treat = defeat.2011*as.numeric(year==2012)) %>%
  #filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(year < 2013) %>%
  drop_na(net.trans.log)
panelView(net.trans.change.log ~ treat, data = dat_2011synth_1, index = c("prov", "year"))

system.time(
  synth_2011_1 <- gsynth(net.trans.change.log ~ treat + defeat + total.rev.log, 
                         data = dat_2011synth_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 3), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2011_1)
plot(synth_2011_1)
plot(synth_2011_1, type = "counterfactual")

## Persistent change

dat_2011synth_p <- dat_synth %>%
  mutate(treat = defeat.2011*as.numeric(year>=2012)) %>%
  #filter(prov != "Hau Giang") %>%
  drop_na(net.trans.log)
panelView(net.trans.change.log ~ treat, data = dat_2011synth_p, index = c("prov", "year"))

system.time(
  synth_2011_p <- gsynth(net.trans.change.log ~ treat + defeat, 
                         data = dat_2011synth_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 3), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         estimator = "ife",
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2011_p)
plot(synth_2011_p)
plot(synth_2011_p, type = "counterfactual", raw = "all")

lay <- cbind(c(rep(1,2), NA),
             c(2:4))

synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(2.5,.5,.5),
                                    widths= c(0.5,8),
                                    grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
                                    #grid.text("Actual Treatment", draw = FALSE),
                                    gsynth_plot(synth_2011_p, xmin = -6, xmax = 4, ymin = -10, ymax = 30),
                                    # slightly hacky solution to align the axis of x_axe with the rest
                                    # of the graph
                                    x_axe(-6,4) + 
                                      scale_x_continuous(labels = function(x) {x + 2011},
                                                         breaks = c(-6:4)) +
                                      # set breaks = -10 s.t the axis text takes up exactly as much space
                                      # as that of the real graphs
                                      scale_y_continuous(breaks = -10),
                                    grid.text("Year", draw = FALSE))
grid.newpage()

png("../../figure/200202_synth_results_2011.png", width = 8, height = 3.5, units="in", res = 96)
synth_results <- grid.draw(synth_results_table)
dev.off()

## Placebo 1: 2010 treatment, as if election is in 2009

# One year change

dat_2011synth_1_placebo2010 <- dat_synth %>%
  mutate(treat = defeat.2011*as.numeric(year==2010)) %>%
  #filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(year < 2011) %>%
  drop_na(net.trans.log)

system.time(
  synth_2011_1_placebo2010 <- gsynth(net.trans.change.log ~ treat + defeat + total.rev.log, 
                                     data = dat_2011synth_1_placebo2010, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 3), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2011_1_placebo2010)
plot(synth_2011_1_placebo2010)

# Persistent change

dat_2011synth_p_placebo2010 <- dat_synth %>%
  mutate(treat = defeat.2011*as.numeric(year>=2010)) %>%
  #filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(year < 2012) %>%
  drop_na(net.trans.log)

system.time(
  synth_2011_p_placebo2010 <- gsynth(net.trans.change.log ~ treat + defeat + total.rev.log.lag, 
                                     data = dat_2011synth_p_placebo2010, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 3), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2011_p_placebo2010)
plot(synth_2011_p_placebo2010)

## Placebo 2: 2011 treatment, as if election is in 2010

# One year change

dat_2011synth_1_placebo2011 <- dat_synth %>%
  mutate(treat = defeat.2011*as.numeric(year==2011)) %>%
  #filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(year < 2012) %>%
  drop_na(net.trans.log)

system.time(
  synth_2011_1_placebo2011 <- gsynth(net.trans.change.log ~ treat + defeat + total.rev.log.lag, 
                                     data = dat_2011synth_1_placebo2011, 
                                     index = c("prov", "year"), force = "two-way",
                                     EM = TRUE,
                                     CV = TRUE, r = c(0, 3), 
                                     se = TRUE, 
                                     inference = "parametric", nboots = 1000,
                                     parallel = TRUE, cores = 6,
                                     na.rm = TRUE)
)

print(synth_2011_1_placebo2011)
plot(synth_2011_1_placebo2011)

# multiple year effect not applicable since post-treatment years would still be included

lay <- cbind(c(rep(1,2), NA),
             c(2:4))

synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(2.5,.5,.5),
                                    widths= c(0.5,8),
                                    grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
                                    #grid.text("Actual Treatment", draw = FALSE),
                                    gsynth_plot(synth_2011_p, xmin = -6, xmax = 4, ymin = -10, ymax = 30),
                                    # slightly hacky solution to align the axis of x_axe with the rest
                                    # of the graph
                                    x_axe(-6,4) + 
                                      scale_x_continuous(labels = function(x) {x + 2011},
                                                         breaks = c(-6:4)) +
                                      # set breaks = -10 s.t the axis text takes up exactly as much space
                                      # as that of the real graphs
                                      scale_y_continuous(breaks = -10),
                                    grid.text("Year", draw = FALSE))
grid.newpage()

grid.draw(synth_results_table)

##########################
# LFE RESULTS FOR 2007
##########################

#### 2007 Results

dat_lme <- plan %>%
  filter(year > 2003) %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  drop_na(net.trans.log, net.trans.lag)

## one-year change

# 2007 only
dat_2007_1 <- dat_lme %>%
  filter(year < 2009)

# without covariates
lm_2007_1a <- lm(net.trans.log ~ defeat + defeat.2007 +
                   factor(prov) + factor(year),
                 data = dat_2007_1) # just lm
summary(lm_2007_1a)
vcov_2007_1a <- cluster.vcov(lm_2007_1a, cluster = ~ prov)
coeftest(lm_2007_1a, vcov = vcov_2007_1a)

# adding time-variant covariate: lagged total revenue
lm_2007_1b <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2007_1) # just lm
summary(lm_2007_1b)
vcov_2007_1b <- cluster.vcov(lm_2007_1b, cluster = ~ prov)
coeftest(lm_2007_1b, vcov = vcov_2007_1b)

# adding time-invariant covariates instead of prov FE
lm_2007_1c <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                   num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                   factor(year),
                 data = dat_2007_1) # just lm
summary(lm_2007_1c)
vcov_2007_1c <- cluster.vcov(lm_2007_1c, cluster = ~ prov)
coeftest(lm_2007_1c, vcov = vcov_2007_1c)


## persistent change

# 2007 only
dat_2007_p <- dat_lme %>%
  mutate(defeat = defeat.2007*(year >= 2008)) %>%
  filter(year < 2011)

# without covariates
lm_2007_pa <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                   factor(prov) + factor(year),
                 data = dat_2007_p) # just lm
summary(lm_2007_pa)
vcov_2007_pa <- cluster.vcov(lm_2007_pa, cluster = ~ prov)
coeftest(lm_2007_pa, vcov = vcov_2007_pa)

# adding time-variant covariate: lagged total revenue
lm_2007_pb <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2007_p) # just lm
summary(lm_2007_pb)
vcov_2007_pb <- cluster.vcov(lm_2007_pb, cluster = ~ prov)
coeftest(lm_2007_pb, vcov = vcov_2007_pb)

# adding time-invariant covariates instead of prov FE
lm_2007_pc <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                   num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                   factor(year),
                 data = dat_2007_p) # just lm
summary(lm_2007_pc)
vcov_2007_pc <- cluster.vcov(lm_2007_pc, cluster = ~ prov)
coeftest(lm_2007_pc, vcov = vcov_2007_pc)

## placebo 1: one-year change as if election was held in 2005 st outcome is in 2006

# 2007 results under placebo 2
dat_2007_1_placebo2006 <- dat_lme %>%
  mutate(defeat_placebo2006 = defeat.2007 * as.numeric(year == 2006)) %>%
  filter(year < 2007)

# without covariates
lm_2007_1_placebo2006a <- lm(net.trans.change.log ~ defeat_placebo2006 + defeat.2007 +
                               factor(prov) + factor(year),
                             data = dat_2007_1_placebo2006) # just lm
summary(lm_2007_1_placebo2006a)
vcov_2007_1_placebo2006a <- cluster.vcov(lm_2007_1_placebo2006a, cluster = ~ prov)
coeftest(lm_2007_1_placebo2006a, vcov = vcov_2007_1_placebo2006a)

# adding time-variant covariate: lagged total revenue
lm_2007_1_placebo2006b <- lm(net.trans.change.log ~ defeat_placebo2006 + defeat.2007 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2007_1_placebo2006) # just lm
summary(lm_2007_1_placebo2006b)
vcov_2007_1_placebo2006b <- cluster.vcov(lm_2007_1_placebo2006b, cluster = ~ prov)
coeftest(lm_2007_1_placebo2006b, vcov = vcov_2007_1_placebo2006b)

# adding time-invariant covariates instead of prov FEs
lm_2007_1_placebo2006c <- lm(net.trans.change.log ~ defeat_placebo2006 + defeat.2007 +
                               num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                               factor(year),
                             data = dat_2007_1_placebo2006) # just lm
summary(lm_2007_1_placebo2006c)
vcov_2007_1_placebo2006c <- cluster.vcov(lm_2007_1_placebo2006c, cluster = ~ prov)
coeftest(lm_2007_1_placebo2006c, vcov = vcov_2007_1_placebo2006c)

## placebo 2: one-year change as if election was held in 2006 st outcome is in 2007

# 2007 results under placebo 2
dat_2007_1_placebo2007 <- dat_lme %>%
  mutate(defeat_placebo2007 = defeat.2007 * as.numeric(year == 2007)) %>%
  filter(year < 2008)

# without covariates
lm_2007_1_placebo2007a <- lm(net.trans.change.log ~ defeat_placebo2007 + defeat.2007 +
                               factor(prov) + factor(year),
                             data = dat_2007_1_placebo2007) # just lm
summary(lm_2007_1_placebo2007a)
vcov_2007_1_placebo2007a <- cluster.vcov(lm_2007_1_placebo2007a, cluster = ~ prov)
coeftest(lm_2007_1_placebo2007a, vcov = vcov_2007_1_placebo2007a)

# adding time-variant covariate: lagged total revenue
lm_2007_1_placebo2007b <- lm(net.trans.change.log ~ defeat_placebo2007 + defeat.2007 +
                               total.rev.log.lag +
                               factor(prov) + factor(year),
                             data = dat_2007_1_placebo2007) # just lm
summary(lm_2007_1_placebo2007b)
vcov_2007_1_placebo2007b <- cluster.vcov(lm_2007_1_placebo2007b, cluster = ~ prov)
coeftest(lm_2007_1_placebo2007b, vcov = vcov_2007_1_placebo2007b)

# adding time-invariant covariates instead of prov FEs
lm_2007_1_placebo2007c <- lm(net.trans.change.log ~ defeat_placebo2007 + defeat.2007 +
                               num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                               factor(year),
                             data = dat_2007_1_placebo2007) # just lm
summary(lm_2007_1_placebo2007c)
vcov_2007_1_placebo2007c <- cluster.vcov(lm_2007_1_placebo2007c, cluster = ~ prov)
coeftest(lm_2007_1_placebo2007c, vcov = vcov_2007_1_placebo2007c)

# multiple year effect not applicable since post-treatment years would still be included
# if placebo treatment is in 2007

#### Coefficient plot for main regression results + placebo results
lfe_2007_plot_dat <- data.frame(ATT = sapply(list(lm_2007_1a, lm_2007_1b, lm_2007_1c,
                                                  lm_2007_1_placebo2006a, lm_2007_1_placebo2006b, lm_2007_1_placebo2006c,
                                                  lm_2007_1_placebo2007a, lm_2007_1_placebo2007b, lm_2007_1_placebo2007c), 
                                             function(mod){
                                               coef(mod)[2]
                                             }),
                                se = sapply(list(vcov_2007_1a, vcov_2007_1b, vcov_2007_1c,
                                                 vcov_2007_1_placebo2006a, vcov_2007_1_placebo2006b, vcov_2007_1_placebo2006c,
                                                 vcov_2007_1_placebo2007a, vcov_2007_1_placebo2007b, vcov_2007_1_placebo2007c), 
                                            function(vcov){
                                              sqrt(diag(vcov))[2]
                                            })) %>%
  mutate(lower = ATT - se*1.96, upper = ATT + se*1.96) %>%
  mutate(spec = rep(c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs"), 3),
         treat_year = rep(c(2007, 2005, 2006), each = 3),
         placebo = c(rep("Estimated Effect", 3), rep("Estimated Effect, Placebo Treatments", 6))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2007", "2005", "2006")),
         spec = factor(spec, levels = c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs")))

ggplot(lfe_2007_plot_dat, aes(x = as.factor(treat_year), y = ATT, ymin = lower, ymax = upper, shape = spec)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect") +
  scale_shape_discrete(name="") +
  scale_x_discrete(labels=c("2007" = "Election in 2007\n\nActual Treatment", 
                            "2005" = "Election in 2005\n\nPlacebo Treatments",
                            "2006" = "Election in 2006")) +
  facet_grid( ~ placebo, scales="free_x", space="free_x") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom") 
ggsave("../../figure/200202_lfe_placebo_2007.png", width = 6, height = 4)

##########################
# RDD RESULTS FOR 2007
##########################

#### Bandwidth selection using candidate-level data ####

# Note: Only possible to select bandwidth on one side
# i.e. search for maximum winning vote share such that
# candidates who won with fewer than this vote share
# are no different from candidates who lost

## Pretreatment covariates for window selector
covs <- c("age", "male", "party", "years_party", "degree2", "power", "num.candidates", "num.seats")

candidates2007$years_party[is.na(candidates2007$years_party)] <- 0

# set minimum threshold to be 59 s.t. there are 9 treated and 12 control
# (Cattaneo et al 2013 recommends 10 on both sides but we can start from slightly lower)
candidates2007 %>% 
  filter(centralnominated == 1 & 
           (percentage < 59 | result == 0)) %>% 
  group_by(defeat) %>% summarise(n = n())

windows <- seq(59, 62.5, by = .25)

pvalues.windows <- sapply(windows, function(w) {
  index <- which(candidates2007$centralnominated == 1 &
                   (candidates2007$percentage < w | candidates2007$result == 0))
  perms <- ri::genperms(candidates2007$defeat[index])
  
  pvalues <- sapply(covs, function(c) {
    res <- omni.ate(as.numeric(candidates2007[[c]][index]),
                    candidates2007[["defeat"]][index],
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

# Create a heatmap
image(1:length(m), 1, as.matrix(m), col = terrain.colors(60), axes = FALSE, xlab = "upper", ylab = "lower")
axis(1, 1:length(m), names(m))
for (x in 1:length(m)) {
  text(x, 1, as.numeric(m[x] < .15))
}


# Candidates thresholds:
# Largest and consistent: 58.5 << most stable, but only 6 controls
# Largest possible: 59.5 << not so stable, but only case with more than 10 controls

# Sample size for each threshold:
candidates2007 %>% filter(centralnominated == 1 & (percentage <= 59.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2007 %>% filter(centralnominated == 1 & (percentage <= 58.5 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())
candidates2007 %>% filter(centralnominated == 1 & (percentage <= 57 | result == 0)) %>% group_by(defeat) %>% summarise(n = n())

# remove Hanoi and Ho Chi Minh city
candidates2007rdd <- candidates2007 %>% filter(prov!="Ha Noi" & prov!="TP HCM") %>% filter(prov!="Ha Tay")

window.final <- 59.5

candidates2007rdd$closewin <- as.numeric(candidates2007rdd$centralnominated==1 &
                                           candidates2007rdd$percentage <= window.final &
                                           candidates2007rdd$defeat != 1)

# function to generate province summaries from candidate-level data

treatment_generate_2007 <- function(candidates, dat_rdd) {
  provinces <- candidates %>%
    group_by(prov) %>%
    filter(prov != "Ha Tay") %>%
    summarise(defeat = max(defeat, na.rm = T),
              closewin = max(closewin, na.rm=T),
              num.closewin = sum(closewin, na.rm=T))
  
  # province-level vector
  provinces$treatment <- provinces$defeat
  provinces$treatment[provinces$defeat == 0 & provinces$closewin == 0] <- NA
  
  # merge with dat_rdd to get province-year vector
  # (simply replicate vector doesn't work because of unbalanced panel)
  provinces_year_dat <- inner_join(dat_rdd, provinces, by = "prov")
  
  provinces_year_treatment <- provinces_year_dat$treatment
  
  return(provinces_year_treatment)
}

## Randomization distribution of candidate-level treatment vector
index <- which(candidates2007rdd$defeat == 1 | candidates2007rdd$closewin == 1)

set.seed(02139)
nsim <- 1000
candidates2007.defeat.randomized <- replicate(nsim, rbinom(length(index), 1, .5))


## Create province-year-level treatment vectors
treatment.2007.randomized <- apply(candidates2007.defeat.randomized, 2, function(t) {
  candidates2007rdd_rand <- candidates2007rdd
  
  candidates2007rdd_rand$defeat <- 0
  candidates2007rdd_rand$defeat[index] <- t
  
  candidates2007rdd_rand$closewin <- 0
  candidates2007rdd_rand$closewin[index] <- 1-t
  
  treatment_rand <- treatment_generate_2007(candidates2007rdd_rand, 
                                            plan %>%
                                              filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                              filter(prov!= "Ha Tay") %>%
                                              filter(year > 2003 & year < 2019))
})

## Observed treatment effects

treatment.2007.observed <- treatment_generate_2007(candidates2007rdd, 
                                                   plan %>%
                                                     filter(prov!="Ha Noi" & prov!="TP HCM") %>%
                                                     filter(prov!= "Ha Tay") %>%
                                                     filter(year > 2003 & year < 2019))

dat_rdd <- plan %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!= "Ha Tay") %>%
  filter(year > 2003 & year < 2019) %>%
  mutate(defeat.2007 = treatment.2007.observed)

## One year effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2007_1 <- list(beta = rep(NA, ncol(treatment.2007.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2007.randomized) +1))
for (i in 0:ncol(treatment.2007.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2007.observed  
  } else {
    treatment <- treatment.2007.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2007 = treatment) %>% 
    mutate(defeat = defeat.2007*as.numeric(year==2008)) %>%
    filter(year < 2009) %>%
    drop_na(defeat, net.trans.log, total.rev.log.lag)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2007 + 
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2007_1$beta[i+1] <- ifelse(abs(coef(fit)["t.tilde"]) > 100,
                                 NA,
                                 coef(fit)["t.tilde"])
  
  # Wilcoxon ranksum
  rdd_2007_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2007_1$beta[1] > rdd_2007_1$beta[-1])
mean(rdd_2007_1$beta[1] < rdd_2007_1$beta[-1])

2*min(mean(rdd_2007_1$beta[1] > rdd_2007_1$beta[-1]),
      mean(rdd_2007_1$beta[1] < rdd_2007_1$beta[-1]))

plot(density(rdd_2007_1$beta))
abline(v = rdd_2007_1$beta[1])

# Wilcoxon rank sum
mean(rdd_2007_1$wilcox[1] > rdd_2007_1$wilcox[-1])
mean(rdd_2007_1$wilcox[1] < rdd_2007_1$wilcox[-1])

2*min(mean(rdd_2007_1$wilcox[1] > rdd_2007_1$wilcox[-1]),
      mean(rdd_2007_1$wilcox[1] < rdd_2007_1$wilcox[-1]))

plot(density(rdd_2007_1$wilcox))
abline(v = rdd_2007_1$wilcox[1])


## Multiple years effect
# should note that adding time-invariant covariates or lagged outcomes don't change results
rdd_2007_p <- list(beta = rep(NA, ncol(treatment.2007.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2007.randomized) +1))
for (i in 0:ncol(treatment.2007.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2007.observed  
  } else {
    treatment <- treatment.2007.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2007 = treatment) %>% 
    mutate(defeat = defeat.2007*as.numeric(year>=2008)) %>%
    filter(year < 2011) %>%
    drop_na(defeat, net.trans.log, total.rev.log.lag)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2007_p$beta[i+1] <- ifelse(abs(coef(fit)["t.tilde"]) > 100,
                                 NA,
                                 coef(fit)["t.tilde"])
  
  # Wilcoxon ranksum
  rdd_2007_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2007_p$beta[1] > rdd_2007_p$beta[-1])
mean(rdd_2007_p$beta[1] < rdd_2007_p$beta[-1])

2*min(mean(rdd_2007_p$beta[1] > rdd_2007_p$beta[-1]),
      mean(rdd_2007_p$beta[1] < rdd_2007_p$beta[-1]))

plot(density(rdd_2007_p$beta))
abline(v = rdd_2007_p$beta[1])

# Wilcoxon rank sum
mean(rdd_2007_p$wilcox[1] > rdd_2007_p$wilcox[-1])
mean(rdd_2007_p$wilcox[1] < rdd_2007_p$wilcox[-1])

2*min(mean(rdd_2007_p$wilcox[1] > rdd_2007_p$wilcox[-1]),
      mean(rdd_2007_p$wilcox[1] < rdd_2007_p$wilcox[-1]))

plot(density(rdd_2007_p$wilcox))
abline(v = rdd_2007_p$wilcox[1])


## placebo 1: one-year change as if election was held in 2005 st outcome is in 2006

# One year effect
rdd_2007_1_placebo2006 <- list(beta = rep(NA, ncol(treatment.2007.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2007.randomized) +1))
for (i in 0:ncol(treatment.2007.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2007.observed  
  } else {
    treatment <- treatment.2007.randomized[,i]
  }
  
  dat <- dat_rdd %>%
    mutate(defeat.2007 = treatment) %>% 
    mutate(defeat = defeat.2007*as.numeric(year==2006)) %>%
    filter(year > 2003 & year < 2007) %>%
    drop_na(defeat, net.trans.log, total.rev.log.lag)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2007_1_placebo2006$beta[i+1] <- ifelse(abs(coef(fit)["t.tilde"]) > 100,
                                             NA,
                                             coef(fit)["t.tilde"])
  
  # Wilcoxon ranksum
  rdd_2007_1_placebo2006$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2007_1_placebo2006$beta[1] > rdd_2007_1_placebo2006$beta[-1]),
      mean(rdd_2007_1_placebo2006$beta[1] < rdd_2007_1_placebo2006$beta[-1]))

plot(density(rdd_2007_1_placebo2006$beta))
abline(v = rdd_2007_1_placebo2006$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2007_1_placebo2006$wilcox[1] > rdd_2007_1_placebo2006$wilcox[-1]),
      mean(rdd_2007_1_placebo2006$wilcox[1] < rdd_2007_1_placebo2006$wilcox[-1]))

plot(density(rdd_2007_1_placebo2006$wilcox))
abline(v = rdd_2007_1_placebo2006$wilcox[1])


# multiple year effect
rdd_2007_p_placebo2006 <- list(beta = rep(NA, ncol(treatment.2007.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2007.randomized) +1))
for (i in 0:ncol(treatment.2007.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2007.observed  
  } else {
    treatment <- treatment.2007.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2007 = treatment) %>% 
    mutate(defeat = defeat.2007*as.numeric(year>=2006)) %>%
    filter(year > 2003 & year < 2011) %>%
    drop_na(defeat, net.trans.log, total.rev.log.lag)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2007_p_placebo2006$beta[i+1] <- ifelse(abs(coef(fit)["t.tilde"]) > 100,
                                             NA,
                                             coef(fit)["t.tilde"])
  
  # Wilcoxon ranksum
  rdd_2007_p_placebo2006$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2007_p_placebo2006$beta[1] > rdd_2007_p_placebo2006$beta[-1]),
      mean(rdd_2007_p_placebo2006$beta[1] < rdd_2007_p_placebo2006$beta[-1]))

plot(density(rdd_2007_p_placebo2006$beta))
abline(v = rdd_2007_p_placebo2006$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2007_p_placebo2010$wilcox[1] > rdd_2007_p_placebo2010$wilcox[-1]),
      mean(rdd_2007_p_placebo2010$wilcox[1] < rdd_2007_p_placebo2010$wilcox[-1]))

plot(density(rdd_2007_p_placebo2010$wilcox))
abline(v = rdd_2007_p_placebo2010$wilcox[1])

## placebo 2: one-year change as if election was held in 2006 st outcome is in 2007

# One year effect
rdd_2007_1_placebo2007 <- list(beta = rep(NA, ncol(treatment.2007.randomized) + 1),
                               wilcox = rep(NA, ncol(treatment.2007.randomized) +1))
for (i in 0:ncol(treatment.2007.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2007.observed  
  } else {
    treatment <- treatment.2007.randomized[,i]
  }
  
  
  dat <- dat_rdd %>%
    mutate(defeat.2007 = treatment) %>% 
    mutate(defeat = defeat.2007*as.numeric(year==2007)) %>%
    filter(year > 2003 & year < 2008) %>%
    drop_na(defeat, net.trans.log, total.rev.log.lag)
  
  # create residual by purging covariate-based noise
  purge.y <- lm(net.trans.change.log ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  
  dat$y.tilde <- resid(purge.y)
  
  # do the same for treatment variable
  purge.t <- lm(defeat ~ defeat.2007 +
                  total.rev.log.lag +
                  factor(prov) + factor(year),
                data = dat) # just lm
  dat$t.tilde <- resid(purge.t)
  
  # Difference-in-means statistics
  fit <- lm(y.tilde ~ t.tilde, data = dat) # just lm
  rdd_2007_1_placebo2007$beta[i+1] <- ifelse(abs(coef(fit)["t.tilde"]) > 100,
                                             NA,
                                             coef(fit)["t.tilde"])
  
  # Wilcoxon ranksum
  rdd_2007_1_placebo2007$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
2*min(mean(rdd_2007_1_placebo2007$beta[1] > rdd_2007_1_placebo2007$beta[-1]),
      mean(rdd_2007_1_placebo2007$beta[1] < rdd_2007_1_placebo2007$beta[-1]))

plot(density(rdd_2007_1_placebo2007$beta))
abline(v = rdd_2007_1_placebo2007$beta[1])

# Wilcoxon rank sum
2*min(mean(rdd_2007_1_placebo2007$wilcox[1] > rdd_2007_1_placebo2007$wilcox[-1]),
      mean(rdd_2007_1_placebo2007$wilcox[1] < rdd_2007_1_placebo2007$wilcox[-1]))

plot(density(rdd_2007_1_placebo2007$wilcox))
abline(v = rdd_2007_1_placebo2007$wilcox[1])

# multiple year effect not applicable since post-treatment years would still be included

#### Randomization inference results for RDD analyses

lay <- rbind(c(NA,1,2,2,NA),
             c(3:7),
             c(NA,8,9,10,NA))

rdd_2007_results <- grid.arrange(layout_matrix = lay,
                                 heights = c(1,10,2),
                                 widths = c(1,rep(3,3),1),
                                 textGrob("Actual Treatment"),
                                 textGrob("Placebo Treatments"),
                                 y_axe(-20,20),
                                 ri_plot(rdd_2007_1, scale=F, xmin=-20, xmax=20, title = "Election in 2007"),
                                 ri_plot(rdd_2007_1_placebo2006, scale=F, xmin=-20, xmax=20, title = "Election in 2009"),
                                 ri_plot(rdd_2007_1_placebo2007, scale=F, xmin=-20, xmax=20, title = "Election in 2010"),
                                 y_axe(-20,20) + scale_x_continuous(position = "top"),
                                 ri_annotate(rdd_2007_1, show_wilcox = FALSE),
                                 ri_annotate(rdd_2007_1_placebo2006, show_wilcox = FALSE),
                                 ri_annotate(rdd_2007_1_placebo2007, show_wilcox = FALSE))
ggsave("../../figure/200202_rdd_results_2007.png", plot = rdd_results, width = 8, height = 4)

##########################
# 2015 STATE BUDGET LAW
##########################

plan %>% 
  group_by(prov) %>%
  arrange(year) %>%
  mutate(ratio.lag = lag(ratio)) %>%
  filter(year == 2017) %>%
  filter(ratio.lag != ratio) %>%
  ungroup %>%
  select(prov, ratio.lag, ratio) %>%
  arrange(ratio.lag, ratio)

