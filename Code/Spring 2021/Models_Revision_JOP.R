#### THIS FILE CONTAINS ALL THE MODELS THAT ARE DONE IN RESPONSE TO FIRST ROUND OF REVIEW AT JOP ####

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("~/vietnam_elections/Data/Working Data")

source("../../Code/Spring 2021/Merge_All.R")


##########################################
# COMPARING CANDIDATES AROUND BOUNDARIES
##########################################

## Covariates for comparison
covariates <- c("age", "male", "party", "years_party", 
                "degree2", "power", 
                "num.candidates", "num.seats")

ggplot(data = candidates2016 %>% 
         filter(central_nominator_leaders != 1) %>%
         filter(centralnominated == 1),
       aes(x = margin, y = power)) +
  geom_smooth(method = "loess", formula = "y ~ x")

balance_candidates_full <- candidates2016 %>%
  filter(centralnominated == 1) %>%
  #filter(central_nominator_leaders != 1) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  mutate(margin_cut = cut(margin, 
                          breaks = c(-30, -10, 
                                     10, 30, 50))) %>%
  group_by(margin_cut) %>%
  summarise(n = n(),
            age_mean = mean(age),
            age_se = sd(age)/sqrt(n),
            male_mean = mean(male),
            male_se = sd(male)/sqrt(n),
            party_mean = mean(party),
            party_se = sd(party)/sqrt(n),
            years_party_mean = mean(years_party, na.rm = T),
            years_party_se = sd(years_party, na.rm = T)/sqrt(n),
            degree_mean = mean(degree2 %>% as.numeric),
            degree_se = sd(degree2 %>% as.numeric)/sqrt(n),
            power_mean = mean(power),
            power_se = sd(power)/sqrt(n))

balance_candidates_sample <- candidates2016 %>%
  filter(centralnominated == 1) %>%
  #filter(central_nominator_leaders != 1) %>%
  mutate(margin_cut = cut(margin, 
                          breaks = c(-30, -10, 
                                     10, 30, 50))) %>%
  group_by(margin_cut) %>%
  summarise(n = n(),
            age_mean = mean(age),
            age_se = sd(age)/sqrt(n),
            male_mean = mean(male),
            male_se = sd(male)/sqrt(n),
            party_mean = mean(party),
            party_se = sd(party)/sqrt(n),
            years_party_mean = mean(years_party, na.rm = T),
            years_party_se = sd(years_party, na.rm = T)/sqrt(n),
            degree_mean = mean(degree2 %>% as.numeric),
            degree_se = sd(degree2 %>% as.numeric)/sqrt(n),
            power_mean = mean(power),
            power_se = sd(power)/sqrt(n))

balance_districts <- districts2016 %>%
  filter(num.centralnominees >= 1) %>%
  #filter(central_nominator_leaders != 1) %>%
  #filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  #filter(prov!="Binh Duong") %>%
  mutate(margin_cut = cut(margin, 
                          breaks = c(-20, -10, 
                                     10, 20, 25, 30, 35, 40, 50))) %>%
  group_by(margin_cut) %>%
  summarise(n = n(),
            num.candidates_mean = mean(num.candidates == 5),
            num.candidates_se = sd(num.candidates == 5)/sqrt(n),
            num.seats_mean = mean(num.seats == 3),
            num.seats_se = sd(num.seats == 3)/sqrt(n))


#################################################
# SIMPLE CROSS-SECTIONAL RANDOMIZATION INFERENCE 
#################################################

# One year effect
treatment.simple.2016.observed <- plan %>% 
  filter(year == 2017) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  pull(defeat.2016)

treatment.simple.2016.randomized <- ri::genperms(treatment.simple.2016.observed)

outcome.simple.2016 <- plan %>% 
  filter(year == 2017) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  pull(net.trans)

ri_2016_cross_1 <- c((outcome.simple.2016 %*% treatment.simple.2016.observed),
                     (outcome.simple.2016 %*% treatment.simple.2016.randomized))/5

ri_2016_cross_1[1]

mean(ri_2016_cross_1[-1])
sd(ri_2016_cross_1[-1])

plot(density(ri_2016_cross_1[-1]))
abline(v = ri_2016_cross_1[1])

mean(ri_2016_cross_1[1] > ri_2016_cross_1[-1], na.rm = TRUE)
mean(ri_2016_cross_1[1] < ri_2016_cross_1[-1], na.rm = TRUE)

2*min(mean(ri_2016_cross_1[1] > ri_2016_cross_1[-1], na.rm = TRUE),
      mean(ri_2016_cross_1[1] < ri_2016_cross_1[-1], na.rm = TRUE), na.rm = TRUE)


##############################################
# LINEAR FIXED EFFECTS MODELS FOR FULL SAMPLE
##############################################

dat_lme_simple <- plan %>%
  filter(year < 2020 & year > 2012) %>%
  #filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  drop_na(net.trans.log, net.trans.lag)

## one-year change

# 2016 only -- positive and significant effect
dat_2016_simple_1 <- dat_lme_simple  %>%
  filter(year < 2018)

# without covariates
lm_2016_simple_1a <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   factor(prov) + factor(year),
                 data = dat_2016_simple_1) # just lm
summary(lm_2016_simple_1a)
vcov_2016_simple_1a <- cluster.vcov(lm_2016_simple_1a, cluster = ~ prov)
coeftest(lm_2016_simple_1a, vcov = vcov_2016_simple_1a)

# adding time-variant covariate: lagged total revenue
lm_2016_simple_1b <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2016_simple_1) # just lm
summary(lm_2016_simple_1b)
vcov_2016_simple_1b <- cluster.vcov(lm_2016_simple_1b, cluster = ~ prov)
coeftest(lm_2016_simple_1b, vcov = vcov_2016_simple_1b)

# adding time-invariant covariates instead of prov FE
lm_2016_simple_1c <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                   factor(year),
                 data = dat_2016_simple_1) # just lm
summary(lm_2016_simple_1c)
vcov_2016_simple_1c <- cluster.vcov(lm_2016_simple_1c, cluster = ~ prov)
coeftest(lm_2016_simple_1c, vcov = vcov_2016_simple_1c)


## persistent change

# 2016 only -- positive effect
dat_2016_simple_p <- dat_lme_simple %>%
  mutate(defeat.true = defeat.true.2016*(year >= 2017)) 

# without covariates
lm_2016_simple_pa <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   factor(prov) + factor(year),
                 data = dat_2016_simple_p) # just lm
summary(lm_2016_simple_pa)
vcov_2016_simple_pa <- cluster.vcov(lm_2016_simple_pa, cluster = ~ prov)
coeftest(lm_2016_simple_pa, vcov = vcov_2016_simple_pa)

# adding time-variant covariate: lagged total revenue
lm_2016_simple_pb <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   total.rev.log.lag +
                   factor(prov) + factor(year),
                 data = dat_2016_simple_p) # just lm
summary(lm_2016_simple_pb)
vcov_2016_simple_pb <- cluster.vcov(lm_2016_simple_pb, cluster = ~ prov)
coeftest(lm_2016_simple_pb, vcov = vcov_2016_simple_pb)

# adding time-invariant covariates instead of prov FE
lm_2016_simple_pc <- lm(net.trans.change.log ~ defeat.true + defeat.true.2016 +
                   num.districts.2016 + num.districts.5.2016 + num.centralnominees.2016 +
                   factor(year),
                 data = dat_2016_simple_p) # just lm
summary(lm_2016_simple_pc)
vcov_2016_simple_pc <- cluster.vcov(lm_2016_simple_pc, cluster = ~ prov)
coeftest(lm_2016_simple_pc, vcov = vcov_2016_simple_pc)


#################################
# GSYNTH FOR FULL SAMPLE
#################################

dat_synth_simple <- plan %>%
  filter(year < 2020) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong")
panelView(net.trans.log ~ defeat, data = dat_synth_simple, index = c("prov", "year"))

## One year change

dat_2016synth_simple_1 <- dat_synth_simple %>%
  mutate(treat = defeat.true.2016*as.numeric(year==2017)) %>%
  filter(year < 2018) %>%
  drop_na(net.trans.log)
panelView(net.trans.log ~ defeat, data = dat_2016synth_simple_1, index = c("prov", "year"))

system.time(
  synth_2016_simple_1 <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_simple_1, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 10),
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_simple_1)
plot(synth_2016_simple_1)
plot(synth_2016_simple_1, type = "counterfactual")

## Persistent change

dat_2016synth_simple_p <- dat_synth_simple %>%
  mutate(treat = defeat.true.2016*as.numeric(year>=2017)) %>%
  drop_na(net.trans.log)
panelView(net.trans.change.log ~ treat, data = dat_2016synth_simple_p, index = c("prov", "year"))

system.time(
  synth_2016_simple_p <- gsynth(net.trans.log ~ treat + defeat, 
                         data = dat_2016synth_simple_p, 
                         index = c("prov", "year"), force = "two-way",
                         EM = TRUE,
                         CV = TRUE, r = c(0, 10), 
                         se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE, cores = 6,
                         na.rm = TRUE)
)

print(synth_2016_simple_p)
plot(synth_2016_simple_p)
plot(synth_2016_simple_p, type = "counterfactual")

############################
# LOCAL RDD FOR FULL SAMPLE
############################

# remove Hanoi and Ho Chi Minh city
candidates2016rdd_simple <- candidates2016 %>% 
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong")

candidates2016rdd_simple$closedefeat <- as.numeric(candidates2016rdd_simple$centralnominated==1 &
                                                     candidates2016rdd_simple$defeat == 1)
candidates2016rdd_simple$closewin <- as.numeric(candidates2016rdd_simple$centralnominated==1 &
                                                  candidates2016rdd_simple$defeat != 1)
## Randomization distribution of candidate-level treatment vector
index <- which(candidates2016rdd_simple$closedefeat == 1 | candidates2016rdd_simple$closewin == 1)

set.seed(02139)
nsim <- 1000
candidates2016.closedefeat.simpe.randomized <- replicate(nsim, rbinom(length(index), 1, .5))


## Create province-year-level treatment vectors
treatment.2016.simple.randomized <- apply(candidates2016.closedefeat.simpe.randomized, 2, function(t) {
  candidates2016rdd_simple_rand <- candidates2016rdd_simple
  
  candidates2016rdd_simple_rand$closedefeat <- 0
  candidates2016rdd_simple_rand$closedefeat[index] <- t
  
  candidates2016rdd_simple_rand$closewin <- 0
  candidates2016rdd_simple_rand$closewin[index] <- 1-t
  
  treatment_rand <- treatment_generate_2016(candidates2016rdd_simple_rand)
})

## Observed treatment effects

treatment.2016.simple.observed <- treatment_generate_2016(candidates2016rdd_simple)

dat_rdd_simple <- plan %>%
  filter(year > 2012 & year < 2020) %>% # number of provinces were different before 2004
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  drop_na(net.trans.log, net.trans.lag)

## One year effect
rdd_2016_simple_1 <- list(beta = rep(NA, ncol(treatment.2016.simple.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2016.simple.randomized) +1))
for (i in 0:ncol(treatment.2016.simple.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.simple.observed  
  } else {
    treatment <- treatment.2016.simple.randomized[,i]
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
  rdd_2016_simple_1$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_simple_1$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_simple_1$beta[1] > rdd_2016_simple_1$beta[-1])
mean(rdd_2016_simple_1$beta[1] < rdd_2016_simple_1$beta[-1])

2*min(mean(rdd_2016_simple_1$beta[1] > rdd_2016_simple_1$beta[-1]),
      mean(rdd_2016_simple_1$beta[1] < rdd_2016_simple_1$beta[-1]))

plot(density(rdd_2016_simple_1$beta))
abline(v = rdd_2016_simple_1$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_simple_1$wilcox[1] > rdd_2016_simple_1$wilcox[-1])
mean(rdd_2016_simple_1$wilcox[1] < rdd_2016_simple_1$wilcox[-1])

2*min(mean(rdd_2016_simple_1$wilcox[1] > rdd_2016_simple_1$wilcox[-1]),
      mean(rdd_2016_simple_1$wilcox[1] < rdd_2016_simple_1$wilcox[-1]))

plot(density(rdd_2016_simple_1$wilcox))
abline(v = rdd_2016_simple_1$wilcox[1])

## Persistent effect
rdd_2016_simple_p <- list(beta = rep(NA, ncol(treatment.2016.simple.randomized) + 1),
                   wilcox = rep(NA, ncol(treatment.2016.simple.randomized) +1))
for (i in 0:ncol(treatment.2016.simple.randomized)) {
  
  if(i == 0) {
    treatment <- treatment.2016.simple.observed  
  } else {
    treatment <- treatment.2016.simple.randomized[,i]
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
  rdd_2016_simple_p$beta[i+1] <- coef(fit)["t.tilde"]
  
  # Wilcoxon ranksum
  rdd_2016_simple_p$wilcox[i+1] <- rank(-dat$y.tilde) %*% dat$defeat
}


# Difference in means
mean(rdd_2016_simple_p$beta[1] > rdd_2016_simple_p$beta[-1])
mean(rdd_2016_simple_p$beta[1] < rdd_2016_simple_p$beta[-1])

2*min(mean(rdd_2016_simple_p$beta[1] > rdd_2016_simple_p$beta[-1]),
      mean(rdd_2016_simple_p$beta[1] < rdd_2016_simple_p$beta[-1]))

plot(density(rdd_2016_simple_p$beta))
abline(v = rdd_2016_simple_p$beta[1])

# Wilcoxon rank sum
mean(rdd_2016_simple_p$wilcox[1] > rdd_2016_simple_p$wilcox[-1])
mean(rdd_2016_simple_p$wilcox[1] < rdd_2016_simple_p$wilcox[-1])

2*min(mean(rdd_2016_simple_p$wilcox[1] > rdd_2016_simple_p$wilcox[-1]),
      mean(rdd_2016_simple_p$wilcox[1] < rdd_2016_simple_p$wilcox[-1]))

plot(density(rdd_2016_simple_p$wilcox))
abline(v = rdd_2016_simple_p$wilcox[1])