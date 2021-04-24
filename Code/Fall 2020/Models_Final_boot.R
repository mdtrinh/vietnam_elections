library(gsynth)
library(panelView)
library(multiwayvcov)
library(lmtest)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Fall 2020/Merge_All.R")

#### Bootstrap to test if results are sensitive to adding Soc Trang to treated provinces ####

prov_heavy_2016 <- "Soc Trang"

lm_2016_treat_add <- t(sapply(prov_heavy_2016, function(x) {
  
  dat_lme <- plan %>%
    filter(year < 2019 & year > 2012) %>%
    mutate(defeat.true.2016 = ifelse(prov == x, 1, defeat.true.2016)) %>%
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
  
  out <- cbind(lm_1a = coeftest(lm_2016_1a, vcov = vcov_2016_1a)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1b = coeftest(lm_2016_1b, vcov = vcov_2016_1b)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1c = coeftest(lm_2016_1c, vcov = vcov_2016_1c)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pa = coeftest(lm_2016_pa, vcov = vcov_2016_pa)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pb = coeftest(lm_2016_pb, vcov = vcov_2016_pb)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pc = coeftest(lm_2016_pc, vcov = vcov_2016_pc)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")])
  
  return(out)
  
}))

colnames(lm_2016_treat_add) <- c("Estimate_1a", "SE_1a", "P_1a",
                                 "Estimate_1b", "SE_1b", "P_1b",
                                 "Estimate_1c", "SE_1c", "P_1c",
                                 "Estimate_pa", "SE_pa", "P_pa",
                                 "Estimate_pb", "SE_pb", "P_pb",
                                 "Estimate_pc", "SE_pc", "P_pc")


lm_2016_treat_add_summary <- as_tibble(lm_2016_treat_add) %>%
  gather(key = "key", value = "value", Estimate_1a:P_pc) %>%
  separate(key, c("Measure", "Model")) %>%
  spread(key = "Measure", value = "value") %>%
  mutate(n_change = 1)


#### Bootstrap to test if results are sensitive to dropping treated province(s) ####

prov_defeat_2016 <- provinces2016$prov[provinces2016$defeat.true == 1]

prov_defeat_2016_grid <- expand.grid(rep(list(0:1), length(prov_defeat_2016)))
prov_defeat_2016_grid <- prov_defeat_2016_grid[-nrow(prov_defeat_2016_grid),]
colnames(prov_defeat_2016_grid) <- prov_defeat_2016

## Linear regressions results for 2016 elections ##

lm_2016_treat_drop <- t(apply(prov_defeat_2016_grid, 1, function(p) {
  
  prov_exclude <- prov_defeat_2016[which(p==1)]
  
  dat_lme <- plan %>%
    filter(year < 2019 & year > 2012) %>%
    filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
    #filter(prov!="Ha Noi" & prov!="TP HCM") %>%
    filter(!prov %in% prov_exclude) %>%
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
  
  out <- cbind(lm_1a = coeftest(lm_2016_1a, vcov = vcov_2016_1a)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
            lm_1b = coeftest(lm_2016_1b, vcov = vcov_2016_1b)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
            lm_1c = coeftest(lm_2016_1c, vcov = vcov_2016_1c)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
            lm_pa = coeftest(lm_2016_pa, vcov = vcov_2016_pa)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
            lm_pb = coeftest(lm_2016_pb, vcov = vcov_2016_pb)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
            lm_pc = coeftest(lm_2016_pc, vcov = vcov_2016_pc)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")])

  return(out)
  
}))

colnames(lm_2016_treat_drop) <- c("Estimate_1a", "SE_1a", "P_1a",
                                  "Estimate_1b", "SE_1b", "P_1b",
                                  "Estimate_1c", "SE_1c", "P_1c",
                                  "Estimate_pa", "SE_pa", "P_pa",
                                  "Estimate_pb", "SE_pb", "P_pb",
                                  "Estimate_pc", "SE_pc", "P_pc")
lm_2016_treat_drop_summary <- as_tibble(cbind(prov_defeat_2016_grid, lm_2016_treat_drop)) %>%
  gather(key = "key", value = "value", Estimate_1a:P_pc) %>%
  separate(key, c("Measure", "Model")) %>%
  spread(key = "Measure", value = "value") %>%
  mutate(n_change = rowSums(.[1:length(prov_defeat_2016)]) * (-1))

## Summary of results

# Result with all treated provinces, including HN and HCMC: no significant
# result
lm_2016_treat_drop_summary %>%
  filter(n_change == 0)

# Result with one province removed, not forcing HN and HCMC: no significant
# result
lm_2016_treat_drop_summary %>%
  filter(n_change == -1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

# Result with one province removed, forcing HN and HCMC: only removing Can Tho
# hurts but not all the time
lm_2016_treat_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_treat_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1a") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1b") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1c")   %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "pa")   %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

# Result with two provinces removed, not forcing HN and HCMC: no significant
# result
lm_2016_treat_drop_summary %>%
  filter(n_change == -2) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

# Result with two provinces removed, forcing HN and HCMC: removing two of Can
# Tho, Phu Yen, Tra Vinh hurts but not all the times
lm_2016_treat_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_treat_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1a") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1b")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1c")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_treat_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "pa")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

#### Bootstrap to test if results are sensitive to dropping control province(s) ####

prov_closewin_2016 <- provinces2016$prov[provinces2016$closewin.true == 1]

prov_closewin_2016_grid <- expand.grid(rep(list(0:1), length(prov_closewin_2016)))
prov_closewin_2016_grid <- prov_closewin_2016_grid[-nrow(prov_closewin_2016_grid),]
colnames(prov_closewin_2016_grid) <- prov_closewin_2016

## Linear regressions results for 2016 elections ##

lm_2016_control_drop <- t(apply(prov_closewin_2016_grid, 1, function(p) {
  
  prov_exclude <- prov_closewin_2016[which(p==1)]
  
  dat_lme <- plan %>%
    filter(year < 2019 & year > 2012) %>%
    filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
    #filter(prov!="Ha Noi" & prov!="TP HCM") %>%
    filter(!prov %in% prov_exclude) %>%
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
  
  out <- cbind(lm_1a = coeftest(lm_2016_1a, vcov = vcov_2016_1a)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1b = coeftest(lm_2016_1b, vcov = vcov_2016_1b)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1c = coeftest(lm_2016_1c, vcov = vcov_2016_1c)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pa = coeftest(lm_2016_pa, vcov = vcov_2016_pa)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pb = coeftest(lm_2016_pb, vcov = vcov_2016_pb)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pc = coeftest(lm_2016_pc, vcov = vcov_2016_pc)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")])
  
  return(out)
  
}))

colnames(lm_2016_control_drop) <- c("Estimate_1a", "SE_1a", "P_1a",
                                    "Estimate_1b", "SE_1b", "P_1b",
                                    "Estimate_1c", "SE_1c", "P_1c",
                                    "Estimate_pa", "SE_pa", "P_pa",
                                    "Estimate_pb", "SE_pb", "P_pb",
                                    "Estimate_pc", "SE_pc", "P_pc")
lm_2016_control_drop_summary <- as_tibble(cbind(prov_closewin_2016_grid, lm_2016_control_drop)) %>%
  gather(key = "key", value = "value", Estimate_1a:P_pc) %>%
  separate(key, c("Measure", "Model")) %>%
  spread(key = "Measure", value = "value") %>%
  mutate(n_change = rowSums(.[1:length(prov_closewin_2016)]) * (-1))

## Summary of results

# Result with all control provinces, including HN and HCMC: no significant
# result
lm_2016_control_drop_summary %>%
  filter(n_change == 0) %>%
  select(Model, Estimate, P)

# Result with one province removed, not forcing HN and HCMC: no significant
# result
lm_2016_control_drop_summary %>%
  filter(n_change == -1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

# Result with one province removed, forcing HN and HCMC: removing either Thai
# Nguyen, Thai Binh or Long An hurts, but no effect on persistent results
lm_2016_control_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_control_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1a") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_control_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1b") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_control_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1c")   %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_control_drop_summary %>%
  filter(n_change == -3 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "pa")   %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

# Result with two provinces removed, not forcing HN and HCMC: no significant
# result
lm_2016_control_drop_summary %>%
  filter(n_change == -2) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

# Result with two provinces removed, forcing HN and HCMC: some cases get hurt,
# but not the persistent results
lm_2016_control_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_control_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1a") %>%
  filter(P > .1) %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) 

lm_2016_control_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1b")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_control_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "1c")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

lm_2016_control_drop_summary %>%
  filter(n_change == -4 & `Ha Noi` == 1 & `TP HCM` == 1) %>%
  filter(Model == "pa")  %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n()))

#### Bootstrap to test if results are sensitive to adding control province(s) ####

prov_unused_2016 <- provinces2016$prov[provinces2016$closewin.true == 0 & provinces2016$defeat.true == 0]

prov_unused_2016_grid <- t(cbind(ri::genperms(c(1, rep(0, length(prov_unused_2016) - 1))),
                                 ri::genperms(c(1, 1, rep(0, length(prov_unused_2016) - 2)))))
colnames(prov_unused_2016_grid) <- prov_unused_2016

prov_unused_2016_list <- apply(prov_unused_2016_grid, 1, function(p) {
  paste(prov_unused_2016[which(p==1)], collapse = " & ")
})

## Linear regressions results for 2016 elections ##

lm_2016_control_add <- t(apply(prov_unused_2016_grid, 1, function(p) {
  
  prov_include <- prov_unused_2016[which(p==1)]
  
  dat_lme <- plan %>%
    filter(year < 2019 & year > 2012) %>%
    mutate(closewin.true.2016 = ifelse(prov %in% prov_include, 1, closewin.true.2016)) %>%
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
  
  out <- cbind(lm_1a = coeftest(lm_2016_1a, vcov = vcov_2016_1a)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1b = coeftest(lm_2016_1b, vcov = vcov_2016_1b)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_1c = coeftest(lm_2016_1c, vcov = vcov_2016_1c)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pa = coeftest(lm_2016_pa, vcov = vcov_2016_pa)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pb = coeftest(lm_2016_pb, vcov = vcov_2016_pb)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")],
               lm_pc = coeftest(lm_2016_pc, vcov = vcov_2016_pc)["defeat.true", c("Estimate", "Std. Error", "Pr(>|t|)")])
  
  return(out)
  
}))

colnames(lm_2016_control_add) <- c("Estimate_1a", "SE_1a", "P_1a",
                                   "Estimate_1b", "SE_1b", "P_1b",
                                   "Estimate_1c", "SE_1c", "P_1c",
                                   "Estimate_pa", "SE_pa", "P_pa",
                                   "Estimate_pb", "SE_pb", "P_pb",
                                   "Estimate_pc", "SE_pc", "P_pc")
lm_2016_control_add_summary <- as_tibble(cbind(prov_unused_2016_grid, lm_2016_control_add)) %>%
  mutate(Prov_include = prov_unused_2016_list) %>%
  gather(key = "key", value = "value", Estimate_1a:P_pc) %>%
  separate(key, c("Measure", "Model")) %>%
  spread(key = "Measure", value = "value") %>%
  mutate(n_change = rowSums(.[1:length(prov_unused_2016)]))

## Summary of results

# Result with one province added: mostly significant
lm_2016_control_add_summary %>%
  filter(n_change == 1) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_control_add_summary %>%
  filter(n_change == 1) %>%
  filter(Model == "1a") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 50)

lm_2016_control_add_summary %>%
  filter(n_change == 1) %>%
  filter(Model == "1b") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 50)

lm_2016_control_add_summary %>%
  filter(n_change == 1) %>%
  filter(Model == "1c") %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 50)


# Result with two provinces added: mostly significant
lm_2016_control_add_summary %>%
  filter(n_change == 2) %>%
  group_by(Model) %>%
  summarise(n_mod = n(),
            n_sig_05 = sum(P < .05),
            n_sig_1 = sum(P < .1))

lm_2016_control_add_summary %>%
  filter(n_change == 2 ) %>%
  filter(Model == "1a") %>%
  filter(P > .1) %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 100)

lm_2016_control_add_summary %>%
  filter(n_change == 2 ) %>%
  filter(Model == "1b") %>%
  filter(P > .1) %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 100)

lm_2016_control_add_summary %>%
  filter(n_change == 2 ) %>%
  filter(Model == "1c") %>%
  filter(P > .1) %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 100)

lm_2016_control_add_summary %>%
  filter(n_change == 2 ) %>%
  filter(Model == "pa") %>%
  filter(P > .05) %>%
  mutate(P_adj = p.adjust(P, method = "BH", n = n())) %>%
  select(Model, Prov_include, Estimate, P, P_adj) %>%
  print(n = 100)

#### Plot findings

boot_plot_dat_gen <- function(boot_summary) {
  boot_summary %>%
    mutate(one_change = if("Ha Noi" %in% names(.)) 
      (abs(n_change) == 3 & `Ha Noi` == 1 & `TP HCM` == 1) else (abs(n_change) == 1)) %>%
    filter(one_change) %>%
    select(Model, Estimate, SE, P) %>%
    mutate(lower = Estimate - 1.96*SE,
           upper = Estimate + 1.96*SE,
           lower_90 = Estimate - 1.65*SE,
           upper_90 = Estimate + 1.65*SE) %>%
    separate(Model, into = c("Effect", "Model"), sep = 1) %>%
    mutate(Effect = factor(Effect, 
                           labels = c("Instantaenous Effect", "Persistent Effect")),
           Model = factor(Model, 
                          labels=c("a" = "Province FEs +\nYear FEs", 
                                   "b" = "Time-variant Covs +\nProvince FEs +\nYear FEs",
                                   "c" = "Competitiveness +\nYear FEs"))) %>%
    filter(Effect == "Instantaenous Effect")
}

lm_2016_treat_add_plot_dat <- boot_plot_dat_gen(lm_2016_treat_add_summary)

ggplot(lm_2016_treat_add_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.4), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                  position = position_dodge(width = 0.4),
                  size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect") +
  xlab("Model Specification") +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")

lm_2016_treat_drop_plot_dat <- boot_plot_dat_gen(lm_2016_treat_drop_summary)

ggplot(lm_2016_treat_drop_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.4), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.4),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect") +
  xlab("Model Specification") +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")

lm_2016_control_add_plot_dat <- boot_plot_dat_gen(lm_2016_control_add_summary)

ggplot(lm_2016_control_add_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.8), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.8),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect") +
  xlab("Model Specification") +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")

lm_2016_control_drop_plot_dat <- boot_plot_dat_gen(lm_2016_control_drop_summary)

ggplot(lm_2016_control_drop_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.8), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.8),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect") +
  xlab("Model Specification") +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")
