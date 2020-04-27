## THIS FILE CONTAINS ANALYSIS DONE ON IMPUTED HYPOTHETICAL TREATMENT VECTORS
## CONSTRUCTED BY RANDOMLY DISTRIBUTE LOSERS' VOTE SHARES AMONG INDIVIDUAL
## LOSERS

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Minh Trinh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("~/vietnam_elections/Data/Working Data")

source("../../Code/Winter 2019/Merge_All.R")

candidates2011 %>% 
  group_by(district, result) %>%
  select(prov, district, name, result, num.seats, num.elected, num.unelected, 
         percentage.loser.total, percentage.toploser.max) %>%
  filter(num.unelected == 3)

table(candidates2011$num.unelected)

# Function that uses rejection sampling to get a rdirichlet draw
# with a maximum and a minimum each constrained within a bound
rdirichlet_constrained <- function(n, alpha, max_max = 1, max_min = 0, min_max = 1, min_min = 0) {
  
  if(is.null(max_max) & is.null(max_min) & is.null(min_max) & is.null(min_min)) {
    return(rdirichlet(n, alpha))
  } else if(is.null(max_max)) {
    max_max <- 1
  } else if(is.null(max_min)) {
    max_min <- 0
  } else if(is.null(min_max)) {
    min_max <- 0
  } else if(is.null(min_min)) {
    min_min <- 0
  }
  
  
  draws <- replicate(n, simplify = TRUE, {
    repeat {
      d <- rdirichlet(1, alpha)
      if(max(d) < max_max & 
         max(d) > max_min &
         min(d) < min_max &
         min(d) > min_min) {
        return(d)
        break
      }
    }
  })
  
  return(t(draws))
}

d <- rdirichlet_constrained(1, c(99,99,1), max_max = .55, min_max = .1)[1,]

## Randomization distribution of candidate-level vote shares

# Function to draw a vector of random vote shares among loser candidates 
# plus writeins and other unallocated shares.

impute_share <- function(district) {
  impute <- vector(mode = "list", 
                   length = nrow(district))
  for(i in 1:nrow(district)) {
    x <- as.numeric(district[i, c("num.unelected",
                                  "percentage.toploser.max",
                                  "percentage.loser.total")])
    max_max <- ifelse(x[2] < x[3], x[2]/x[3], 1)
    
    # flat dirichlet shape parameters -- no assumption
    alpha <- rep(1, (x[1]+1))
    
    # POSSIBLE EXTENSION:
    # non-flat shape parameters -- may use data
    # from 2016 to construct empirical distribution
    # and estimate shape parameters
    
    # if(x[1] == 1) {
    #   alpha <- c(99,1)
    # } else if(x[1] == 2) {
    #   alpha <- c(53, 43, 4)
    # } else if(x[1] == 3) {
    #   alpha <- c(46, 33, 17 ,4)
    # }
    
    min_max <- 10/x[3] # no district shall have more than 10% unallocated shares
    
    draw <- rdirichlet_constrained(1, 
                                   alpha,
                                   max_max = max_max,
                                   min_max = min_max)[1,]*x[3]
    
    index_min <- which(draw == min(draw))
    
    impute[[i]] <- draw[-index_min]
  }
  impute <- unlist(impute)
  
  return(impute)
}

###############################
# IMPUTATION ANALYSIS FOR 2011
###############################

# function to generate province summaries from candidate-level data
treatment_impute_2011 <- function(candidates, years = c(2005:2017)) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(defeat.true = max(defeat.true, na.rm = T),
              closewin.true = max(closewin.true, na.rm=T),
              num.closewin = sum(closewin.true, na.rm=T),
              year = 2011)
  
  # province-level vector
  provinces_treatment <- provinces$defeat.true
  provinces_treatment[provinces$defeat.true == 0 & provinces$closewin.true == 0] <- NA
  
  names(provinces_treatment) <- provinces$prov
  
  return(provinces_treatment)
  
  # province-year vector
  #provinces_year_treatment <- rep(provinces_treatment, each = length(years))
  
  #return(provinces_year_treatment)
}

# Draw a large number of randomized distribution for districts that experienced
# central nominee defeats
districts2011_central_defeats <- districts2011 %>% filter(defeat == 1)

impute_share(districts2011_central_defeats)

index_losers <- candidates2011 %>%
  group_by(prov, district) %>%
  mutate(district_defeat = sum(centralnominated*defeat),
         losers = (result == 0) & (district_defeat > 0)) %>%
  pull(losers) %>%
  which

set.seed(02142)
nsim <- 10000
candidates.2011.shares.imputed <- replicate(nsim, 
                                            impute_share(districts2011_central_defeats))

## Create province-year-level treatment vectors
treatment.2011.imputed <- apply(candidates.2011.shares.imputed, 2, function(t) {
  candidates2011_imputed <- candidates2011 
  
  candidates2011_imputed$percentage[index_losers] <-
    t
  
  # create candidate-level data using imputed shares
  candidates2011_imputed <- candidates2011_imputed %>%
    group_by(prov, district) %>%
    mutate(percentage.winner = ifelse(result == 1, percentage, NA),
           percentage.loser = ifelse(result == 0, percentage, NA),
           percentage.central = ifelse(centralnominated == 1, percentage, NA),
           percentage.local = ifelse(centralnominated == 0, percentage, NA),
           share.central.min = min(percentage.central, na.rm=T),
           share.loser.max = max(percentage.loser, na.rm=T),
           share.winner.min = min(percentage.winner, na.rm=T)) %>%
    # individual margin
    mutate(margin = (1-defeat)*(percentage - pmax(share.loser.max, 50)) +
             (defeat)*ifelse(num.seats > num.elected,
                             percentage - 50,
                             percentage - share.winner.min)) %>%
    # candidate ranking among winners and losers 
    # s.t. close wins and defeats are only counted among those closest to winning and losing
    group_by(prov, district, result) %>%
    mutate(rank = rank(percentage)) %>%
    # a closewin.true variable to test coding of closewin
    mutate(closewin.true = as.numeric(centralnominated == 1 & result == 1 & margin < 10 
                                      & (rank == min(rank) | percentage < 60))) %>%
    # a defeat.true variable to test coding of defeat
    mutate(defeat.true = as.numeric(centralnominated == 1 & result == 0 & margin > -10)) %>%
    ungroup
  
  # create province-year-level treatment vector
  treatment_impute <- treatment_impute_2011(candidates2011_imputed)
})

## Frequency at which each province appears in the sample,
## and frequency at which each province is "treated"

data.frame(prov = plan %>%
             filter(year == 2011) %>% 
             pull(prov), 
           in_sample = rowMeans(!is.na(treatment.2011.imputed), na.rm = T),
           treated = rowMeans(treatment.2011.imputed, na.rm = TRUE)) %>% 
  filter(in_sample > 0) %>% group_by(prov) %>% 
  summarise(in_sample = max(in_sample), treated = max(treated))

## Fit Linear Models
dat_lme <- plan %>%
  filter(year > 2008 & year < 2019) %>%
  mutate(defeat = ifelse(year == 2008, 0, defeat)) %>% 
  filter(defeat.2011!=0 | closewin.2011!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Long An") %>%
  drop_na(net.trans.log, net.trans.lag)

## One year effect
impute_2011_1 <- list(beta_1a = rep(NA, ncol(treatment.2011.imputed)),
                      beta_1b = rep(NA, ncol(treatment.2011.imputed)),
                      beta_1c = rep(NA, ncol(treatment.2011.imputed)))
for (i in 1:ncol(treatment.2011.imputed)) {
  
  treatment <- tibble(prov = names(treatment.2011.imputed[,i]),
                      treat = treatment.2011.imputed[,i])
  
  ## One-year effect

  dat_2011_imputed_1 <- dat_lme %>%
    inner_join(treatment, by = "prov") %>%
    mutate(defeat.2011 = treat)  %>%
    mutate(defeat = defeat.2011*as.numeric(year==2012)) %>%
    filter(year < 2013) %>%
    drop_na(defeat, net.trans.log)
    
  # Run model with out covariates
  lm_2011_imputed_1a <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                     factor(prov) + factor(year),
                   data = dat_2011_imputed_1)
  impute_2011_1$beta_1a[i] <- coef(lm_2011_imputed_1a)["defeat"]
  
  # adding time-variant covariate: lagged total revenue
  lm_2011_imputed_1b <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                     total.rev.log.lag +
                     factor(prov) + factor(year),
                   data = dat_2011_imputed_1)
  impute_2011_1$beta_1b[i] <- coef(lm_2011_imputed_1b)["defeat"]
  
  # adding time-invariant covariates instead of prov FE
  lm_2011_imputed_1c <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                     num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                     factor(year),
                   data = dat_2011_imputed_1)
  impute_2011_1$beta_1c[i] <- coef(lm_2011_imputed_1c)["defeat"]

}

# .723 cases yield positive estimates
plot(density(impute_2011_1$beta_1a, na.rm = TRUE))
summary(impute_2011_1$beta_1a)
mean(impute_2011_1$beta_1a > 0, na.rm = TRUE)

# .848 cases yield positive estimates
plot(density(impute_2011_1$beta_1b, na.rm = TRUE))
summary(impute_2011_1$beta_1b)
mean(impute_2011_1$beta_1b > 0, na.rm = TRUE)

# .723 cases yield positive estimates
plot(density(impute_2011_1$beta_1c, na.rm = TRUE))
summary(impute_2011_1$beta_1c)
mean(impute_2011_1$beta_1c > 0, na.rm = TRUE)

## Persistent effect
impute_2011_p <- list(beta_pa = rep(NA, ncol(treatment.2011.randomized)),
                      beta_pb = rep(NA, ncol(treatment.2011.randomized)),
                      beta_pc = rep(NA, ncol(treatment.2011.randomized)))
for (i in 1:ncol(treatment.2011.imputed)) {
  
  treatment <- tibble(prov = names(treatment.2011.imputed[,i]),
                      treat = treatment.2011.imputed[,i])
  
  dat_2011_imputed_p <- dat_lme %>%
    inner_join(treatment, by = "prov") %>%
    mutate(defeat.2011 = treat)  %>%
    mutate(defeat = defeat.2011*as.numeric(year>=2012)) %>%
    filter(year < 2016) %>%
    drop_na(defeat, net.trans.log) 
  
  # Run model with out covariates
  lm_2011_imputed_pa <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                             factor(prov) + factor(year),
                           data = dat_2011_imputed_p)
  impute_2011_p$beta_pa[i] <- coef(lm_2011_imputed_pa)["defeat"]
  
  # adding time-variant covariate: lagged total revenue
  lm_2011_imputed_pb <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                             total.rev.log.lag +
                             factor(prov) + factor(year),
                           data = dat_2011_imputed_p)
  impute_2011_p$beta_pb[i] <- coef(lm_2011_imputed_pb)["defeat"]
  
  # adding time-invariant covariates instead of prov FE
  lm_2011_imputed_pc <- lm(net.trans.change.log ~ defeat + defeat.2011 +
                             num.districts.2011 + num.districts.5.2011 + num.centralnominees.2011 +
                             factor(year),
                           data = dat_2011_imputed_p)
  impute_2011_p$beta_pc[i] <- coef(lm_2011_imputed_pc)["defeat"]
  
}

# .874 cases yield positive estimates
plot(density(impute_2011_p$beta_pa, na.rm = TRUE))
summary(impute_2011_p$beta_pa)
mean(impute_2011_p$beta_pa > 0, na.rm = TRUE)

# .874 cases yield positive estimates
plot(density(impute_2011_p$beta_pb, na.rm = TRUE))
summary(impute_2011_p$beta_pb)
mean(impute_2011_p$beta_pb > 0, na.rm = TRUE)

# .874 cases yield positive estimates
plot(density(impute_2011_p$beta_pc, na.rm = TRUE))
summary(impute_2011_p$beta_pc)
mean(impute_2011_p$beta_pc > 0, na.rm = TRUE)

###############################
# IMPUTATION ANALYSIS FOR 2007
###############################

# function to generate province summaries from candidate-level data
treatment_impute_2007 <- function(candidates, years = c(2004,2006:2017)) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(defeat.true = max(defeat.true, na.rm = T),
              closewin.true = max(closewin.true, na.rm=T),
              num.closewin = sum(closewin.true, na.rm=T),
              year = 2007)
  
  # province-level vector
  provinces_treatment <- provinces$defeat.true
  provinces_treatment[provinces$defeat.true == 0 & provinces$closewin.true == 0] <- NA
  
  names(provinces_treatment) <- provinces$prov
  
  return(provinces_treatment)
  
  # province-year vector
  #provinces_year_treatment <- rep(provinces_treatment, each = length(years))
  
  #return(provinces_year_treatment)
}

# Draw a large number of randomized distribution for districts that experienced
# central nominee defeats
districts2007_central_defeats <- districts2007 %>% filter(defeat == 1)

impute_share(districts2007_central_defeats)

index_losers <- candidates2007 %>%
  group_by(prov, district) %>%
  mutate(district_defeat = sum(centralnominated*defeat),
         losers = (result == 0) & (district_defeat > 0)) %>%
  pull(losers) %>%
  which

set.seed(02142)
nsim <- 10000
candidates.2007.shares.imputed <- replicate(nsim, 
                                            impute_share(districts2007_central_defeats))

## Create province-year-level treatment vectors
treatment.2007.imputed <- apply(candidates.2007.shares.imputed, 2, function(t) {
  candidates2007_imputed <- candidates2007
  
  candidates2007_imputed$percentage[index_losers] <-
    t
  
  # create candidate-level data using imputed shares
  candidates2007_imputed <- candidates2007_imputed %>%
    filter(prov != "Ha Tay") %>%
    group_by(prov, district) %>%
    mutate(percentage.winner = ifelse(result == 1, percentage, NA),
           percentage.loser = ifelse(result == 0, percentage, NA),
           percentage.central = ifelse(centralnominated == 1, percentage, NA),
           percentage.local = ifelse(centralnominated == 0, percentage, NA),
           share.central.min = min(percentage.central, na.rm=T),
           share.loser.max = max(percentage.loser, na.rm=T),
           share.winner.min = min(percentage.winner, na.rm=T)) %>%
    # individual margin
    mutate(margin = (1-defeat)*(percentage - pmax(share.loser.max, 50)) +
             (defeat)*ifelse(num.seats > num.elected,
                             percentage - 50,
                             percentage - share.winner.min)) %>%
    # candidate ranking among winners and losers 
    # s.t. close wins and defeats are only counted among those closest to winning and losing
    group_by(prov, district, result) %>%
    mutate(rank = rank(percentage)) %>%
    # a closewin.true variable to test coding of closewin
    mutate(closewin.true = as.numeric(centralnominated == 1 & result == 1 & margin < 10 
                                      & (rank == min(rank) | percentage < 60))) %>%
    # a defeat.true variable to test coding of defeat
    mutate(defeat.true = as.numeric(centralnominated == 1 & result == 0 & margin > -10)) %>%
    ungroup
  
  # create province-year-level treatment vector
  treatment_impute <- treatment_impute_2007(candidates2007_imputed)
})

## Frequency at which each province appears in the sample,
## and frequency at which each province is "treated"

data.frame(prov = plan %>%
             pull(prov) %>% 
             unique, 
           in_sample = rowMeans(!is.na(treatment.2007.imputed), na.rm = T),
           treated = rowMeans(treatment.2007.imputed, na.rm = TRUE)) %>% 
  filter(in_sample > 0) %>% group_by(prov) %>% 
  summarise(in_sample = max(in_sample), treated = max(treated))

## Fit Linear Models
dat_lme <- plan %>%
  filter(defeat.2007!=0 | closewin.2007!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  drop_na(net.trans.log, net.trans.lag)

## One year effect
impute_2007_1 <- list(beta_1a = rep(NA, ncol(treatment.2007.imputed)),
                      beta_1b = rep(NA, ncol(treatment.2007.imputed)),
                      beta_1c = rep(NA, ncol(treatment.2007.imputed)))
for (i in 1:ncol(treatment.2007.imputed)) {
  
  treatment <- tibble(prov = names(treatment.2007.imputed[,i]),
                      treat = treatment.2007.imputed[,i])
  
  dat_2007_imputed_1 <- dat_lme %>%
    inner_join(treatment, by = "prov") %>%
    mutate(defeat.2007 = treat)  %>%
    mutate(defeat = defeat.2007*as.numeric(year==2008)) %>%
    filter(year < 2009) %>%
    drop_na(defeat, net.trans.log)
  
  # Run model with out covariates
  lm_2007_imputed_1a <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             factor(prov) + factor(year),
                           data = dat_2007_imputed_1)
  impute_2007_1$beta_1a[i] <- coef(lm_2007_imputed_1a)["defeat"]
  
  # adding time-variant covariate: lagged total revenue
  lm_2007_imputed_1b <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             total.rev.log.lag +
                             factor(prov) + factor(year),
                           data = dat_2007_imputed_1)
  impute_2007_1$beta_1b[i] <- coef(lm_2007_imputed_1b)["defeat"]
  
  # adding time-invariant covariates instead of prov FE
  lm_2007_imputed_1c <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                             factor(year),
                           data = dat_2007_imputed_1)
  impute_2007_1$beta_1c[i] <- coef(lm_2007_imputed_1c)["defeat"]
  
}

# .742 cases yield positive estimates
plot(density(impute_2007_1$beta_1a, na.rm = TRUE))
summary(impute_2007_1$beta_1a)
mean(impute_2007_1$beta_1a > 0, na.rm = TRUE)

# .742 cases yield positive estimates
plot(density(impute_2007_1$beta_1b, na.rm = TRUE))
summary(impute_2007_1$beta_1b)
mean(impute_2007_1$beta_1b > 0, na.rm = TRUE)

# .742 cases yield positive estimates
plot(density(impute_2007_1$beta_1c, na.rm = TRUE))
summary(impute_2007_1$beta_1c)
mean(impute_2007_1$beta_1c > 0, na.rm = TRUE)

## Persistent effect
impute_2007_p <- list(beta_pa = rep(NA, ncol(treatment.2007.randomized)),
                      beta_pb = rep(NA, ncol(treatment.2007.randomized)),
                      beta_pc = rep(NA, ncol(treatment.2007.randomized)))
for (i in 1:ncol(treatment.2007.imputed)) {
  
  treatment <- tibble(prov = names(treatment.2007.imputed[,i]),
                      treat = treatment.2007.imputed[,i])
  
  dat_2007_imputed_p <- dat_lme %>%
    inner_join(treatment, by = "prov") %>%
    mutate(defeat.2007 = treat)  %>%
    mutate(defeat = defeat.2007*as.numeric(year>=2008)) %>%
    filter(year < 2011) %>%
    drop_na(defeat, net.trans.log) 
  
  # Run model with out covariates
  lm_2007_imputed_pa <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             factor(prov) + factor(year),
                           data = dat_2007_imputed_p)
  impute_2007_p$beta_pa[i] <- coef(lm_2007_imputed_pa)["defeat"]
  
  # adding time-variant covariate: lagged total revenue
  lm_2007_imputed_pb <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             total.rev.log.lag +
                             factor(prov) + factor(year),
                           data = dat_2007_imputed_p)
  impute_2007_p$beta_pb[i] <- coef(lm_2007_imputed_pb)["defeat"]
  
  # adding time-invariant covariates instead of prov FE
  lm_2007_imputed_pc <- lm(net.trans.change.log ~ defeat + defeat.2007 +
                             num.districts.2007 + num.districts.5.2007 + num.centralnominees.2007 +
                             factor(year),
                           data = dat_2007_imputed_p)
  impute_2007_p$beta_pc[i] <- coef(lm_2007_imputed_pc)["defeat"]
  
}

# .492 cases yield positive estimates
plot(density(impute_2007_p$beta_pa, na.rm = TRUE))
summary(impute_2007_p$beta_pa)
mean(impute_2007_p$beta_pa > 0, na.rm = TRUE)

# .492 cases yield positive estimates
plot(density(impute_2007_p$beta_pb, na.rm = TRUE))
summary(impute_2007_p$beta_pb)
mean(impute_2007_p$beta_pb > 0, na.rm = TRUE)

# .492 cases yield positive estimates
plot(density(impute_2007_p$beta_pc, na.rm = TRUE))
summary(impute_2007_p$beta_pc)
mean(impute_2007_p$beta_pc > 0, na.rm = TRUE)

