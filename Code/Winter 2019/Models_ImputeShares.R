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
# that is constrained by a min and a max
rdirichlet_constrained <- function(n, alpha, theta_max = 1, theta_min = 0) {
  
  if(is.null(theta_max) & is.null(theta_max)) {
    return(rdirichlet(n, alpha))
  } else if(is.null(theta_max)) {
    theta_max <- 1
  } else if(is.null(theta_min)) {
    theta_min <- 0
  }
  
  draws <- replicate(n, simplify = TRUE, {
    repeat {
      d <- rdirichlet(1, alpha)
      if(max(d) < theta_max & min(d) > theta_min) {
        return(d)
        break
      }
    }
  })
  
  return(t(draws))
}

rdirichlet_constrained(10, c(1,1,1))
rdirichlet_constrained(10, c(1,1,1), .5, .2)
rdirichlet(10, c(1,1,1))
rdirichlet(1, c(1,1,1))[1,1:3]

candidates2011 %>% 
  group_by(prov, district, result) %>%
  select(prov, district, name, result, percentage,
         num.seats, num.elected, num.unelected, 
         percentage.loser.total, percentage.toploser.max) %>%
  filter(num.unelected == 3)


districts2011_multilosers <- districts2011 %>%
  group_by(prov, district) %>%
  filter(num.unelected > 1) %>%
  select(prov, district, 
         num.unelected, 
         percentage.toploser.max,
         percentage.loser.total)

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
    theta_max <- ifelse(x[2] < x[3], x[2]/x[3], 1)
    alpha <- rep(1, (x[1]+1))
    
    draw <- rdirichlet_constrained(1, 
                                   alpha,
                                   theta_max)
    
    impute[[i]] <- draw[1, 1:x[1]]
  }
  impute <- unlist(impute)
  
  return(impute)
}

# function to generate province summaries from candidate-level data
treatment_impute_2011 <- function(candidates) {
  provinces <- candidates %>%
    group_by(prov) %>%
    summarise(defeat.true = max(defeat.true, na.rm = T),
              closewin.true = max(closewin.true, na.rm=T),
              num.closewin = sum(closewin.true, na.rm=T),
              year = 2011)
  
  # province-level vector
  provinces_treatment <- provinces$defeat.true
  provinces_treatment[provinces$defeat.true == 0 & provinces$closewin.true == 0] <- NA
  
  # province-year vector
  provinces_year_treatment <- rep(provinces_treatment, each = 13)
  
  return(provinces_year_treatment)
}

# Draw a large number of randomized distribution
index_losers <- which(candidates2011$result == 0)

set.seed(02142)
nsim <- 1000000
candidates.2011.shares.imputed <- replicate(nsim, 
                                            impute_share(districts2011))*100

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
  
  # create province-level treatment vector
  treatment_impute <- treatment_impute_2011(candidates2011_imputed)
})


## One year effect
impute_2011_1 <- list(beta_1a = rep(NA, ncol(treatment.2011.imputed)),
                      beta_1b = rep(NA, ncol(treatment.2011.imputed)),
                      beta_1c = rep(NA, ncol(treatment.2011.imputed)))
for (i in 1:ncol(treatment.2011.imputed)) {
  
  treatment <- treatment.2011.imputed[,i]
  
  
  dat_2011_imputed <-plan %>%
    filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
    mutate(defeat.2011 = treatment)  %>%
    filter(prov!="Ha Noi" & prov!="TP HCM") %>%
    drop_na(net.trans.log, net.trans.lag)
  
  dat_2011_imputed_1 <- dat_2011_imputed %>%
    mutate(defeat = defeat.2011*as.numeric(year==2012)) %>%
    filter(year < 2013 & year > 2008) %>%
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

# .674 cases yield positive estimates
plot(density(impute_2011_1$beta_1a, na.rm = TRUE))
summary(impute_2011_1$beta_1a)
mean(impute_2011_1$beta_1a > 0, na.rm = TRUE)

# .717 cases yield positive estimates
plot(density(impute_2011_1$beta_1b, na.rm = TRUE))
summary(impute_2011_1$beta_1b)
mean(impute_2011_1$beta_1b > 0, na.rm = TRUE)

# .674 cases yield positive estimates
plot(density(impute_2011_1$beta_1c, na.rm = TRUE))
summary(impute_2011_1$beta_1c)
mean(impute_2011_1$beta_1c > 0, na.rm = TRUE)

## Persistent effect
impute_2011_p <- list(beta_pa = rep(NA, ncol(treatment.2011.randomized)),
                      beta_pb = rep(NA, ncol(treatment.2011.randomized)),
                      beta_pc = rep(NA, ncol(treatment.2011.randomized)))
for (i in 1:ncol(treatment.2011.imputed)) {
  
  treatment <- treatment.2011.imputed[,i]
  
  
  dat_2011_imputed <-plan %>%
    filter(year > 2004 & year < 2019) %>% # number of provinces were different before 2004
    mutate(defeat.2011 = treatment)  %>%
    filter(prov!="Ha Noi" & prov!="TP HCM") %>%
    drop_na(net.trans.log, net.trans.lag)
  
  dat_2011_imputed_p <- dat_2011_imputed %>%
    mutate(defeat = defeat.2011*as.numeric(year>=2012)) %>%
    filter(year < 2016 & year > 2008) %>%
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

# .632 cases yield positive estimates
plot(density(impute_2011_p$beta_pa, na.rm = TRUE))
summary(impute_2011_p$beta_pa)
mean(impute_2011_p$beta_pa > 0, na.rm = TRUE)

# .639 cases yield positive estimates
plot(density(impute_2011_p$beta_pb, na.rm = TRUE))
summary(impute_2011_p$beta_pb)
mean(impute_2011_p$beta_pb > 0, na.rm = TRUE)

# .632 cases yield positive estimates
plot(density(impute_2011_p$beta_pc, na.rm = TRUE))
summary(impute_2011_p$beta_pc)
mean(impute_2011_p$beta_pc > 0, na.rm = TRUE)

