library(foreign)
library(sandwich)
library(lmtest)
library(mfx)
library(dplyr)

setwd("D:/Dropbox (MIT)/Documents/Works/G1 17.830 Empirical Methods in Political Economy/Project/Data/Malesky and Schuler/")

source("../../Code/SYP/Merge_All.R")

# helper function to calculate marginal effect (PEA)
# source: http://researchrepository.ucd.ie/bitstream/handle/10197/3404/WP11_22.pdf?sequence=1
mfx <- function(x, robust=TRUE, cluster=NULL, returnvcov=FALSE){
  if(as.character(x$call)[1]=="lm") {
    
    # robust or normal SEs
    # for Stata 10's reg, "robust" vcov is 
    # the HC1 vcov
    if(robust==TRUE){
      vcov <- vcovHC(x, type="HC1")
    } else {
      vcov <- vcov(x)
    }
    
    if(!is.null(cluster)){
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- x$rank
      
      dfc <- (M/(M-1))*((N-1)/(N-K))
      
      #calculate the uj's
      uj  <- apply(estfun(x),2, function(c) tapply(c, cluster, sum))
      
      #use sandwich to get the var-covar matrix
      vcov <- dfc*sandwich(x, meat=crossprod(uj)/N)
    }
    
    out <- sapply(1:length(coef(x)), function(i) {
      tau <- coef(x)[i]
      
      v.tau <- vcov[i,i]
      
      return(cbind(tau, sqrt(v.tau)))
    })
    
  } else if(as.character(x$call)[1]=="glm") {
    
    # robust or normal SEs
    # for Stata 10's probit, "robust" vcov is 
    # the HC0 vcov with n/(n-1) degree-of-freedom correction
    if(robust==TRUE){
      vcov <- vcovHC(x, type="HC0") * nobs(x) / (nobs(x)-1)
    } else {
      vcov <- vcov(x)
    }
    
    if(!is.null(cluster)){
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- x$rank
      
      dfc <- (M/(M-1))*((N-1)/(N-K))
      
      #calculate the uj's
      uj  <- apply(estfun(x),2, function(c) tapply(c, cluster, sum))
      
      #use sandwich to get the var-covar matrix
      vcov <- dfc*sandwich(x, meat=crossprod(uj)/N)
    }
    
    F <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                pnorm,
                plogis)
    
    f <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",
                dnorm,
                dlogis)
    
    mean <- colMeans(model.matrix(x))
    out <- sapply(1:length(coef(x)), function(i) {
      # binary variables
      if(length(unique(model.matrix(x)[,i]))==2) {
        term.0 <- mean[-i] %*% coef(x)[-i]
        term.1 <- term.0 + coef(x)[i]
        
        # marginal effect at the mean
        tau <- F(term.1) - F(term.0)
        
        # delta method to calculate standard error
        dBdt <- mean * (f(term.1)-f(term.0))
        dBdt[i] <- f(term.1)
        
        v.tau <- t(dBdt) %*% vcov %*% dBdt
      }
      # continuous variables
      else {
        term <- mean %*% coef(x)
        
        # marginal effect at the mean
        tau <- f(term) * coef(x)[i]
        
        # delta method to calculate standard error
        dBdt <- coef(x)[i] * mean * (-term*dnorm(term)) # -x*dnorm(x) = derivative of Std.Normal PDF
        dBdt[i] <- dnorm(term) + coef(x)[i] * mean[i] * (-term*dnorm(term))
        
        v.tau <- t(dBdt) %*% vcov %*% dBdt
      }
      
      return(cbind(tau, sqrt(v.tau)))
    })
  }
  
  if(returnvcov==FALSE){
    out <- t(out)
    colnames(out) <- c("Effect", "SE")
    rownames(out) <- names(coef(x))
    
    return(out)
  }
  else return(vcov)
}

## Import Malesky's data
rep.data <- read.dta("..//Malesky and Schuler/NA_January10.dta")
rep.data <- dplyr::filter(rep.data, prov!="") %>%
  mutate(centralnominated=as.numeric(centralnominated=="Yes"))


## Table 2
# replicate
table.2.1.rep <- glm(candidates_district ~ centralnominated + politburo, 
                     data=rep.data, family=binomial(link="probit"))
mfx(table.2.1.rep)

table.2.2.rep <- glm(candidates_district ~ centralnominated + politburo + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=rep.data, family=binomial(link="probit"))
mfx(table.2.2.rep) # as K increases, Stata and results no longer match perfectly
probitmfx(table.2.2.rep$formula, data=rep.data, robust=TRUE) # using probitmfx() yields similar results but still no perfect match

table.2.3.rep <- lm(candidates_district ~ centralnominated + politburo + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame + factor(prov_id), 
                    data=rep.data)
mfx(table.2.3.rep)[c(2:9),]

table.2.4.rep <- lm(power_total ~ centralnominated + politburo, 
                    data=rep.data)
mfx(table.2.4.rep)

table.2.5.rep <- lm(power_total ~ centralnominated + politburo+ male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame, 
                    data=rep.data)
mfx(table.2.5.rep) # identical results except for the intercept

table.2.6.rep <- lm(power_total ~ centralnominated + politburo+ male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame + factor(prov_id), 
                    data=rep.data)
mfx(table.2.6.rep)[c(2:9),]

# new 2007 data - different values but doesnt seem to have many substantial differences
table.2.1.new <- glm(candidates_district ~ centralnominated + politburo, 
                     data=candidates2007, family=binomial(link="probit"))
mfx(table.2.1.new)
mfx(table.2.1.rep)

table.2.2.new <- glm(candidates_district ~ centralnominated + politburo + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=candidates2007, family=binomial(link="probit"))
mfx(table.2.2.new) # as K increases, Stata and results no longer match perfectly
mfx(table.2.2.rep)

table.2.3.new <- lm(candidates_district ~ centralnominated + politburo + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame + factor(prov), 
                    data=candidates2007)
mfx(table.2.3.new)[c(2:9),]
mfx(table.2.3.rep)[c(2:9),]

table.2.4.new <- lm(power_total ~ centralnominated + politburo, 
                    data=candidates2007)
mfx(table.2.4.new)
mfx(table.2.4.rep)

table.2.5.new <- lm(power_total ~ centralnominated + politburo+ male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame, 
                    data=candidates2007)
mfx(table.2.5.new)
mfx(table.2.5.rep)

table.2.6.new <- lm(power_total ~ centralnominated + politburo+ male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame + factor(prov), 
                    data=candidates2007)
mfx(table.2.6.new)[c(2:9),]
mfx(table.2.6.rep)[c(2:9),]

## Table 3

# replicate: none of the standard errors are actually clustered...
table.3.1.rep <- glm(result ~ power_total + candidates_district, 
                     data=rep.data, family=binomial(link="probit"))
mfx(table.3.1.rep)  # robust SEs slightly smaller than original results
                    # note also that Table 3 mentions cluster but 
                    # in .do file cluster() is not specified

table.3.5.rep <- lm(percentage ~ power_total + candidates_district, 
                    data=rep.data)
mfx(table.3.5.rep)

table.3.2.rep <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=rep.data, family=binomial(link="probit"))
mfx(table.3.2.rep)

table.3.6.rep <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame, 
                    data=rep.data)
mfx(table.3.6.rep)

table.3.3.rep <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business, 
                     data=rep.data, family=binomial(link="probit")) 
mfx(table.3.3.rep)[c(2:12),]

table.3.7.rep <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business, 
                    data=rep.data) 
mfx(table.3.7.rep)[c(2:12),]

table.3.4.rep <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business + factor(prov_id), 
                     data=rep.data, family=binomial(link="probit")) 
mfx(table.3.4.rep)[c(2:12),] # SEs slightly off

table.3.8.rep <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + factor(prov_id), 
                    data=rep.data) 
mfx(table.3.8.rep)[c(2:12),]

table.3.9.rep <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + 
                      politburo + centralcommittee + centralnominated + factor(prov_id), 
                    data=rep.data) 
mfx(table.3.9.rep)[c(2:16),]

# new 2007 data

table.3.1.new <- glm(result ~ power_total + candidates_district, 
                     data=candidates2007, family=binomial(link="probit"))
mfx(table.3.1.new)  
mfx(table.3.1.rep)

table.3.5.new <- lm(percentage ~ power_total + candidates_district, 
                    data=candidates2007)
mfx(table.3.5.new)
mfx(table.3.5.rep)

table.3.2.new <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=candidates2007, family=binomial(link="probit"))
mfx(table.3.2.new)
mfx(table.3.2.rep)

table.3.6.new <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame, 
                    data=candidates2007)
mfx(table.3.6.new)
mfx(table.3.6.rep)

table.3.3.new <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business, 
                     data=candidates2007, family=binomial(link="probit")) 
mfx(table.3.3.new)[c(2:12),]
mfx(table.3.3.rep)[c(2:12),]

table.3.7.new <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business, 
                    data=candidates2007) 
mfx(table.3.7.new)[c(2:12),]
mfx(table.3.7.rep)[c(2:12),]

table.3.4.new <- glm(result ~ power_total + candidates_district + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business + factor(prov), 
                     data=candidates2007, family=binomial(link="probit")) 
mfx(table.3.4.new)[c(2:12),]
mfx(table.3.4.rep)[c(2:12),]

table.3.8.new <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + factor(prov), 
                    data=candidates2007) 
mfx(table.3.8.new)[c(2:12),] # weakn effect for party
mfx(table.3.8.rep)[c(2:12),]

table.3.9.new <- lm(percentage ~ power_total + candidates_district + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + 
                      politburo + centralcommittee + centralnominated + factor(prov), 
                    data=candidates2007) 
mfx(table.3.9.new)[c(2:16),] # weaken effect for party $ business, centralnominated, strengthen for central committee
mfx(table.3.9.rep)[c(2:16),]

## Table 4

# replicate: none of the standard errors are actually clustered...
table.4.1.rep <- glm(result ~ candidates_district + power_total + party +
                       incumbencyterms + male + religion_any + age + as.numeric(degree) + 
                       south + trans_rev, 
                     data=rep.data %>% filter(centralnominated=="Yes", !is.na(stt)), 
                     family=binomial(link="probit"))
coeftest(table.4.1.rep, vcov=mfx(table.4.1.rep, cluster=table.4.1.rep$data$prov_id, returnvcov=TRUE))

table.4.4.rep <- lm(percentage ~ candidates_district + power_total + party +
                       incumbencyterms + male + religion_any + age + as.numeric(degree) + 
                       south + trans_rev, 
                     data=rep.data %>% filter(centralnominated=="Yes", !is.na(stt)))
mfx(table.4.4.rep, cluster=rep.data %>% 
      filter(centralnominated=="Yes", 
             !is.na(stt),
             !is.na(percentage)) %>% 
      dplyr::select(prov_id) %>%
      unlist)

# new 2007 data
table.4.1.new <- glm(result ~ candidates_district + power_total + party +
                       incumbencyterms + male + religion_any + age + as.numeric(degree) + 
                       south + trans_rev, 
                     data=candidates2007 %>% filter(centralnominated==1), 
                     family=binomial(link="probit"))
coeftest(table.4.1.rep, 
         vcov=mfx(table.4.1.rep, cluster=table.4.1.rep$data$prov_id, returnvcov=TRUE))
coeftest(table.4.1.new, 
         vcov=mfx(table.4.1.new, cluster=table.4.1.new$data$prov, returnvcov=TRUE))
                                          # TRANSFER is less significant!
# clustered SEs slightly smaller than original results

table.4.4.new <- lm(percentage ~ candidates_district + power_total + party +
                       incumbencyterms + male + religion_any + age + as.numeric(degree) + 
                       south + trans_rev, 
                     data=candidates2007 %>% filter(centralnominated==1))
coeftest(table.4.4.rep, 
         vcov=mfx(table.4.4.rep, cluster=rep.data %>% 
                    filter(centralnominated=="Yes", 
                           !is.na(stt),
                           !is.na(percentage)) %>% 
                    dplyr::select(prov_id) %>%
                    unlist, returnvcov=T))
coeftest(table.4.4.new, 
         vcov=mfx(table.4.4.new, cluster=candidates2007 %>% 
                    filter(centralnominated==1, 
                           !is.na(percentage)) %>% 
                    dplyr::select(prov) %>%
                    unlist, returnvcov=T)) # TRANSFER no longer significant!!
