library(ri)

setwd("D:/Dropbox (MIT)/Documents/Works/G1 17.830 Empirical Methods in Political Economy/Project/Data/Working Data/")

source("../../Code/SYP/Merge_All.R")

#######

### helper function

ritest <- function(data, year, variable){
  y <- unlist(data[data$year==year, variable])
  treat <- unlist(data[data$year==year, "defeat"])
  
  omni.ate(y, treat, genperms(treat))    
}

rifull <- function(model.step1, model.step2, t, data=plan.2007 %>% filter(year==t)) {
  perm <- genperms(data$defeat)
  data$y <- resid(model.step1)
  
  beta.actual <- coef(summary(model.step2))["defeat", 1]
  p.actual <- coef(summary(model.step2))["defeat", 4]
  
  for(i in 1:ncol(perm)) {
    data$defeat <- perm[,i]
    
    fit <- lm(y ~ defeat, data=data)
    
    if(!is.na(coef(fit)["defeat"])){
      beta[i] <- coef(summary(fit))["defeat", 1]
      p[i] <- coef(summary(fit))["defeat", 4]
    } else {
      beta[i] <- NA
      b[i] <- NA
    }
  }
  
  beta.greater <- mean(beta > beta.actual, na.rm=T)
  beta.smaller <- mean(beta < beta.actual, na.rm=T)
  p.smaller <- mean(p < p.actual, na.rm=T)
  
  return(c(beta.actual=beta.actual,
           beta.greater=beta.greater,
           beta.smaller=beta.smaller,
           p.actual=p.actual,
           p.smaller=p.smaller))
}

##### 2007 #####

### Import files

plan.2007 <- candidates2007 %>%
  dplyr::select(prov,defeat,closewin) %>%
  plyr::join(plan, by="prov") %>%
  dplyr::group_by(prov, year) %>%
  dplyr::filter(defeat==1 | closewin==1) %>%
  dplyr::summarise_each(funs(max))

final.2007 <- candidates2007 %>%
  dplyr::select(prov,defeat,closewin) %>%
  plyr::join(plan, by="prov") %>%
  dplyr::group_by(prov, year) %>%
  dplyr::filter(defeat==1 | closewin==1) %>%
  dplyr::summarise_each(funs(max))

### Budget plans

RI2007.plan <- list()

## One-year change (2007-2008)
# net transfer, log change
RI2007.plan[[1]] <- ritest(plan.2007, 2008, "net.trans.change.log")
  
# net transfer, pct change 
RI2007.plan[[2]] <- ritest(plan.2007, 2008, "net.trans.change.pct")

# project transfer, log change
RI2007.plan[[3]] <- ritest(plan.2007, 2008, "project.trans.change.log")

# project transfer, pct change 
RI2007.plan[[4]] <- ritest(plan.2007, 2008, "project.trans.change.pct")

# transfer as ratio of rev, change
RI2007.plan[[5]] <- ritest(plan.2007, 2008, "trans_rev.change")

# transfer as ratio of rev, pct change 
RI2007.plan[[6]] <- ritest(plan.2007, 2008, "trans_rev.change.pct")

# revenue, log change
RI2007.plan[[7]] <- ritest(plan.2007, 2008, "total.rev.change.log")

# revenue, pct change 
RI2007.plan[[8]] <- ritest(plan.2007, 2008, "total.rev.change.pct")

## Two-year change
# net transfer, log change
RI2007.plan[[9]] <- ritest(plan.2007, 2009, "net.trans.change2.log")

# net transfer, pct change 
RI2007.plan[[10]] <- ritest(plan.2007, 2009, "net.trans.change2.pct")

# project transfer, log change
RI2007.plan[[11]] <- ritest(plan.2007, 2009, "project.trans.change2.log")

# project transfer, pct change 
RI2007.plan[[12]] <- ritest(plan.2007, 2009, "project.trans.change2.pct")

# transfer as ratio of exp, change
RI2007.plan[[13]] <- ritest(plan.2007, 2009, "trans_rev.change2")

# transfer as ratio of exp, pct change 
RI2007.plan[[14]] <- ritest(plan.2007, 2009, "trans_rev.change2.pct")

# revenue, log change
RI2007.plan[[15]] <- ritest(plan.2007, 2009, "total.rev.change2.log")

# revenue, pct change 
RI2007.plan[[16]] <- ritest(plan.2007, 2009, "total.rev.change2.pct")

## Robustness check #1: One year 2006-2007
# net transfer, log change
RI2007.plan[[17]] <- ritest(plan.2007, 2007, "net.trans.change.log")

# net transfer, pct change 
RI2007.plan[[18]] <- ritest(plan.2007, 2007, "net.trans.change.pct")

# project transfer, log change
RI2007.plan[[19]] <- ritest(plan.2007, 2007, "project.trans.change.log")

# project transfer, pct change 
RI2007.plan[[20]] <- ritest(plan.2007, 2007, "project.trans.change.pct")

# transfer as ratio of rev, change
RI2007.plan[[21]] <- ritest(plan.2007, 2007, "trans_rev.change")

# transfer as ratio of rev, pct change 
RI2007.plan[[22]] <- ritest(plan.2007, 2007, "trans_rev.change.pct")

# revenue, log change
RI2007.plan[[23]] <- ritest(plan.2007, 2007, "total.rev.change.log")

# revenue, pct change 
RI2007.plan[[24]] <- ritest(plan.2007, 2007, "total.rev.change.pct")

## Robustness check #2: drop Hanoi, Ho Chi Minh City
# net transfer, log change
RI2007.plan[[25]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "net.trans.change.log")

# net transfer, pct change 
RI2007.plan[[26]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "net.trans.change.pct")

# project transfer, log change
RI2007.plan[[27]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "project.trans.change.log")

# project transfer, pct change 
RI2007.plan[[28]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "project.trans.change.pct")

# transfer as ratio of rev, change
RI2007.plan[[29]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "trans_rev.change")

# transfer as ratio of rev, pct change 
RI2007.plan[[30]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "trans_rev.change.pct")

# revenue, log change
RI2007.plan[[31]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "total.rev.change.log")

# revenue, pct change 
RI2007.plan[[32]] <- ritest(plan.2007 %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))), 
                            2008, "total.rev.change.pct")


### Manual RI
model.step1 <- model.step2 <- list()

## one-year change: 2007-2008
# actual results
model.step1[[1]] <- lm(neglog(net.trans) ~ neglog(net.trans.lag) + total.rev.change.log, 
                   data=plan.2007 %>% filter(year==2008))
model.step2[[1]] <- lm(resid(model.step1[[1]]) ~ defeat, 
                  data=plan.2007 %>% filter(year==2008))
summary(model.step2[[1]])

# RI
RI2007.plan[[33]] <- rifull(model.step1[[1]], model.step2[[1]], 2008)

## two-year change: 2007-2009
# actual results
model.step1[[2]] <- lm(neglog(net.trans) ~ neglog(net.trans.lag2) + total.rev.change2.log, 
                  data=plan.2007 %>% filter(year==2009))
model.step2[[2]] <- lm(resid(model.step1[[2]]) ~ defeat, 
                  data=plan.2007 %>% filter(year==2009))
summary(model.step2[[2]])

# RI
RI2007.plan[[34]] <- rifull(model.step1[[2]], model.step2[[2]], 2009)

## Robustness check #1: One year 2006-2007
# actual results
model.step1[[3]] <- lm(neglog(net.trans) ~ neglog(net.trans.lag) + total.rev.change.log, 
                       data=plan.2007 %>% filter(year==2007))
model.step2[[3]] <- lm(resid(model.step1[[3]]) ~ defeat, 
                       data=plan.2007 %>% filter(year==2007))
summary(model.step2[[3]])

# RI
RI2007.plan[[35]] <- rifull(model.step1[[3]], model.step2[[3]], 2007)

## Robustness check #2: drop Ha Noi and Ho Chi Minh City
# actual results
model.step1[[4]] <- lm(neglog(net.trans) ~ neglog(net.trans.lag) + total.rev.change.log, 
                       data=plan.2007 %>% filter(year==2008) %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))))
model.step2[[4]] <- lm(resid(model.step1[[4]]) ~ defeat, 
                       data=plan.2007 %>% filter(year==2008) %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))))
summary(model.step2[[4]])

# RI
RI2007.plan[[36]] <-  rifull(model.step1[[4]], model.step2[[4]], 2008, 
       data=plan.2007 %>% filter(year==2008) %>% filter(!(prov %in% c("Ha Noi", "Ho Chi Minh City"))))

