library(foreign)
library(sandwich)
library(lmtest)
library(mfx)
library(mlogit)

setwd("D:/Dropbox (MIT)/Documents/Works/G1 17.830 Empirical Methods in Political Economy/Project/Data/Malesky and Schuler/")

source("../../SYP/Code/Analyze_Replication.R")

## Rename variables for ease of use
candidates2011 <- candidates2011 %>%
  mutate(candidates_district_ordinal = as.factor(candidates_district)) %>%
  mutate(candidates_district_binary = as.numeric(candidates_district==2)) 

## Table 2

# Model 1
table.2.1.2011.a <- polr(candidates_district_ordinal ~ centralnominated + politburo, 
                     data=candidates2011, method="probit")
summary(table.2.1.2011.a)

table.2.1.2011.b <- glm(candidates_district_binary ~ centralnominated + politburo, 
                        data=candidates2011, family=binomial(link="probit"))
summary(table.2.1.2011.b)

summary(table.2.1.2011.a)
summary(table.2.1.2011.b)
summary(table.2.1.new)
summary(table.2.1.rep)

coeftest(table.2.1.2011.a)
coeftest(table.2.1.2011.b)

# Model 2
table.2.2.2011.a <- polr(candidates_district_ordinal ~ centralnominated + politburo + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=candidates2011, method="probit")
table.2.2.2011.b <- glm(candidates_district_binary ~ centralnominated + politburo + male + minority +
                          religion_any + as.numeric(degree) + age + que_districtsame, 
                        data=candidates2011, family=binomial(link="probit"))

summary(table.2.2.2011.a)
summary(table.2.2.2011.b)
summary(table.2.2.new)
summary(table.2.2.rep)

coeftest(table.2.2.2011.a)
coeftest(table.2.2.2011.b)

# Model 3

table.2.3.2011.a <- polr(candidates_district_ordinal ~ centralnominated + politburo + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame + factor(prov), 
                    data=candidates2011, method="probit")
table.2.3.2011.b <- glm(candidates_district_binary ~ centralnominated + politburo + male + minority +
                          religion_any + as.numeric(degree) + age + que_districtsame + factor(prov), 
                        data=candidates2011, family=binomial(link="probit"))
table.2.3.2011.c <- lm(candidates_district ~ centralnominated + politburo + male + minority +
                          religion_any + as.numeric(degree) + age + que_districtsame + factor(prov), 
                        data=candidates2011)

summary(table.2.3.2011.a)
summary(table.2.3.2011.b)
summary(table.2.3.2011.c)
summary(table.2.3.new)
summary(table.2.3.rep)

coeftest(table.2.3.2011.a)
coeftest(table.2.3.2011.b)
coeftest(table.2.3.2011.c)
coeftest(table.2.3.new)
coeftest(table.2.3.rep)


## Table 3

table.3.1.2011 <- glm(result ~ power_total + candidates_district_ordinal, 
                     data=candidates2011, family=binomial(link="probit"))
mfx(table.3.1.2011)  
mfx(table.3.1.new)
mfx(table.3.1.rep)

table.3.5.2011 <- lm(percentage ~ power_total + candidates_district_ordinal, 
                    data=candidates2011)
mfx(table.3.5.2011)
mfx(table.3.5.rep)

table.3.2.2011 <- glm(result ~ power_total + candidates_district_ordinal + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame, 
                     data=candidates2011, family=binomial(link="probit"))
mfx(table.3.2.2011)
mfx(table.3.2.rep)

table.3.6.2011 <- lm(percentage ~ power_total + candidates_district_ordinal + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame, 
                    data=candidates2011)
mfx(table.3.6.2011)
mfx(table.3.6.rep)

table.3.3.2011 <- glm(result ~ power_total + candidates_district_ordinal + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business, 
                     data=candidates2011, family=binomial(link="probit")) 
mfx(table.3.3.2011)[c(2:12),]
mfx(table.3.3.rep)[c(2:12),]

table.3.7.2011 <- lm(percentage ~ power_total + candidates_district_ordinal + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business, 
                    data=candidates2011) 
mfx(table.3.7.2011)[c(2:12),]
mfx(table.3.7.rep)[c(2:12),]

table.3.4.2011 <- glm(result ~ power_total + candidates_district_ordinal + male + minority +
                       religion_any + as.numeric(degree) + age + que_districtsame +
                       party + incumbencyterms + locallegislature + business + factor(prov), 
                     data=candidates2011, family=binomial(link="probit")) 
mfx(table.3.4.2011)[c(2:12),]
mfx(table.3.4.rep)[c(2:12),]

table.3.8.2011 <- lm(percentage ~ power_total + candidates_district_ordinal + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + factor(prov), 
                    data=candidates2011) 
mfx(table.3.8.2011)[c(2:12),] 
mfx(table.3.8.rep)[c(2:12),]

table.3.9.2011 <- lm(percentage ~ power_total + candidates_district_ordinal + male + minority +
                      religion_any + as.numeric(degree) + age + que_districtsame +
                      party + incumbencyterms + locallegislature + business + 
                      politburo + centralcommittee + centralnominated + factor(prov), 
                    data=candidates2011) 
mfx(table.3.9.2011)[c(2:16),]
mfx(table.3.9.rep)[c(2:16),]