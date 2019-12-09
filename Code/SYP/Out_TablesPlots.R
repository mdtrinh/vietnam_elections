library(lme4)
library(ggplot2)
library(coefplot)
library(stargazer)
library(gridExtra)
library(xtable)

setwd("D:/Dropbox (MIT)/Documents/Works/G1 17.830 Empirical Methods in Political Economy/Project/Data/Malesky and Schuler/")
load(".RData")

### Result Figure 1: Original and Independently-coded data for Table 2
f1.1 <- multiplot(table.2.1.rep, 
                  table.2.1.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 1")

f1.2 <- multiplot(table.2.2.rep, 
                  table.2.2.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 2")

f1.3 <- multiplot(table.2.3.rep, 
                  table.2.3.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 3")

f1.4 <- multiplot(table.2.4.rep, 
                  table.2.4.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 4")

f1.5 <- multiplot(table.2.5.rep, 
                  table.2.5.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 5")

f1.6 <- multiplot(table.2.6.rep, 
                  table.2.6.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("centralnominated", "politburo"),
                  newNames=c(centralnominated="Centrally Nominated", 
                             politburo="Politburo"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 6")

grid.arrange(f1.1, f1.2, f1.3, f1.4, f1.5, f1.6, ncol=2)

### Result Figure 2: Original and Independently-coded data for Table 2
f2.1 <- multiplot(table.3.1.rep, 
                  table.3.1.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 1")

f2.3 <- multiplot(table.3.3.rep, 
                  table.3.3.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total", "party", "business"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District",
                             party="Party membership",
                             business="Business owner"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 3")

f2.4 <- multiplot(table.3.4.rep, 
                  table.3.4.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total", "party", "business"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District",
                             party="Party membership",
                             business="Business owner"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 4")

f2.5 <- multiplot(table.3.5.rep, 
                  table.3.5.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 5")

f2.8 <- multiplot(table.3.8.rep, 
                  table.3.8.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total", "party", "business"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District",
                             party="Party membership",
                             business="Business owner"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 8")

f2.9 <- multiplot(table.3.9.rep, 
                  table.3.9.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("candidates_district", "power_total", "party", "business", "centralnominated"),
                  newNames=c(candidates_district="Competitiveness", 
                             power_total="5/3 District",
                             party="Party membership",
                             business="Business owner",
                             centralnominated="Central Nominee"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 9")

grid.arrange(f2.3, f2.4, f2.8, f2.9, ncol=2)

### Result Figure 3: Original and Independently-coded data for Table 4
f3.1 <- multiplot(table.4.1.rep, 
                  table.4.1.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("trans_rev"),
                  newNames=c(trans_rev="Transfers"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 1")

f3.4 <- multiplot(table.4.4.rep, 
                  table.4.4.new,
                  names=c("Original data", "Independently-coded"),
                  coefficients=c("trans_rev"),
                  newNames=c(trans_rev="Transfers"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  guides(color=FALSE) +
  theme_bw() +
  ggtitle("Model 4")

grid.arrange(f3.1, f3.4, ncol=2)

## Result Table 1: Who Ran in 3-to-2 districts
stargazer(table.2.1.2011.a, table.2.2.2011.a,table.2.3.2011.a,
          table.2.1.2011.b, table.2.2.2011.b,table.2.3.2011.b,
          keep=c("centralnominated", "politburo"),
          covariate.labels=c("Centrally Nominated",
                             "Politburo"),
          dep.var.labels=c("Ordinal (2/1, 5/3, 3/2)", "Binary (3/2)"),
          dep.var.caption="Candidates-to-seat Ratio",
          add.lines=list(c("Province FE", "No", "No", "Yes", "No", "No", "Yes"), 
                         c("Demographic Variables", "No", "Yes", "Yes","No", "Yes", "Yes")),
          title="Who Ran in 3-to-2 Districts in 2011?",
          keep.stat = c("n"),
          label="table1",
          table.placement = "!htb",
          no.space=T,
          notes="Intercept not shown. Robust standard errors in parentheses")

## Result Figure 5: Effectiveness of Easy District
f5.1 <- multiplot(table.3.1.2011, 
                  table.3.2.2011,
                  table.3.3.2011,
                  table.3.4.2011,
                  names=c("Baseline", "Demographic", "Political", "Province FEs"),
                  coefficients=c("candidates_district_ordinal1",
                                 "candidates_district_ordinal2"),
                  newNames=c(candidates_district_ordinal1="5/3 District", 
                             candidates_district_ordinal2="3/2 District"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  theme_bw() +
  ggtitle("Election to National Assembly")

f5.2 <- multiplot(table.3.5.2011, 
                  table.3.6.2011,
                  table.3.7.2011,
                  table.3.8.2011,
                  names=c("Baseline", "Demographic", "Political", "Province FEs"),
                  coefficients=c("candidates_district_ordinal1",
                                 "candidates_district_ordinal2"),
                  newNames=c(candidates_district_ordinal1="5/3 District", 
                             candidates_district_ordinal2="3/2 District"),
                  innerCI=1.96,
                  outerCI=0,
                  xlab="",
                  ylab="") +
  theme_bw() +
  ggtitle("Vote Shares")

grid.arrange(f5.1, f5.2, nrow=2)

## Result Table 2
table2 <- rbind(RI2007.plan[[33]], 
                RI2007.plan[[34]], 
                RI2007.plan[[35]], 
                RI2007.plan[[36]])[,1:3]
rownames(table2) <- c(1:4)
colnames(table2) <- c("\\beta_0", 
                      "P(\\beta^* > \\beta_0)", 
                      "P(\\beta^* > \\beta_0)")
print(xtable(table2,
             caption = "Distribution of mean differences from RI",
             label = "table2",
             digits=3),
      sanitize.colnames.function = identity)

stargazer(rbind(RI2007.plan[[33]], 
             RI2007.plan[[34]], 
             RI2007.plan[[35]], 
             RI2007.plan[[36]])[,1:3],
       caption = "Distribution of mean differences from RI",
       label = "table2")
