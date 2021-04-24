#### THIS FILE CONTAINS THE MAIN MODELS DONE ON DIFFERENCE IN PERFORMANCE ####

library(gsynth)
library(panelView)
library(multiwayvcov)
library(lmtest)
library(ri)
library(grid)
library(gridExtra)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("~/vietnam_elections/Data/Working Data")

source("../../Code/Spring 2021/Merge_All.R")

#### Linear regressions results for 2016 elections ####

dat_lme_share <- plan %>%
  mutate(share.dif = share.central.mean.2011 - share.central.mean.2016) %>%
  filter(year < 2019 & year > 2012) %>%
  #filter(defeat.2011 != 1) %>%
  #filter(defeat.true.2016!=0 | closewin.true.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong") %>%
  drop_na(net.trans.log, net.trans.lag)

## one-year change

# 2016 only
dat_2016_share_1 <- dat_lme_share  %>%
  mutate(share.dif = share.dif*(year == 2017)) %>%
  filter(year < 2018)

# Just difference in share
lm_2016_share_1a <- lm(net.trans.change.log ~ share.dif  +
                   factor(prov) + factor(year),
                 data = dat_2016_share_1) # just lm
summary(lm_2016_share_1a)
vcov_2016_share_1a <- cluster.vcov(lm_2016_share_1a, cluster = ~ prov)
coeftest(lm_2016_share_1a, vcov = vcov_2016_share_1a)

# Difference in share squared
lm_2016_share_1b <- lm(net.trans.change.log ~ share.dif + I(share.dif^2) + 
                   factor(prov) + factor(year),
                 data = dat_2016_share_1) # just lm
summary(lm_2016_share_1b)
vcov_2016_share_1b <- cluster.vcov(lm_2016_share_1b, cluster = ~ prov)
coeftest(lm_2016_share_1b, vcov = vcov_2016_share_1b)

# Difference in share squared + defeats
lm_2016_share_1c <- lm(net.trans.change.log ~ share.dif + I(share.dif^2) + defeat.true  +
                         factor(prov) + factor(year),
                 data = dat_2016_share_1) # just lm
summary(lm_2016_share_1c)
vcov_2016_share_1c <- cluster.vcov(lm_2016_share_1c, cluster = ~ prov)
coeftest(lm_2016_share_1c, vcov = vcov_2016_share_1c)

## persistent change

# 2016 only 
dat_2016_share_p <- dat_lme_share %>%
  mutate(share.dif = share.dif*(year >= 2017)) 

# Just difference in share
lm_2016_share_pa <- lm(net.trans.change.log ~ share.dif + 
                   factor(prov) + factor(year),
                 data = dat_2016_share_p) # just lm
summary(lm_2016_share_pa)
vcov_2016_share_pa <- cluster.vcov(lm_2016_share_pa, cluster = ~ prov)
coeftest(lm_2016_share_pa, vcov = vcov_2016_share_pa)

# Difference in share + defeats
lm_2016_share_pb <- lm(net.trans.change.log ~ share.dif + I(share.dif^2) + 
                   factor(prov) + factor(year),
                 data = dat_2016_share_p) # just lm
summary(lm_2016_share_pb)
vcov_2016_share_pb <- cluster.vcov(lm_2016_share_pb, cluster = ~ prov)
coeftest(lm_2016_share_pb, vcov = vcov_2016_share_pb)

# Interaction Term
lm_2016_share_pc <- lm(net.trans.change.log ~ share.dif + I(share.dif^2) + defeat.true + 
                         factor(prov) + factor(year),
                 data = dat_2016_share_p) # just lm
summary(lm_2016_share_pc)
vcov_2016_share_pc <- cluster.vcov(lm_2016_share_pc, cluster = ~ prov)
coeftest(lm_2016_share_pc, vcov = vcov_2016_share_pc)

## regression table
stargazer(lm_2016_share_1b, lm_2016_share_1c,
          lm_2016_share_pb, lm_2016_share_pc,
          se = list(sqrt(diag(vcov_2016_share_1b)), sqrt(diag(vcov_2016_share_1c)),
                    sqrt(diag(vcov_2016_share_pb)), sqrt(diag(vcov_2016_share_pc))),
          title = "Estimated effects of vote share swings on central transfers from linear fixed effects models",
          label = "tab:lfe_share",
          style = "apsr",
          out = "../../figure/200218_reg_table_share.tex",
          column.labels = c("Instantaneous Effect", "Persistent Effect"),
          column.separate = c(2,2),
          covariate.labels = c("Vote Share Dif.", "Vote Share Dif.\\textsuperscript{2}", "Election Defeat"),
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          digits = 3,
          keep = c("share.dif$", "share.dif", "defeat.true$"),
          add.lines=list(c("Province FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                         c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("n", "rsq"),
          table.layout = "=c#-t-a-s=n")

## Effect plot
library(interplot)
interplot_share_1b <- interplot(m = lm_2016_share_1b, var1 = "share.dif", var2 = "share.dif", 
                                var2_dt = lm_2016_share_1b$model %>% filter(`factor(year)`=="2017") %>% pull(share.dif),
                                hist = TRUE, xmin = -20, xmax = 17, sims = 500) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-3,2))

interplot_share_1c <- interplot(m = lm_2016_share_1c, var1 = "share.dif", var2 = "share.dif", 
                                var2_dt = lm_2016_share_1c$model %>% filter(`factor(year)`=="2017") %>% pull(share.dif),
                                hist = TRUE, xmin = -20, xmax = 17, sims = 500) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-3,2))

interplot_share_pb <- interplot(m = lm_2016_share_pb, var1 = "share.dif", var2 = "share.dif", 
                                var2_dt = lm_2016_share_pb$model %>% filter(`factor(year)`=="2017") %>% pull(share.dif),
                                hist = TRUE, xmin = -20, xmax = 17, sims = 500) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 3") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-3.5,2))

interplot_share_pc <- interplot(m = lm_2016_share_pc, var1 = "share.dif", var2 = "share.dif", 
                                var2_dt = lm_2016_share_pc$model %>% filter(`factor(year)`=="2017") %>% pull(share.dif),
                                hist = TRUE, xmin = -20, xmax = 17, sims = 500) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 4") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(-3.5,2))

lay <- rbind(c(1,2,2),
             c(1,3,4),
             c(1,5,5),
             c(1,6,7),
             c(NA,8,8))

lfe_share <- grid.arrange(layout_matrix = lay,
             heights = c(1,4,1,4,1),
             widths = c(.5,4,4),
             grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
             textGrob("Instantaneous Effect"),
             interplot_share_1b,
             interplot_share_1c,
             textGrob("Persistent Effect"),
             interplot_share_pb,
             interplot_share_pc,
             textGrob("Avg. Central Candidate Vote Share 2011 - Avg. Central Candidate Vote Share 2016"))

ggsave("../../figure/200218_lfe_share.png", plot = lfe_share, width = 8, height = 8)

