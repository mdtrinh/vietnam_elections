#### THIS FILE CONTAINS ILLUSTRATION FOR ALL MODELs THAT WILL APPEAR IN THE FINAL PAPER ####

## i.e. it runs Models_Final.R and then output well designed graphics for these models ##

library(stargazer)
library(grid)
library(gridExtra)
library(ggplot2)
library(xtable)
library(kableExtra)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("C:/Users/Minh Trinh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Winter 2019/Models_Final.R")
source("../../Code/Winter 2019/Models_Mechanism.R")
source("../../Code/Winter 2019/Balance_Final.R")
source("../../Code/Winter 2019/Models_DigitTests.R")
source("../../Code/Winter 2019/Illustration_Functions.R")

#### Some statistics to include in the paper

## percentage of leaders serving in their own hometown in 2015
leaders %>% 
  ungroup() %>% 
  filter(year == 2015) %>% 
  select(id, name, position, province, hometown, nha_que, nha_que2) %>% 
  summarize(hometown = mean(hometown, na.rm = T))

## average growth in central transfers
plan %>% 
  filter(year < 2018 & year > 2012) %>%
  filter(defeat.2016!=0 | closewin.2016!=0) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  drop_na(net.trans.log, net.trans.lag) %>%
  group_by(prov) %>%
  summarise(net.trans.change = mean(net.trans.change, na.rm = T),
            total.exp = mean(total.exp, na.rm = T)) %>%
  arrange(net.trans.change) %>%
  as.data.frame

## Candidates who lose despite being helped
candidates2016 %>% 
  filter(centralnominated == 1, defeat == 1) %>% 
  select(prov, district, name, num.candidates, num.seats, power_total)

#### Balance Tables

balance_table <- cbind(t(balance_lme), t(balance_rdd))
rownames(balance_table) <- c("Budget Revenue (Billions of VND)", 
                             "Budget Expenditure (Billions of VND)",
                             "Number of Seats",
                             "Number of Candidates",
                             "Number of Central Candidates",
                             "Surface Area (Thousands Km$^2$)",
                             "Population (Thousands)",
                             "Population Density (Thousands/Km$^2$)",
                             "Provincial GDP (Billions of VND)",
                             "Average Monthly Income (Thousands of VND)",
                             "Employment Rate Among >15yo Population ($\\%$)",
                             "Share of Agriculture Land ($\\%$)",
                             "Infant (<5yo) Mortality Rate (\\textperthousand)",
                             "Number of Beds in Public Hospitals",
                             "Number of Schools",
                             "Number of Primary Schools")

colnames(balance_table) <- c("Control Mean ($N = 11$)", "Treated Mean ($N = 4$)", "Std. Diff. in Means", "RI Std. Error", "RI p-value", "OLS Std. Error", "OLS p-value",
                             "Control Mean ($N = 11$)", "Treated Mean ($N = 4$)", "Std. Diff. in Means", "RI Std. Error", "RI p-value", "OLS Std. Error", "OLS p-value")

# Experiment with kable()

sink("../../figure/191210_table_balance.tex")
kable(balance_table[,c(1, 2, 3, 5, 7,
                       8, 9, 10, 12, 14)],
      format = "latex",
      digits = c(1,1,2,2,2,
                 1,1,2,2,2),
      align = c("r","r","c","c","c",
                "r","r","c","c","c"),
      booktabs = T,
      escape = FALSE,
      caption = "Balance between control and treatment provinces based on 2015 data",
      label = "balance") %>%
  add_header_above(c(" " = 1, 
                     "Linear Fixed Effects Sample" = 5, 
                     "Local Randomization RDD Sample" = 5), 
                   align = "c") %>%
  column_spec(1, width = "14em") %>%
  column_spec(c(2:11), width = "4.1em") %>%
  row_spec(0, align = "c") %>%
  pack_rows("Budget", 1, 2, indent = T) %>%
  pack_rows("Election", 3, 5, indent = T) %>%
  pack_rows("Structural Condition", 6, 8, indent = T) %>%
  pack_rows("Economic", 9, 12, indent = T) %>%
  pack_rows("Public Goods", 13, 16, indent = T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                position = "c") %>%
  landscape()
sink()

#### Regression table for main regression results

stargazer(lm_2016_1a, lm_2016_1b, lm_2016_1c,
          lm_2016_pa, lm_2016_pb, lm_2016_pc,
          se = list(sqrt(diag(vcov_2016_1a)), sqrt(diag(vcov_2016_1b)), sqrt(diag(vcov_2016_1c)),
                    sqrt(diag(vcov_2016_pa)), sqrt(diag(vcov_2016_pb)), sqrt(diag(vcov_2016_pc))),
          title = "Estimated treatment effects on central transfers from linear fixed effects models",
          label = "tab:lfe_main",
          style = "apsr",
          out = "../../figure/191210_reg_table.tex",
          column.labels = c("Instantaneous Effect", "Persistent Effect"),
          column.separate = c(3,3),
          covariate.labels = c("Treatment Effect"),
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          digits = 3,
          keep = "defeat.true$",
          keep.stat = c("n", "rsq"),
          add.lines=list(c("Election Competitiveness", "", "", "Yes", "", "", "Yes"), 
                         c("Time-variant Covariates", "", "Yes", "", "", "Yes", ""),
                         c("Province FEs", "Yes", "Yes", "", "Yes", "Yes", ""),
                         c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          table.layout = "=c#-t-a-s=n")


#### Coefficient plot for main regression results + placebo results
lfe_plot_dat <- data.frame(ATT = sapply(list(lm_2016_1a, lm_2016_1b, lm_2016_1c,
                                             lm_2016_1_placebo2014a, lm_2016_1_placebo2014b, lm_2016_1_placebo2014c,
                                             lm_2016_1_placebo2015a, lm_2016_1_placebo2015b, lm_2016_1_placebo2015c,
                                             lm_2016_1_placebo2016a, lm_2016_1_placebo2016b, lm_2016_1_placebo2016c), 
                                        function(mod){
                                          coef(mod)[2]
                                        }),
                           se = sapply(list(vcov_2016_1a, vcov_2016_1b, vcov_2016_1c,
                                            vcov_2016_1_placebo2014a, vcov_2016_1_placebo2014b, vcov_2016_1_placebo2014c,
                                            vcov_2016_1_placebo2015a, vcov_2016_1_placebo2015b, vcov_2016_1_placebo2015c,
                                            vcov_2016_1_placebo2016a, vcov_2016_1_placebo2016b, vcov_2016_1_placebo2016c), 
                                       function(vcov){
                                         sqrt(diag(vcov))[2]
                                       })) %>%
  mutate(lower = ATT - se*1.96, upper = ATT + se*1.96) %>%
  mutate(spec = rep(c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs"), 4),
         treat_year = rep(c(2016, 2013, 2014, 2015), each = 3),
         placebo = c(rep("Estimated Effect", 3), rep("Estimated Effect, Placebo Treatments", 9))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2016", "2013", "2014", "2015")),
         spec = factor(spec, levels = c("Province FEs + Year FEs", "Time-variant Covs + Province FEs + Year FEs", "Competitiveness + Year FEs")))

ggplot(lfe_plot_dat, aes(x = as.factor(treat_year), y = ATT, ymin = lower, ymax = upper, shape = spec)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect") +
  scale_shape_discrete(name="") +
  scale_x_discrete(labels=c("2016" = "Election in 2016\n\nActual Treatment", 
                            "2013" = "Election in 2013",
                            "2014" = "Election in 2014\n\nPlacebo Treatments",
                            "2015" = "Election in 2015")) +
  facet_grid( ~ placebo, scales="free_x", space="free_x") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom") 
ggsave("../../figure/190618_lfe_placebo.png", width = 8, height = 4)

#### Randomization inference results for RDD analyses

lay <- rbind(c(NA,1,2,2,2,NA),
             c(3:8),
             c(NA,9,10,11,12,NA))

rdd_results <- grid.arrange(layout_matrix = lay,
                            heights = c(1,10,2),
                            widths = c(1,rep(3,4),1),
                            textGrob("Actual Treatment"),
                            textGrob("Placebo Treatments"),
                            y_axe(-20,20),
                            ri_plot(rdd_2016_1, scale=F, xmin=-20, xmax=20, title = "Election in 2016"),
                            ri_plot(rdd_2016_1_placebo2014, scale=F, xmin=-20, xmax=20, title = "Election in 2013"),
                            ri_plot(rdd_2016_1_placebo2015, scale=F, xmin=-20, xmax=20, title = "Election in 2014"),
                            ri_plot(rdd_2016_1_placebo2016, scale=F, xmin=-20, xmax=20, title = "Election in 2015"),
                            y_axe(-20,20) + scale_x_continuous(position = "top"),
                            ri_annotate(rdd_2016_1, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2014, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2015, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2016, show_wilcox = FALSE))
ggsave("../../figure/190618_rdd_results.png", plot = rdd_results, width = 8, height = 4)

#### Synthetic control ATT plots

lay <- cbind(c(rep(1,2), NA),
             c(2:4))
# somehow each call to gsynth_plot() causes an entire plot to be created and filled in the backgroud.
# to get rid of this I have to save the object with arrangeGrob, then call grid.draw() after grid.newpage()

# May 29 UPDATE: This is no longer a problem since plot.gsynth now goes back to return ggplot object
synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(2.5,.5,.5),
                                    widths= c(0.5,8),
                                    grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
                                    #grid.text("Actual Treatment", draw = FALSE),
                                    gsynth_plot(synth_2016_p, xmin = -11, xmax = 2, ymin = -10, ymax = 30),
                                    # slightly hacky solution to align the axis of x_axe with the rest
                                    # of the graph
                                    x_axe(-11,2) + 
                                      scale_x_continuous(labels = function(x) {x + 2016},
                                                         breaks = c(-11:2)) +
                                      # set breaks = -10 s.t the axis text takes up exactly as much space
                                      # as that of the real graphs
                                      scale_y_continuous(breaks = -10),
                                    grid.text("Year", draw = FALSE))
grid.newpage()

png("../../figure/190618_synth_results.png", width = 8, height = 3.5, units="in", res = 96)
synth_results <- grid.draw(synth_results_table)
dev.off()

#### Regression table for dev + admin expenditure results

stargazer(lm_dev_2016_1a, lm_dev_2016_1b, lm_dev_2016_1c,
          lm_admin_2016_1a, lm_admin_2016_1b, lm_admin_2016_1c,
          #se = list(sqrt(diag(vcov_dev_2016_1a)), sqrt(diag(vcov_dev_2016_1b)), sqrt(diag(vcov_dev_2016_1c)),
          #          sqrt(diag(vcov_admin_2016_1a)), sqrt(diag(vcov_admin_2016_1b)), sqrt(diag(vcov_admin_2016_1c))),
          title = "Estimated treatment effects on development and administration expenditures from linear fixed effects models",
          label = "tab:lfe_mech",
          style = "apsr",
          #out = "../../figure/191210_reg_table_mech.tex",
          column.labels = c("Development Expenditure", "Administrative Expenditure"),
          column.separate = c(3,3),
          covariate.labels = c("Treatment Effect"),
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          digits = 3,
          keep = "defeat.true$",
          keep.stat = c("n", "rsq"),
          add.lines=list(c("Election Competitiveness", "", "", "Yes", "", "", "Yes"), 
                         c("Time-invariant Covariates", "", "Yes", "", "", "Yes", ""),
                         c("Province FEs", "Yes", "Yes", "", "Yes", "Yes", ""),
                         c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          table.layout = "=c#-t-a-s=n")

#### RDD and synthetic control results

lay <- cbind(c(NA, 3, NA, NA, rep(9,5)),
             c(1,4,7,10:15),
             c(2,5,8,10:15),
             c(NA,6,rep(NA,7)))

mech_results_table <- arrangeGrob(layout_matrix = lay,
                                  heights = c(1,10,2,1,4,1,4,1,1),
                                  widths = c(.5,4,4,.5),
                                  textGrob("Development Expenditure"),
                                  textGrob("Administrative Expenditure"),
                                  y_axe(-1,1),
                                  ri_plot(rdd_dev_2016_1, scale=F, xmin=-1, xmax=1, title = " "),
                                  ri_plot(rdd_admin_2016_1, scale=F, xmin=-1, xmax=1, title = " "),
                                  y_axe(-1,1) + scale_x_continuous(position = "top"),
                                  ri_annotate(rdd_dev_2016_1, show_wilcox = FALSE),
                                  ri_annotate(rdd_admin_2016_1, show_wilcox = FALSE),
                                  grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
                                  grid.text("Development Expenditure", draw = FALSE),
                                  gsynth_plot(synth_dev_2016_1, xmin = -11, xmax = 2, ymin = -1, ymax = 1),
                                  grid.text("Administrative Expenditure", draw = FALSE),
                                  gsynth_plot(synth_admin_2016_1, xmin = -11, xmax = 2, ymin = -1, ymax = 1),
                                  # slightly hacky solution to align the axis of x_axe with the rest
                                  # of the graph
                                  x_axe(-11,2) + 
                                    scale_x_continuous(labels = function(x) {x + 2016},
                                                       breaks = c(-11:2)) +
                                    # set breaks = -10 s.t the axis text takes up exactly as much space
                                    # as that of the real graphs
                                    scale_y_continuous(breaks = -10),
                                  grid.text("Year", draw = FALSE))
grid.newpage()

png("../../figure/190618_mech_results.png", width = 8, height = 8, units="in", res = 96)
mech_results <- grid.draw(mech_results_table)
dev.off()

#### Table tabulating promotion outcomes



promotion_table <- rbind(cbind(rbind(table(promoted = dat_promo_2006$num.promoted,
                                           defeat = dat_promo_2006$defeat), c(0,0)),
                               rbind(table(promoted = dat_promo_2007$num.promoted,
                                           defeat = dat_promo_2007$defeat), c(0,0)),
                               table(promoted = dat_promo_2010$num.promoted,
                                     defeat = dat_promo_2010$defeat),
                               table(promoted = dat_promo_2011$num.promoted,
                                     defeat = dat_promo_2011$defeat)),
                         cbind(fisher_display(fisher.test(x = dat_promo_2006$any.promoted, 
                                                          y = dat_promo_2006$defeat, alternative = "less"))$odd_ratio,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2007$any.promoted, 
                                                          y = dat_promo_2007$defeat, alternative = "less"))$odd_ratio,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2010$any.promoted, 
                                                          y = dat_promo_2010$defeat, alternative = "less"))$odd_ratio,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2011$any.promoted, 
                                                          y = dat_promo_2011$defeat, alternative = "less"))$odd_ratio,
                               NA),
                         cbind(fisher_display(fisher.test(x = dat_promo_2006$any.promoted, 
                                                          y = dat_promo_2006$defeat, alternative = "less"))$p.value,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2007$any.promoted, 
                                                          y = dat_promo_2007$defeat, alternative = "less"))$p.value,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2010$any.promoted, 
                                                          y = dat_promo_2010$defeat, alternative = "less"))$p.value,
                               NA,
                               fisher_display(fisher.test(x = dat_promo_2011$any.promoted, 
                                                          y = dat_promo_2011$defeat, alternative = "less"))$p.value,
                               NA))
colnames(promotion_table) <- rep(c("T = 0", "T = 1"), 4)
rownames(promotion_table) <- c("No Promotion", "1 Promotion", "2 Promotions", "Fisher Odd Ratio", "Fisher p-value")

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{2}{c}{Gov. in 2006} & \\multicolumn{2}{c}{Gov. in 2007} & \\multicolumn{2}{c}{Gov. in 2010} & \\multicolumn{2}{c}{Gov. in 2011} \\\\\n",
                      "& $T = 0$ & $T = 1$ & $T = 0$ & $T = 1$ & $T = 0$ & $T = 1$ & $T = 0$ & $T = 1$ \\\\\n")
print(xtable(promotion_table, 
             digits = matrix(c(rep(0, 27), rep(2, 18)), nrow = 5, byrow = TRUE),
             align = "lcccccccc",
             label = "tab:promo_mech",
             caption = "Summary of promotion records for the top two leaders in all the provinces in the restricted sample. 
             Promotion records are calculated for leaders who are in power in 2006 (ruling just before the 2007 election), 2007 (in charge of managing the 2007 election),
             2010 (ruling just before the 2011 election), and 2011 (in charge of managing the 2011 election)."), 
      hline.after = c(0,3),
      add.to.row = addtorow, 
      latex.environments = "center",
      include.colnames = FALSE,
      sanitize.text.function = function(x) {x},
      file = "../../figure/190618_table_promo.tex")

#### Synthetic control results specfically for Soc Trang and Can Tho ####

lay <- cbind(c(rep(1,5), NA),
             c(2:7))
# somehow each call to gsynth_plot() causes an entire plot to be created and filled in the backgroud.
# to get rid of this I have to save the object with arrangeGrob, then call grid.draw() after grid.newpage()
synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.5,2.5,.5,2.5,.5,.5),
                                   widths= c(0.5,8),
                                   grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE),
                                   grid.text("Soc Trang", draw = FALSE),
                                   gsynth_plot(synth_2016_p, id = "Soc Trang", xmin = -11, xmax = 2, ymin = -20, ymax = 30),
                                   grid.text("Can Tho", draw = FALSE),
                                   gsynth_plot(synth_2016_p, id = "Can Tho", xmin = -11, xmax = 2, ymin = -20, ymax = 30),
                                   # slightly hacky solution to align the axis of x_axe with the rest
                                   # of the graph
                                   x_axe(-11,2) + 
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-11:2)) +
                                     # set breaks = -10 s.t the axis text takes up exactly as much space
                                     # as that of the real graphs
                                     scale_y_continuous(breaks = -10),
                                   grid.text("Year", draw = FALSE))
grid.newpage()

png("../../figure/190618_synth_results_2prov.png", width = 8, height = 5.5, units="in", res = 96)
synth_results <- grid.draw(synth_results_table)
dev.off()

#### Appendix: No high-level manipulation ####
lay <- rbind(c(NA,1,1,1,NA),
             c(2,3,4,5,6),
             c(NA,7,7,7,NA),
             c(8,9,10,11,12),
             c(NA,13,13,13,NA),
             c(14,15,16,17,18))
digit_test_table <- grid.arrange(layout_matrix = lay,
                                 heights=c(1,10,1,10,1,10),
                                 widths=c(.5,3,3,3,.5),
                                 textGrob("2011 Turnout"),
                                 y_axe(0,.4,flip=F),
                                 digit_test(turnout2011$turnout.voters, digit=1, title="1st Digit*"),
                                 digit_test(turnout2011$turnout.voters, digit=2, title="2nd Digit"),
                                 digit_test(turnout2011$turnout.voters, digit=3, title="3rd Digit"),
                                 y_axe(0,.4,flip=F) + scale_y_continuous(position = "right"),
                                 textGrob("2011 Invalid Votes"),
                                 y_axe(0,.4,flip=F),
                                 digit_test(turnout2011$invalid.votes, digit=1, title="1st Digit"),
                                 digit_test(turnout2011$invalid.votes, digit=2, title="2nd Digit"),
                                 digit_test(turnout2011$invalid.votes, digit=3, title="3rd Digit"),
                                 y_axe(0,.4,flip=F) + scale_y_continuous(position = "right"),
                                 textGrob("2016 Vote Counts"),
                                 y_axe(0,.4,flip=F),
                                 digit_test(result2016$vote, digit=1, title="1st Digit*"),
                                 digit_test(result2016$vote, digit=2, title="2nd Digit"),
                                 digit_test(result2016$vote, digit=3, title="3rd Digit"),
                                 y_axe(0,.4,flip=F) + scale_y_continuous(position = "right"))
ggsave("../../figure/190716_digit_test.png", plot=digit_test_table, width = 8, height = 10)
