#### THIS FILE CONTAINS ILLUSTRATION FOR ALL MODELs THAT WILL APPEAR IN THE FINAL PAPER ####

## i.e. it runs Models_Final.R and then output well designed graphics for these models ##

library(stargazer)
library(grid)
library(gridExtra)
library(ggplot2)
library(xtable)
library(kableExtra)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2021/Models_Final.R")
source("../../Code/Spring 2021/Models_Mechanism.R")
source("../../Code/Spring 2021/Models_Revision.R")
source("../../Code/Spring 2021/Balance_Final.R")
source("../../Code/Spring 2021/Models_DigitTests.R")
source("../../Code/Spring 2021/Models_ImputeShares.R")
source("../../Code/Spring 2021/Models_Final_boot.R")
source("../../Code/Spring 2021/Models_ShareDif.R")
source("../../Code/Spring 2021/Illustration_Functions.R")

#### Some statistics to include in the paper

## percentage of leaders serving in their own hometown in 2015
leaders %>% 
  ungroup() %>% 
  filter(year == 2015) %>% 
  select(id, name, position, province, hometown, nha_que, nha_que2) %>% 
  summarize(hometown = mean(hometown, na.rm = T))

## Baseline transfers to understand treatment effects
plan %>% 
  filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), 
         year > 2011, year < 2017) %>% 
  select(prov, year, net.trans, net.trans.change, total.exp) %>%
  group_by(prov) %>%
  summarise(net.trans_mean = mean(net.trans)/1000,
            net.trans.change_mean = mean(net.trans.change)/1000,
            total.exp_2016 = sum(total.exp * (year == 2015))/1000)

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

colnames(balance_table) <- c("Control Mean ($N = 9$)", "Treated Mean ($N = 4$)", "Std. Diff. in Means", "RI Std. Error", "RI p-value", "OLS Std. Error", "OLS p-value",
                             "Control Mean ($N = 11$)", "Treated Mean ($N = 4$)", "Std. Diff. in Means", "RI Std. Error", "RI p-value", "OLS Std. Error", "OLS p-value")

# Experiment with kable()

sink("../../figure/210118_table_balance.tex")
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
          title = "Estimated treatment effects of localized defeats on log of first-differenced central transfers from linear fixed effects models. Cluster-robust standard errors appear in parentheses.",
          label = "tab:lfe_main",
          style = "apsr",
          out = "../../figure/210202_reg_table.tex",
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
  mutate(spec = rep(c("Prov. FEs + Year FEs", 
                      "Time-variant Covs + Prov. FEs + Year FEs", 
                      "Competitiveness + Year FEs"), 4),
         treat_year = rep(c(2016, 2013, 2014, 2015), each = 3),
         placebo = c(rep("Actual Treatment", 3), rep("Placebo Treatments", 9))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2016", "2013", "2014", "2015")),
         spec = factor(spec, levels = c("Prov. FEs + Year FEs", 
                                        "Time-variant Covs + Prov. FEs + Year FEs", 
                                        "Competitiveness + Year FEs")))

lay <- rbind(c(1,2,3),
             c(NA,NA,3))

lfe_results <- grid.arrange(layout_matrix = lay,
                            heights = c(2,.5),
                            widths = c(.2,.2,6.1),
                            grid.text("Estimated Treatment Effect", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=10, fontface = "bold")),
                            grid.text("(log of first-differenced central transfers)", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=9, fontface = "italic")),
                            ggplot(lfe_plot_dat, aes(x = factor(treat_year,
                                                                levels = c("2016","2015","2014","2013")), 
                                                     y = ATT, ymin = lower, ymax = upper, shape = spec)) +
                              geom_point(position = position_dodge(width = 0.4)) +
                              geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
                              geom_hline(yintercept = 0, linetype = "dashed") +
                              ylab(NULL) +
                              scale_shape_discrete(name="") +
                              scale_x_discrete(labels=c("2016" = "Election in 2016", 
                                                        "2013" = "Election in 2013",
                                                        "2014" = "Election in 2014",
                                                        "2015" = "Election in 2015")) +
                              facet_grid( ~ placebo, scales="free_x", space="free_x") +
                              theme_bw() + 
                              theme(axis.title = element_text(size = 11, face = "bold"),
                                    axis.title.x = element_blank(),
                                    axis.text.x = element_text(size = 10, face = "bold"),
                                    axis.ticks=element_blank(),
                                    legend.position="bottom",
                                    legend.key.size = unit(.5,"lines"),
                                    legend.box.spacing = unit(5,"points"),
                                    legend.text.align = 0))
ggsave("../../figure/210202_lfe_placebo.png", plot = lfe_results, width = 6.5, height = 2.5)

#### Randomization inference results for RDD analyses

lay <- rbind(c(NA,NA,NA, 1,2,2,2),
             c(3:9),
             c(NA,NA,NA, 10:13))

rdd_results <- grid.arrange(layout_matrix = lay,
                            heights = c(.25,2,.25),
                            widths = c(.2,.2,.25,rep(1.4625,4)),
                            textGrob("Actual Treatment", gp=gpar(fontsize=10)),
                            textGrob("Placebo Treatments", gp=gpar(fontsize=10)),
                            grid.text("Estimated Treatment Effect", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=10, fontface = "bold")),
                            grid.text("(log of first-differenced central transfers)", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=9, fontface = "italic")),
                            y_axe(-20,20),
                            ri_plot(rdd_2016_1, scale=F, xmin=-20, xmax=20, title = "Election in 2016") +
                              theme(plot.margin = unit(c(5.5,5.5,5.5,0), "points")),
                            ri_plot(rdd_2016_1_placebo2016, scale=F, xmin=-20, xmax=20, title = "Election in 2015"),
                            ri_plot(rdd_2016_1_placebo2015, scale=F, xmin=-20, xmax=20, title = "Election in 2014"),
                            ri_plot(rdd_2016_1_placebo2014, scale=F, xmin=-20, xmax=20, title = "Election in 2013"),
                            #y_axe(-20,20) + scale_x_continuous(position = "top"),
                            ri_annotate(rdd_2016_1, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2016, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2015, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_1_placebo2014, show_wilcox = FALSE))
ggsave("../../figure/210202_rdd_results.png", plot = rdd_results, width = 6.5, height = 3, units = "in")

#### Synthetic control ATT plots

lay <- rbind(c(1,2,3),
             c(NA,NA,4))
# somehow each call to gsynth_plot() causes an entire plot to be created and filled in the backgroud.
# to get rid of this I have to save the object with arrangeGrob, then call grid.draw() after grid.newpage()

synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(1.75,.25),
                                    widths= c(0.2,0.2,6.1),
                                    grid.text("Estimated Treatment Effect", rot = 90, 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")),
                                    grid.text("(log of central transfers)", rot = 90, 
                                              draw = FALSE,
                                              gp=gpar(fontsize=9, fontface = "italic")),
                                    gsynth_plot(synth_2016_p, xmin = -13, xmax = 3,
                                                ymin = - 1, ymax = 3) +
                                      scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
                                      scale_x_continuous(labels = function(x) {x + 2016},
                                                         breaks = c(-13:3)) + 
                                      theme(axis.text.x = element_text(size = 10),
                                            legend.position="bottom"),
                                    grid.text("Year", 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results.png", plot = synth_results_table, width = 6.5, height = 2.5, units = "in")


#### Appendix: dev + admin expenditure results ####

## linear fixed effects regression table
stargazer(lm_dev_2016_1a, lm_dev_2016_1b, lm_dev_2016_1c,
          lm_admin_2016_1a, lm_admin_2016_1b, lm_admin_2016_1c,
          se = list(sqrt(diag(vcov_dev_2016_1a)), sqrt(diag(vcov_dev_2016_1b)), sqrt(diag(vcov_dev_2016_1c)),
                    sqrt(diag(vcov_admin_2016_1a)), sqrt(diag(vcov_admin_2016_1b)), sqrt(diag(vcov_admin_2016_1c))),
          title = "Estimated instantaneous treatment effects of localized defeats on log of development and administration expenditures from linear fixed effects models. Cluster-robust standard errors appear in parentheses.",
          label = "tab:lfe_mech",
          style = "apsr",
          out = "../../figure/210202_reg_table_mech.tex",
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

## RDD and synthetic control results

lay <- cbind(rep(1,7),
             rep(2,7),
             c(3,5,7:11),
             c(4,6,7:11))

mech_results_table <- arrangeGrob(layout_matrix = lay,
                                  heights = c(2.5,
                                              .50,
                                              .25,2.25,
                                              .25,2.25,
                                              .25),
                                  widths = c(.25,.25,3,3),
                                  grid.text("Estimated Treatment Effect", rot = 90, draw = FALSE,
                                            gp=gpar(fontsize=10, fontface = "bold")),
                                  grid.text("(log of expenditures)", rot = 90, 
                                            draw = FALSE,
                                            gp=gpar(fontsize=9, fontface = "italic")),
                                  ri_plot(rdd_dev_2016_1, scale=F, xmin=-1, xmax=1, axe.y = TRUE, title = "Development Expenditure"),
                                  ri_plot(rdd_admin_2016_1, scale=F, xmin=-1, xmax=1, title = "Administrative Expenditure"),
                                  ri_annotate(rdd_dev_2016_1, show_wilcox = FALSE),
                                  ri_annotate(rdd_admin_2016_1, show_wilcox = FALSE),
                                  grid.text("Development Expenditure", draw = FALSE,
                                            gp=gpar(fontsize=10, fontface = "bold")),
                                  gsynth_plot(synth_dev_2016_p, xmin = -12, xmax = 3, ymin = -.5, ymax = 1) +
                                    theme(axis.text.x = element_text(color = "white")),
                                  grid.text("Administrative Expenditure", draw = FALSE,
                                            gp=gpar(fontsize=10, fontface = "bold")),
                                  gsynth_plot(synth_admin_2016_p, xmin = -12, xmax = 3, ymin = -.5, ymax = 1) +
                                    scale_x_continuous(labels = function(x) {x + 2016},
                                                       breaks = c(-12:3)),
                                  grid.text("Year", 
                                            draw = FALSE,
                                            gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(mech_results_table)
ggsave("../../figure/210202_mech_results.png", plot = mech_results_table, 
       width = 6.5, height = 8.25, units = "in")


#### Appendix: Absence of punishment ####

## Table tabulating promotion outcomes
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
      caption.placement = "top",
      file = "../../figure/210202_table_promo.tex")

#### Appendix: No effect of bargaining

## Synthetic control results specfically for Can Tho

lay <- cbind(c(NA,1,NA),
             c(NA,2,NA),
             c(3:5))

synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.25,1.75,.25),
                                   widths= c(0.2, 0.2, 6.1),
                                   grid.text("Estimated Treatment Effect", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   grid.text("(log of central transfers)", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=9, fontface = "italic")),
                                   grid.text("Can Tho", draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   gsynth_plot(synth_2016_p, id = "Can Tho", xmin = -13, xmax = 3,
                                               ymin = -.5, ymax = 1) +
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-13:3)) + 
                                     theme(axis.text.x = element_text(size = 10),
                                           legend.position="bottom"),
                                   grid.text("Year", 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_CanTho.png", plot = synth_results_table, width = 6.5, height = 2.75, units = "in")

## Synthetic control results specfically for remaining provinces

lay <- cbind(c(NA,1,NA),
             c(NA,2,NA),
             c(3:5))

# Dong Thap
synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.25,1.75,.25),
                                   widths= c(0.2, 0.2, 6.1),
                                   grid.text("Estimated Treatment Effect", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   grid.text("(log of central transfers)", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=9, fontface = "italic")),
                                   grid.text("Dong Thap", draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   gsynth_plot(synth_2016_p, id = "Dong Thap", xmin = -13, xmax = 3,
                                               ymin = -.5, ymax = 3) +
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-13:3)) + 
                                     theme(axis.text.x = element_text(size = 10),
                                           legend.position="bottom"),
                                   grid.text("Year", 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_DongThap.png", plot = synth_results_table, width = 6.5, height = 2.75, units = "in")


# Phu Yen
synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.25,1.75,.25),
                                   widths= c(0.2, 0.2, 6.1),
                                   grid.text("Estimated Treatment Effect", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   grid.text("(log of central transfers)", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=9, fontface = "italic")),
                                   grid.text("Phu Yen", draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   gsynth_plot(synth_2016_p, id = "Phu Yen", xmin = -13, xmax = 3,
                                               ymin = -.5, ymax = 3) +
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-13:3)) + 
                                     theme(axis.text.x = element_text(size = 10),
                                           legend.position="bottom"),
                                   grid.text("Year", 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_PhuYen.png", plot = synth_results_table, width = 6.5, height = 2.75, units = "in")


# Tra Vinh
synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.25,1.75,.25),
                                   widths= c(0.2, 0.2, 6.1),
                                   grid.text("Estimated Treatment Effect", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   grid.text("(log of central transfers)", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=9, fontface = "italic")),
                                   grid.text("Tra Vinh", draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   gsynth_plot(synth_2016_p, id = "Tra Vinh", xmin = -13, xmax = 3,
                                               ymin = -.5, ymax = 3) +
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-13:3)) + 
                                     theme(axis.text.x = element_text(size = 10),
                                           legend.position="bottom"),
                                   grid.text("Year", 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_TraVinh.png", plot = synth_results_table, width = 6.5, height = 2.75, units = "in")

# Soc Trang
synth_results_table <- arrangeGrob(layout_matrix = lay,
                                   heights= c(.25,1.75,.25),
                                   widths= c(0.2, 0.2, 6.1),
                                   grid.text("Estimated Treatment Effect", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   grid.text("(log of central transfers)", rot = 90, 
                                             draw = FALSE,
                                             gp=gpar(fontsize=9, fontface = "italic")),
                                   grid.text("Soc Trang", draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")),
                                   gsynth_plot(synth_2016_SocTrang_p, id = "Soc Trang", xmin = -13, xmax = 3,
                                               ymin = -.5, ymax = 3) +
                                     scale_x_continuous(labels = function(x) {x + 2016},
                                                        breaks = c(-13:3)) + 
                                     theme(axis.text.x = element_text(size = 10),
                                           legend.position="bottom"),
                                   grid.text("Year", 
                                             draw = FALSE,
                                             gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_SocTrang.png", plot = synth_results_table, width = 6.5, height = 2.75, units = "in")


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
ggsave("../../figure/190916_digit_test.png", plot=digit_test_table, width = 8, height = 10)

#### Appendix: Robustness to small sample ####
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
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  xlab("Model Specification") +
  coord_cartesian(ylim = c(-2.5, 20)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 8),
        legend.position="bottom")
ggsave("../../figure/210202_perturb_results_treat_add.png", width = 6.5, height = 2.5)

lm_2016_treat_drop_plot_dat <- boot_plot_dat_gen(lm_2016_treat_drop_summary)

ggplot(lm_2016_treat_drop_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.4), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.4),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  xlab("Model Specification") +
  coord_cartesian(ylim = c(-2.5, 20)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 8),
        legend.position="bottom")
ggsave("../../figure/210202_perturb_results_treat_drop.png", width = 6.5, height = 2.25)

lm_2016_control_add_plot_dat <- boot_plot_dat_gen(lm_2016_control_add_summary)

ggplot(lm_2016_control_add_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.8), fatten = 3) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.8),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  xlab("Model Specification") +
  coord_cartesian(ylim = c(-2.5, 20)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 8),
        legend.position="bottom")
ggsave("../../figure/210202_perturb_results_control_add.png", width = 6.5, height = 2.25)

lm_2016_control_drop_plot_dat <- boot_plot_dat_gen(lm_2016_control_drop_summary)

ggplot(lm_2016_control_drop_plot_dat, aes(x = Model, y = Estimate, ymin = lower, ymax = upper, group = Estimate)) +
  geom_pointrange(position = position_dodge(width = 0.8), fatten = 5) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90),
                 position = position_dodge(width = 0.8),
                 size = 1.2, alpha = .5) +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  xlab("Model Specification") +
  coord_cartesian(ylim = c(-2.5, 20)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 8),
        legend.position="bottom")
ggsave("../../figure/210202_perturb_results_control_drop.png", width = 6.5, height = 2.25)


#### Appendix: 2011 and 2007 results ####

## Naive estimates

# 2011
lfe_2011_plot_dat <- data.frame(ATT = sapply(list(lm_2011_1a, lm_2011_1b, lm_2011_1c,
                                                  lm_2011_1_placebo2009a, lm_2011_1_placebo2009b, lm_2011_1_placebo2009c,
                                                  lm_2011_1_placebo2010a, lm_2011_1_placebo2010b, lm_2011_1_placebo2010c,
                                                  lm_2011_1_placebo2011a, lm_2011_1_placebo2011b, lm_2011_1_placebo2011c), 
                                             function(mod){
                                               coef(mod)[2]
                                             }),
                                se = sapply(list(vcov_2011_1a, vcov_2011_1b, vcov_2011_1c,
                                                 vcov_2011_1_placebo2009a, vcov_2011_1_placebo2009b, vcov_2011_1_placebo2009c,
                                                 vcov_2011_1_placebo2010a, vcov_2011_1_placebo2010b, vcov_2011_1_placebo2010c,
                                                 vcov_2011_1_placebo2011a, vcov_2011_1_placebo2011b, vcov_2011_1_placebo2011c), 
                                            function(vcov){
                                              sqrt(diag(vcov))[2]
                                            })) %>%
  mutate(lower = ATT - se*1.96, upper = ATT + se*1.96) %>%
  mutate(spec = rep(c("Prov. FEs + Year FEs", 
                      "Time-variant Covs + Prov. FEs + Year FEs", 
                      "Competitiveness + Year FEs"), 4),
         treat_year = rep(c(2011, 2008, 2009, 2010), each = 3),
         placebo = c(rep("Actual Treatment", 3), rep("Placebo Treatments", 9))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2011", "2008", "2009", "2010")),
         spec = factor(spec, levels = c("Prov. FEs + Year FEs", 
                                        "Time-variant Covs + Prov. FEs + Year FEs", 
                                        "Competitiveness + Year FEs")))

ggplot(lfe_2011_plot_dat, aes(x = factor(treat_year,
                                         levels = c("2011", "2010", "2009", "2008")), 
                              y = ATT, ymin = lower, ymax = upper, shape = spec)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  scale_shape_discrete(name="") +
  scale_x_discrete(labels=c("2011" = "Election in 2011", 
                            "2008" = "Election in 2008",
                            "2009" = "Election in 2009",
                            "2010" = "Election in 2010")) +
  facet_grid( ~ placebo, scales="free_x", space="free_x") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom",
        legend.key.size = unit(.5,"lines"),
        legend.box.spacing = unit(5,"points"),
        legend.text.align = 0)
ggsave("../../figure/210202_lfe_placebo_2011.png", width = 6.5, height = 3)

# 2007

#### Coefficient plot for main regression results + placebo results
lfe_2007_plot_dat <- data.frame(ATT = sapply(list(lm_2007_pa, lm_2007_pb, lm_2007_pc,
                                                  lm_2007_p_placebo2006a, lm_2007_p_placebo2006b, lm_2007_p_placebo2006c,
                                                  lm_2007_p_placebo2007a, lm_2007_p_placebo2007b, lm_2007_p_placebo2007c), 
                                             function(mod){
                                               coef(mod)[2]
                                             }),
                                se = sapply(list(vcov_2007_pa, vcov_2007_pb, vcov_2007_pc,
                                                 vcov_2007_p_placebo2006a, vcov_2007_p_placebo2006b, vcov_2007_p_placebo2006c,
                                                 vcov_2007_p_placebo2007a, vcov_2007_p_placebo2007b, vcov_2007_p_placebo2007c), 
                                            function(vcov){
                                              sqrt(diag(vcov))[2]
                                            })) %>%
  mutate(lower = ATT - se*1.96, upper = ATT + se*1.96) %>%
  mutate(spec = rep(c("Prov. FEs + Year FEs", 
                      "Time-variant Covs +\nProv. FEs + Year FEs", 
                      "Competitiveness +\nYear FEs"), 3),
         treat_year = rep(c(2007, 2005, 2006), each = 3),
         placebo = c(rep("Actual Treatment", 3), rep("Placebo Treatments", 6))) %>%
  mutate(treat_year = factor(treat_year, levels = c("2007", "2005", "2006")),
         spec = factor(spec, levels = c("Prov. FEs + Year FEs", 
                                        "Time-variant Covs +\nProv. FEs + Year FEs", 
                                        "Competitiveness +\nYear FEs")))

ggplot(lfe_2007_plot_dat, aes(x = as.factor(treat_year), y = ATT, ymin = lower, ymax = upper, shape = spec)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  scale_shape_discrete(name="") +
  scale_x_discrete(labels=c("2007" = "Election in 2007", 
                            "2005" = "Election in 2005",
                            "2006" = "Election in 2006")) +
  facet_grid( ~ placebo, scales="free_x", space="free_x") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom",
        legend.key.size = unit(.5,"lines"),
        legend.box.spacing = unit(5,"points"),
        legend.text.align = 0) 
ggsave("../../figure/210202_lfe_placebo_2007.png", width = 5, height = 3)

## Synthetic control

# 2011
lay <- cbind(c(1,NA),
             c(2,3))

synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(1.75,.25),
                                    widths= c(0.4,6.1),
                                    grid.text("Estimated Treatment Effect\n(log of central transfers)", rot = 90, 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")),
                                    gsynth_plot(synth_2011_p, xmin = -8, xmax = 4, ymin = -1, ymax = 4) +
                                      scale_x_continuous(labels = function(x) {x + 2011},
                                                         breaks = c(-8:4)) + 
                                      theme(axis.text.x = element_text(size = 10),
                                            legend.position="bottom"),
                                    grid.text("Year", 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results_2011.png", plot = synth_results_table, width = 6.5, height = 2.5, units = "in")


lay <- rbind(c(1,2,3),
             c(NA,NA,4))
# somehow each call to gsynth_plot() causes an entire plot to be created and filled in the backgroud.
# to get rid of this I have to save the object with arrangeGrob, then call grid.draw() after grid.newpage()

synth_results_table <- grid.arrange(layout_matrix = lay,
                                    heights= c(1.75,.25),
                                    widths= c(0.2,0.2,6.1),
                                    grid.text("Estimated Treatment Effect", rot = 90, 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")),
                                    grid.text("(log of central transfers)", rot = 90, 
                                              draw = FALSE,
                                              gp=gpar(fontsize=9, fontface = "italic")),
                                    gsynth_plot(synth_2016_p, xmin = -13, xmax = 3,
                                                ymin = - 1, ymax = 3) +
                                      scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
                                      scale_x_continuous(labels = function(x) {x + 2016},
                                                         breaks = c(-13:3)) + 
                                      theme(axis.text.x = element_text(size = 10),
                                            legend.position="bottom"),
                                    grid.text("Year", 
                                              draw = FALSE,
                                              gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_results_table)
ggsave("../../figure/210202_synth_results.png", plot = synth_results_table, width = 6.5, height = 2.5, units = "in")



## Imputation Output

# 2011
impute_2011_plot_dat <- data.frame(impute_2011_1) %>% 
  na.omit %>%
  gather(key = "model",
         value = "beta")

ggplot(impute_2011_plot_dat, aes(x = model, y = beta)) +
  geom_violin(alpha = .3, fill = "gray") +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect\n(log of first-differenced transfers)") +
  xlab("Model Specification") +
  scale_x_discrete(labels=c("beta_1a" = "Province FEs +\nYear FEs", 
                            "beta_1b" = "Time-variant Covs +\nProvince FEs +\nYear FEs",
                            "beta_1c" = "Competitiveness +\nYear FEs")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")

ggsave("../../figure/210202_impute_results_2011.png", width = 5, height = 3)

# 2007 
impute_2007_plot_dat <- data.frame(impute_2007_1) %>% 
  na.omit %>%
  gather(key = "model",
         value = "beta")

ggplot(impute_2007_plot_dat, aes(x = model, y = beta)) +
  geom_violin(alpha = .3, fill = "gray") +
  geom_hline(aes(yintercept = 0), linetype="dashed", colour="black") +
  ylab("Estimated Treatment Effect\n(log of central transfers)") +
  xlab("Model Specification") +
  scale_x_discrete(labels=c("beta_1a" = "Province FEs +\nYear FEs", 
                            "beta_1b" = "Time-variant Covs +\nProvince FEs +\nYear FEs",
                            "beta_1c" = "Competitiveness +\nYear FEs")) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        legend.position="bottom")

ggsave("../../figure/210202_impute_results_2007.png", width = 5, height = 3)

#### Appendix: Absence of strategic behavior ####

## regression table
stargazer(lm_repeat_1a, 
          lm_repeat_1b,
          lm_repeat_1c,
          se = list(sqrt(diag(vcov_repeat_1a)), 
                    sqrt(diag(vcov_repeat_1b)),
                    sqrt(diag(vcov_repeat_1c))),
          title = "Estimated effects of past central candidate defeat and net transfer changes
          on probability of defeat width = 6.5 fixed effects models",
          label = "tab:lfe_repeat",
          style = "apsr",
          out = "../../figure/210202_reg_table_repeat.tex",
          #column.labels = c("Instantaneous Effect", "Persistent Effect"),
          #column.separate = c(2,2),
          covariate.labels = c("Previous Central Candidate Defeat", 
                               "(Logged) Net Transfer Change", 
                               "Previous Central Candidate Defeat $\\times$ \\\\ (Logged) Net Transfer Change"),
          dep.var.caption = "Any Central Candidate Defeat",
          dep.var.labels.include = TRUE,
          digits = 3,
          keep = c("defeat.lag$", "net.trans.change.log.lag$"),
          add.lines=list(c("Province FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                         c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("n", "rsq"),
          table.layout = "=c#-t-a-s=n")

#### Appendix: Compare central candidates inside and outside the range ####

balance_candidates <- rbind(t(balance_candidates_sample[,-1]), t(balance_candidates_full[,-1])) %>%
  na_if(0)

balance_candidates_print <- balance_candidates

balance_candidates_print[c(1, 14),] <- formatC(balance_candidates[c(1,14),], format = "d")
balance_candidates_print[c(seq(2, 12, 2),
                           seq(15, 25, 2)),] <- formatC(balance_candidates[c(seq(2, 12, 2),
                                                                             seq(15, 25, 2)),], 
                                                        digits = 2,
                                                        format = "f")
balance_candidates_print[c(seq(3, 13, 2),
                           seq(16, 26, 2)),] <- paste0("(", formatC(balance_candidates[c(seq(3, 13, 2),
                                                                                         seq(16, 26, 2)),], 
                                                                    digits = 2,
                                                                    format = "f"),
                                                       ")")
balance_candidates_print[balance_candidates == 1] <- "1"
balance_candidates_print[is.na(balance_candidates)] <- ""

rownames(balance_candidates_print) <- rep(c("N",
                                            "Age", "",
                                            "Male", "",
                                            "CPV Member", "",
                                            "Years in CPV", "",
                                            "Education Level (1-5)", "",
                                            "Power Index (1-5)", ""), 2)

colnames(balance_candidates_print) <- balance_candidates_sample$margin_cut

# Experiment with kable()

options(knitr.kable.NA = '')
sink("../../figure/210123_table_candidates_stats.tex")
kable(balance_candidates_print,
      format = "latex",
      digits = c(2,2,2,2),
      align = c("c","c","c","c"),
      booktabs = T,
      escape = T,
      caption = "Summary of candidate-level statistics for central candidates 
      within different ranges of vote margins. Standard errors appear in parentheses.",
      label = "candidates_stats") %>%
  add_header_above(c(" " = 1,
                     "Vote Margin" = 4),
                   align = "c",
                   line = FALSE) %>%
  column_spec(1, width = "10em") %>%
  column_spec(c(2:4), width = "4.1em") %>%
  pack_rows("Including Hanoi, Ho Chi Minh City, and Binh Duong", 1, 13, 
            latex_gap_space = ".5em", bold = TRUE, indent = FALSE) %>%
  pack_rows("Excluding Hanoi, Ho Chi Minh City, and Binh Duong", 14, 26, 
            latex_gap_space = ".5em", bold = TRUE, indent = FALSE) %>%
  kable_styling(latex_options = c("hold_position"),
                position = "c")
sink()

#### Appendix: Results without any bandwith restriction

#### Regression table for main regression results

## linear fixed effects model
stargazer(lm_2016_simple_1a, lm_2016_simple_1b, lm_2016_simple_1c,
          lm_2016_simple_pa, lm_2016_simple_pb, lm_2016_simple_pc,
          se = list(sqrt(diag(vcov_2016_simple_1a)), sqrt(diag(vcov_2016_simple_1b)), sqrt(diag(vcov_2016_simple_1c)),
                    sqrt(diag(vcov_2016_simple_pa)), sqrt(diag(vcov_2016_simple_pb)), sqrt(diag(vcov_2016_simple_pc))),
          title = "Estimated treatment effects of localized defeats on log of first-differenced central transfers 
          width = 6.5 fixed effects models for a full sample without restriction on central candidate vote margins.",
          label = "tab:lfe_simple",
          style = "apsr",
          out = "../../figure/210125_reg_simple_table.tex",
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

## Randomization inference results for RDD analyses

lay <- rbind(c(NA, NA, NA, 1, 2),
             c(3, 4, 5, 6, 7),
             c(NA, NA, NA, 8, 9))

rdd_simple_results <- grid.arrange(layout_matrix = lay,
                            heights = c(.25,2.5,.25),
                            widths = c(.2,.2,.25,rep(1.5,2)),
                            textGrob("Instantaneous Effect", gp=gpar(fontsize=10)),
                            textGrob("Persistent Effect", gp=gpar(fontsize=10)),
                            grid.text("Estimated Treatment Effect", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=10, fontface = "bold")),
                            grid.text("(log of first-differenced central transfers)", rot = 90, 
                                      draw = FALSE,
                                      gp=gpar(fontsize=9, fontface = "italic")),
                            y_axe(-20,20) + ggtitle(NULL),
                            ri_plot(rdd_2016_simple_1, scale=F, xmin=-20, xmax=20) +
                              theme(plot.margin = unit(c(5.5,5.5,5.5,0), "points")),
                            ri_plot(rdd_2016_simple_p, scale=F, xmin=-20, xmax=20),
                            ri_annotate(rdd_2016_simple_1, show_wilcox = FALSE),
                            ri_annotate(rdd_2016_simple_p, show_wilcox = FALSE))
ggsave("../../figure/210202_rdd_simple_results.png", plot = rdd_simple_results, width = 3.65, height = 3, units = "in")

#### Synthetic control ATT plots

lay <- rbind(c(1,2,3),
             c(NA,NA,4))

synth_simple_results_table <- grid.arrange(layout_matrix = lay,
                                           heights= c(1.75,.25),
                                           widths= c(0.2,0.2,6.1),
                                           grid.text("Estimated Treatment Effect", rot = 90, 
                                                     draw = FALSE,
                                                     gp=gpar(fontsize=10, fontface = "bold")),
                                           grid.text("(log of central transfers)", rot = 90, 
                                                     draw = FALSE,
                                                     gp=gpar(fontsize=9, fontface = "italic")),
                                           gsynth_plot(synth_2016_simple_p, xmin = -13, xmax = 3,
                                                       ymin = - 1, ymax = 6) +
                                             scale_y_continuous(breaks = c(-1:6)) +
                                             scale_x_continuous(labels = function(x) {x + 16},
                                                                breaks = c(-13:3)) + 
                                             theme(axis.text.x = element_text(size = 10),
                                                   legend.position="bottom"),
                                           grid.text("Year", 
                                                     draw = FALSE,
                                                     gp=gpar(fontsize=10, fontface = "bold")))
grid.newpage()
grid.draw(synth_simple_results_table)
ggsave("../../figure/210125_synth_simple_results.png", plot = synth_simple_results_table, width = 6.5, height = 2.5, units = "in")

