#### THIS FILE CONTAINS ALL THE MODELS THAT ARE DONE IN RESPONSE TO SECOND ROUND OF REVIEW AT JOP ####

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("~/vietnam_elections/Data/Working Data")

source("../../Code/Spring 2021/Merge_All.R")

#############################################
# Individual pure synthetic control analyses
#############################################

dat_puresynth <- plan %>%
  filter(year < 2020) %>%
  filter(prov!="Ha Noi" & prov!="TP HCM") %>%
  filter(prov!="Binh Duong")
panelView(net.trans.log ~ defeat, data = dat_puresynth, index = c("prov", "year"))

dat_2016puresynth_p <- dat_puresynth %>%
  mutate(treat = defeat.2016*as.numeric(year>=2017)) %>%
  mutate(prov_no = as.numeric(as.factor(prov))) %>%
  #filter(!prov %in% c("Hau Giang", "Dien Bien", "Dak Nong")) %>%
  filter(year >= 2005) %>%
  drop_na(net.trans.log)
panelView(net.trans.log ~ treat, data = dat_2016puresynth_p, index = c("prov", "year"))

controls_2016_puresynth <- dat_2016puresynth_p %>%
  #filter(defeat.2007!=0 | closewin.2007!=0 | defeat.2011!=0 | closewin.2011!=0 | defeat.2016!=0 | closewin.2016!=0) %>%
  filter(defeat.2016==0) %>%
  pull(prov) %>%
  unique

treat_2016_puresynth <- c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh")
  
## Can Tho 
puresynth_2016_p <- sapply(c(treat_2016_puresynth, controls_2016_puresynth), function(prov) {
  
  puresynth_data_prep <- dataprep(
    dat_2016puresynth_p,
    predictors = c("net.trans", "total.rev", "total.rev.log"),
    special.predictors = list(list("defeat", c(2008, 2012), "mean")),
                              #list("num.districts", c(2008, 2012), "mean"),
                              #list("num.candidates", c(2008, 2012), "mean"),
                              #list("num.seats", c(2008, 2012), "mean"),
                              #list("num.centralnominees", c(2008, 2012), "mean")),
    dependent = "net.trans",
    unit.variable = "prov_no",
    time.variable = "year",
    treatment.identifier = prov,
    controls.identifier = controls_2016_puresynth[controls_2016_puresynth != prov],
    unit.names.variable = "prov",
    time.predictors.prior = c(2005:2016),
    time.optimize.ssr = c(2005:2016),
    time.plot = c(2005:2019)
  )
  
  puresynth_out <- synth(puresynth_data_prep)
  
  effect_out <- puresynth_data_prep$Y1 - puresynth_data_prep$Y0 %*% puresynth_out$solution.w
  
  return(effect_out)
  
}) %>% 
  as_tibble() %>%
  tibble::add_column(year = c(2005:2019)) %>%
  pivot_longer(cols = !year,
               names_to = "prov",
               values_to = "effect") %>%
  mutate(treat = prov %in% treat_2016_puresynth)

ggplot(puresynth_2016_p, 
       aes(x = year, y = effect, group = prov, color = treat, alpha = treat)) +
  geom_line(alpha = .5, size = 1) +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Estimated Treatment Effect\n(log of net transfers)") +
  xlab("Year") +
  #coord_cartesian(ylim = c(-30, 20)) +
  scale_x_continuous(breaks = c(2005:2019)) +
  scale_color_manual(values = c("light grey", "black")) +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.title.y = element_text(size = 8),
        legend.position="bottom")
