#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("G:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2021/Merge_All.R")

## Big picture
plan %>% 
  filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011) %>% 
  select(prov, year, net.trans, net.trans.log, net.trans.change) %>% 
  ggplot(data = ., aes(x = year, y = net.trans.log)) + 
  geom_point() +
  geom_smooth(data = plan %>% 
                filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), 
                       year>2011 , year <= 2016),
              formula = y ~ x,
              method = "lm") + 
  geom_smooth(data = plan %>% 
               filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), 
                      year>2011 , year >= 2017),
             formula = y ~ x,
             method = "lm") + 
  facet_wrap(. ~ prov, scales = "free_y")

prov %>% 
  filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011) %>% 
  select(prov, year, dev.exp, dev.exp.log, dev.exp.change) %>% 
  ggplot(data = ., aes(x = year, y = dev.exp.log)) + 
  geom_point() +
  geom_smooth(data = prov %>% 
                filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011 , year <= 2016),
              formula = y ~ x,
              method = "lm") + 
  geom_smooth(data = prov %>% 
                filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011 , year >= 2017),
              formula = y ~ x,
              method = "lm") + 
  facet_wrap(. ~ prov, scales = "free_y")

prov %>% 
  filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011) %>% 
  select(prov, year, admin.exp, admin.exp.log, dev.exp.change) %>% 
  ggplot(data = ., aes(x = year, y = admin.exp.log)) + 
  geom_point() +
  geom_smooth(data = prov %>% 
                filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011 , year <= 2016),
              formula = y ~ x,
              method = "lm") + 
  geom_smooth(data = prov %>% 
                filter(prov %in% c("Can Tho", "Dong Thap", "Phu Yen", "Soc Trang", "Tra Vinh"), year>2011 , year >= 2017),
              formula = y ~ x,
              method = "lm") + 
  facet_wrap(. ~ prov, scales = "free_y")

## Details for each province

# Can Tho
plan %>% 
  arrange(year) %>%
  filter(prov == "Can Tho") %>%
  mutate(net.trans = round(net.trans / 1000)) %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Can Tho", district == "2") %>%
  select(prov, district, name, age, male, central_nominator, occupation, centralnominated, percentage, result, defeat)

# Dong Thap

plan %>% 
  arrange(year) %>%
  filter(prov == "Dong Thap") %>%
  mutate(net.trans = round(net.trans / 1000)) %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Dong Thap", district == "1") %>%
  select(prov, district, name, age, male, central_nominator, occupation, centralnominated, percentage, result, defeat)

# Phu Yen

plan %>% 
  arrange(year) %>%
  filter(prov == "Phu Yen") %>%
  mutate(net.trans = round(net.trans / 1000)) %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Phu Yen", district == "1") %>%
  select(prov, district, name, age, male, central_nominator, occupation, centralnominated, percentage, result, defeat)

# Tra Vinh

plan %>% 
  arrange(year) %>%
  filter(prov == "Tra Vinh") %>%
  mutate(net.trans = round(net.trans / 1000)) %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Tra Vinh", district == "1") %>%
  select(prov, district, name, age, male, central_nominator, occupation, centralnominated, percentage, result, defeat)

# Soc Trang
plan %>% 
  arrange(year) %>%
  filter(prov == "Soc Trang") %>%
  mutate(net.trans = round(net.trans / 1000)) %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Soc Trang", district == "2") %>%
  select(prov, district, name, age, male, central_nominator, occupation, centralnominated, percentage, result, defeat)

## List of defeated candidates
candidates2016 %>% 
  filter(centralnominated == 1, defeat == 1) %>%
  select(prov, district, name, gender, occupation, power)

## Table showing which provinces lost in each election
plan %>% 
  filter(year == 2017) %>%
  filter(defeat.2007 == 1 | defeat.2011 == 1 | defeat.2016 == 1) %>%
  select(prov, defeat.2007, defeat.2011, defeat.2016)
