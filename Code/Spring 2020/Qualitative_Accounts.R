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
  filter(prov == "Can Tho") %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Can Tho", district == "2") %>%
  select(prov, district, name, occupation, centralnominated, percentage, result, defeat)

# Dong Thap

plan %>% 
  filter(prov == "Dong Thap") %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Dong Thap", district == "1") %>%
  select(prov, district, name, occupation, centralnominated, percentage, result, defeat)

# Phu Yen

plan %>% 
  filter(prov == "Phu Yen") %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Phu Yen", district == "1") %>%
  select(prov, district, name, occupation, centralnominated, percentage, result, defeat)

# Tra Vinh

plan %>% 
  filter(prov == "Tra Vinh") %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Tra Vinh", district == "1") %>%
  select(prov, district, name, occupation, centralnominated, percentage, result, defeat)

# Soc Trang
plan %>% 
  filter(prov == "Soc Trang") %>%
  select(prov, year, net.trans, net.trans.change, defeat)

candidates2016 %>% 
  filter(prov == "Soc Trang", district == "2") %>%
  select(prov, district, name, occupation, centralnominated, percentage, result, defeat)

