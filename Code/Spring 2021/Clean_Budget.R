library(haven)
library(xlsx)
library(dplyr)
library(ggplot2)
library(vietnamcode)
library(vietnamdata)

setwd("../Vietnam National Budget/")
source("../../Code/Spring 2021/Functions.R")

#### Import Budget Plans and Finals

# guides contains info on which rows/columns to keep
guide.plan <- read.csv("Plan_Import.csv", stringsAsFactors=F)
guide.final <- read.csv("Final_Import.csv", stringsAsFactors=F)
guide.prov <- read.csv("../Vietnam Provincial Budget/Plan/Province_Import.csv", stringsAsFactors=F)

# read in budget and plan finals for every year using guides
plan <- do.call(bind_rows, lapply(1:nrow(guide.plan), function(i) {
  x <- guide.plan[i,]

  file.plan <- paste("Plan_", x$year, ".xls", sep="")

  # plan <- read.xlsx2(file.plan, sheetIndex=x$sheetIndex, startRow=x$startRow,
  #                    colClasses=c("numeric", "character", rep("numeric", 15)))
  # plan <- plan[find.numeric(plan[,1]), 
  #              eval(parse(text=paste("c(",x$keepCol,")", sep="")))]
  # plan[,1] <- as.character(plan[,1])
  
  if(x$year %in% c(2003, 2004)) {
    plan <- readxl::read_excel(file.plan,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 6)))
  } else if(x$year %in% c(2005, 2006)) {
    plan <- readxl::read_excel(file.plan,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 13)))
  } else if(x$year %in% c(2007, 2008, 2009, 2013,2014,2015, 2016)) {
    plan <- readxl::read_excel(file.plan,
                               sheet = x$sheetIndex,
                               skip = x$startRow,
                               col_names = FALSE,
                               col_types = c("text", "text",
                                             rep("numeric", 9)))
  } else if(x$year %in% c(2011,2012)) {
    plan <- readxl::read_excel(file.plan,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 8)))
  } else if(x$year %in% c(2017)) {
    plan <- readxl::read_excel(file.plan,
                               sheet = x$sheetIndex,
                               skip = x$startRow,
                               col_names = FALSE,
                               col_types = c("text", "text",
                                             rep("numeric", 11)))
  } else if(x$year %in% c(2018,2019)) {
    plan <- readxl::read_excel(file.plan,
                               sheet = x$sheetIndex,
                               skip = x$startRow,
                               col_names = FALSE,
                               col_types = c("text", "text",
                                             rep("numeric", 12)))
  } else {
    plan <- readxl::read_excel(file.plan,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE)
  }
  
  # plan <- readxl::read_excel(file.plan,
  #                             sheet = x$sheetIndex,
  #                             skip = x$startRow,
  #                             col_names = FALSE)
  plan <- plan[find.numeric(plan[[1]]), 
                 na.omit(as.numeric(x[1,5:21]))]
  plan[[1]] <- as.character(plan[[1]])

  # Note: last 3 columns not usable -- inconsistent across years
  names(plan) <-  c("prov", 
    "total.rev", 
    "ratio", 
    "total.exp", 
    "additional.transfer", 
    "balance.transfer", 
    "minimum.wage.trans", 
    "target.trans", 
    "target.trans.foreign", 
    "target.trans.missions", 
    "target.trans.policies", 
    "target.trans.programs", 
    "target.trans.other.programs", 
    "target.trans.national.programs", 
    "target.trans.135", 
    "target.trans.forestation", 
    "target.trans.salary")[!is.na(as.numeric(x[1,5:21]))]
  
  
  if(x$year <= 2004) {
    # for 2003 and 2004, target transfers contain only one category
    plan[["target.trans"]] <- plan[["target.trans.programs"]]
  } else if(x$year %in% c(2005)) {
    # for 2005, target transfers towards programs are not available
    # but subcomponents are
    plan[["target.trans.programs"]] <- plan[["target.trans.other.programs"]] +
      plan[["target.trans.national.programs"]] +
      plan[["target.trans.135"]] +
      plan[["target.trans.forestation"]]
  } else if(x$year %in% c(2006)) {
    # for 2006, target transfers towards programs are not available
    # but subcomponents are
    plan[["target.trans.programs"]] <- plan[["target.trans.national.programs"]] +
      plan[["target.trans.135"]] +
      plan[["target.trans.forestation"]] +
      plan[["target.trans.salary"]]
  } else if (x$year %in% c(2013, 2014, 2015, 2016)) {
    # for 2013-2016, additional transfers are not available but subcomponents are
    plan[["additional.transfer"]] <- plan[["balance.transfer"]] +
      plan[["minimum.wage.trans"]] +
      plan[["target.trans"]]
  } else if (x$year %in% c(2017)) {
    # same for 2017, but here minimum wage transfers are presumed 0
    plan[["additional.transfer"]] <- plan[["balance.transfer"]] +
      plan[["target.trans"]]
  }
  
  plan[is.nan(plan)] <- 0
  plan$net.trans <- plan$total.exp - plan$total.rev
  plan$trans_rev <- plan$total.exp/plan$total.rev * 100 # as specified by Malesky and Schuler (2011)
  plan$year <- x$year

  return(plan)
}))

final <- do.call(bind_rows, lapply(1:nrow(guide.final), function(i) {
  x <- guide.final[i,]

  file.final <- paste("Final_", x$year, ".xls", sep="")

  # adjust col_types to match with input data for each year
  if(x$year %in% c(2001)) {
    final <- readxl::read_excel(file.final,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 6)))
  } else if(x$year %in% c(2009,2010)) {
    final <- readxl::read_excel(file.final,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 6)))
  } else if(x$year %in% c(2011,2012,2013,2014, 2015, 2016)) {
    final <- readxl::read_excel(file.final,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE,
                                col_types = c("text", "text",
                                              rep("numeric", 8)))
  } else {
    final <- readxl::read_excel(file.final,
                                sheet = x$sheetIndex,
                                skip = x$startRow,
                                col_names = FALSE)
  }

  # final <- read.xlsx2(file.final,
  #                     sheetIndex=x$sheetIndex,
  #                     startRow=x$startRow,
  #                     header = FALSE,
  #                    colClasses=c("numeric", "character", rep("character", 9)))
  final <- final[find.numeric(final[[1]]), 
                 na.omit(as.numeric(x[1,5:13]))]
  final[[1]] <- as.character(final[[1]])

  names(final) <- c("prov", "total.rev", "ratio", "total.exp", 
                    "additional.transfer", "balance.trans", "target.trans",
                    "target.trans.foreign", "target.trans.missions")[!is.na(as.numeric(x[1,5:13]))]

  # for 2011 onwards, target transfer is reported in 2 columns
  if(x$year > 2010) final[["target.trans"]] <- final[["target.trans.foreign"]] + 
    final[["target.trans.missions"]]

  final[is.nan(final)] <- 0
  
  # from 2017 onwards revenue and expenditure are no longer reported
  if(x$year <= 2016) {
    final$net.trans <- as.numeric(final$total.exp) - as.numeric(final$total.rev)
    final$trans_rev <- final$total.exp/final$total.rev * 100 # as specified by Malesky and Schuler (2011)
  }
  final$year <- x$year

  return(final)
}))

prov <- do.call(bind_rows, lapply(1:nrow(guide.prov), function(i) {
  x <- guide.prov[i,]

  file.prov <- paste("../Vietnam Provincial Budget/Plan/MOF Budget ", x$year, ".xlsx", sep="")

  prov <- read.xlsx2(file.prov, sheetIndex=x$sheetIndex, startRow=x$startRow,
                      colClasses=c("numeric", "character", rep("numeric", 3), "character"))
  
  #prov <- prov[find.numeric(prov[,1]),]
  prov[,2] <- as.character(prov[,2])
  
  if(ncol(prov) == 5) prov <- cbind(prov, "")
  
  names(prov) <- c("year", "prov", "total.exp", "dev.exp", "admin.exp", "note")

  prov[is.nan(prov)] <- NA

  prov <- prov %>% 
    mutate_at(c("total.exp", "dev.exp", "admin.exp"), .funs = ~ifelse(note %in% c("", "from PDF"), ., NA))
  
  return(prov)
}))

# clean up province names using a look up table
# provnames <- read.csv("Provinces.csv", stringsAsFactors=F)
#
# plan$prov <- lookup.clean(plan$prov, provnames)
# final$prov <- lookup.clean(final$prov, provnames)
# prov$prov <- lookup.clean(prov$prov, provnames)

# clean up province names using the Vietnam Code function
plan$prov <- vietnamcode(plan$prov, origin = "province_name", destination = "province_name")
final$prov <- vietnamcode(final$prov, origin = "province_name", destination = "province_name")
prov$prov <- vietnamcode(prov$prov, origin = "province_name", destination = "province_name")

# create leads and lags, changes, percentage changes variables
plan <- plan %>%
  group_by(prov) %>%
  mutate(net.recipient = as.numeric(net.trans > 0)) %>%
  mutate(net.trans.log = neglog(net.trans)) %>%
  mutate(trans_rev.log = neglog(trans_rev)) %>%
  mutate(target.trans.log = neglog(target.trans)) %>%
  mutate(total.rev.log = neglog(total.rev)) %>%
  # one year lag
  mutate(net.trans.lag = lag(net.trans, order_by=year)) %>%
  mutate(trans_rev.lag = lag(trans_rev, order_by=year)) %>%
  mutate(target.trans.lag = lag(target.trans, order_by=year)) %>%
  mutate(total.rev.lag = lag(total.rev, order_by=year)) %>%
  mutate(net.trans.log.lag = lag(net.trans.log, order_by=year)) %>%
  mutate(trans_rev.log.lag = lag(trans_rev.log, order_by=year)) %>%
  mutate(target.trans.log.lag = lag(target.trans.log, order_by=year)) %>%
  mutate(total.rev.log.lag = lag(total.rev.log, order_by=year)) %>%
  mutate(total.rev.change = total.rev - total.rev.lag) %>%
  mutate(total.rev.change.log = neglog(total.rev.change)) %>%
  mutate(total.rev.log.change = total.rev.log - total.rev.log.lag) %>%
  mutate(total.rev.change.pct = total.rev.change/abs(total.rev.lag)) %>%
  mutate(net.trans.change = net.trans - net.trans.lag) %>%
  mutate(net.trans.change.log = neglog(net.trans.change)) %>%
  mutate(net.trans.log.change = net.trans.log - net.trans.log.lag) %>%
  mutate(net.trans.change.pct = net.trans.change/abs(net.trans.lag)) %>%
  mutate(net.trans.change.lag = lag(net.trans.change, order_by=year)) %>%
  mutate(trans_rev.change = trans_rev - trans_rev.lag) %>%
  mutate(trans_rev.change.log = neglog(trans_rev.change)) %>%
  mutate(trans_rev.log.change = trans_rev.log - trans_rev.log.lag) %>%
  mutate(trans_rev.change.pct = trans_rev.change/abs(trans_rev.lag)) %>%
  mutate(target.trans.change = target.trans - target.trans.lag) %>%
  mutate(target.trans.change.log = neglog(target.trans.change)) %>%
  mutate(target.trans.log.change = target.trans.log - target.trans.log.lag) %>%
  mutate(target.trans.change.pct = target.trans.change/abs(target.trans.lag)) %>%
  mutate(pct.change.rev = net.trans.change/total.rev) %>%
  # 2 year lag
  mutate(net.trans.lag2 = lag(net.trans, n=2, order_by=year)) %>%
  mutate(trans_rev.lag2 = lag(trans_rev, n=2, order_by=year)) %>%
  mutate(target.trans.lag2 = lag(target.trans, n=2, order_by=year)) %>%
  mutate(net.trans.change.lag2 = lag(net.trans.change, n=2, order_by = year)) %>%
  mutate(net.trans.log.lag2 = lag(net.trans.log, n=2, order_by=year)) %>%
  mutate(trans_rev.log.lag2 = lag(trans_rev.log, n=2, order_by=year)) %>%
  mutate(target.trans.log.lag2 = lag(target.trans.log, n=2, order_by=year)) %>%
  mutate(total.rev.log.lag2 = lag(total.rev.log, n=2, order_by=year)) %>%
  mutate(total.rev.lag2 = lag(total.rev, n=2, order_by=year)) %>%
  mutate(total.rev.change2 = total.rev - total.rev.lag2) %>%
  mutate(total.rev.change2.log = neglog(total.rev.change2)) %>%
  mutate(total.rev.log.change2 = total.rev.log - total.rev.log.lag2) %>%
  mutate(total.rev.change2.pct = total.rev.change2/abs(total.rev.lag2)) %>%
  mutate(net.trans.change2 = net.trans - net.trans.lag2) %>%
  mutate(net.trans.change2.log = neglog(net.trans.change2)) %>%
  mutate(net.trans.log.change2 = net.trans.log - net.trans.log.lag2) %>%
  mutate(net.trans.change2.pct = net.trans.change2/abs(net.trans.lag2)) %>%
  mutate(trans_rev.change2 = trans_rev - trans_rev.lag2) %>%
  mutate(trans_rev.change2.log = neglog(trans_rev.change2)) %>%
  mutate(trans_rev.log.change2 = trans_rev.log - trans_rev.log.lag2) %>%
  mutate(trans_rev.change2.pct = trans_rev.change2/abs(trans_rev.lag2)) %>%
  mutate(target.trans.change2 = target.trans - target.trans.lag2) %>%
  mutate(target.trans.change2.log = neglog(target.trans.change2)) %>%
  mutate(target.trans.log.change2 = target.trans.log - target.trans.log.lag2) %>%
  mutate(target.trans.change2.pct = target.trans.change/abs(target.trans.lag2))


final <- final %>%
  group_by(prov) %>%
  mutate(net.recipient = as.numeric(net.trans > 0)) %>%
  mutate(net.trans.log = neglog(net.trans)) %>%
  mutate(trans_rev.log = neglog(trans_rev)) %>%
  mutate(total.rev.log = neglog(total.rev)) %>%
  # one year lag
  mutate(net.trans.lag = lag(net.trans, order_by=year)) %>%
  mutate(trans_rev.lag = lag(trans_rev, order_by=year)) %>%
  mutate(total.rev.lag = lag(total.rev, order_by=year)) %>%
  mutate(net.trans.log.lag = lag(net.trans.log, order_by=year)) %>%
  mutate(trans_rev.log.lag = lag(trans_rev.log, order_by=year)) %>%
  mutate(total.rev.log.lag = lag(total.rev.log, order_by=year)) %>%
  mutate(total.rev.change = total.rev - total.rev.lag) %>%
  mutate(total.rev.change.log = neglog(total.rev.change)) %>%
  mutate(total.rev.log.change = total.rev.log - total.rev.log.lag) %>%
  mutate(total.rev.change.pct = total.rev.change/abs(total.rev.lag)) %>%
  mutate(net.trans.change = net.trans - net.trans.lag) %>%
  mutate(net.trans.change.log = neglog(net.trans.change)) %>%
  mutate(net.trans.log.change = net.trans.log - net.trans.log.lag) %>%
  mutate(net.trans.change.pct = net.trans.change/abs(net.trans.lag)) %>%
  mutate(net.trans.change.lag = lag(net.trans.change, order_by=year)) %>%
  mutate(trans_rev.change = trans_rev - trans_rev.lag) %>%
  mutate(trans_rev.change.log = neglog(trans_rev.change)) %>%
  mutate(trans_rev.log.change = trans_rev.log - trans_rev.log.lag) %>%
  mutate(trans_rev.change.pct = trans_rev.change/abs(trans_rev.lag)) %>%
  mutate(pct.change.rev = net.trans.change/total.rev) %>%
  # 2 year lag
  mutate(net.trans.lag2 = lag(net.trans, n=2, order_by=year)) %>%
  mutate(trans_rev.lag2 = lag(trans_rev, n=2, order_by=year)) %>%
  mutate(net.trans.change.lag2 = lag(net.trans.change, n=2, order_by = year)) %>%
  mutate(net.trans.log.lag2 = lag(net.trans.log, n=2, order_by=year)) %>%
  mutate(trans_rev.log.lag2 = lag(trans_rev.log, n=2, order_by=year)) %>%
  mutate(total.rev.log.lag2 = lag(total.rev.log, n=2, order_by=year)) %>%
  mutate(total.rev.lag2 = lag(total.rev, n=2, order_by=year)) %>%
  mutate(total.rev.change2 = total.rev - total.rev.lag2) %>%
  mutate(total.rev.change2.log = neglog(total.rev.change2)) %>%
  mutate(total.rev.log.change2 = total.rev.log - total.rev.log.lag2) %>%
  mutate(total.rev.change2.pct = total.rev.change2/abs(total.rev.lag2)) %>%
  mutate(net.trans.change2 = net.trans - net.trans.lag2) %>%
  mutate(net.trans.change2.log = neglog(net.trans.change2)) %>%
  mutate(net.trans.log.change2 = net.trans.log - net.trans.log.lag2) %>%
  mutate(net.trans.change2.pct = net.trans.change2/abs(net.trans.lag2)) %>%
  mutate(trans_rev.change2 = trans_rev - trans_rev.lag2) %>%
  mutate(trans_rev.change2.log = neglog(trans_rev.change2)) %>%
  mutate(trans_rev.log.change2 = trans_rev.log - trans_rev.log.lag2) %>%
  mutate(trans_rev.change2.pct = trans_rev.change2/abs(trans_rev.lag2))
  
prov <- prov %>%
  group_by(prov) %>%
  mutate(total.exp.log = neglog(total.exp)) %>%
  mutate(dev.exp.log = neglog(dev.exp)) %>%
  mutate(admin.exp.log = neglog(admin.exp)) %>%
  # one year lag
  mutate(total.exp.lag = lag(total.exp, order_by=year)) %>%
  mutate(dev.exp.lag = lag(dev.exp, order_by=year)) %>%
  mutate(admin.exp.lag = lag(admin.exp, order_by=year)) %>%
  mutate(total.exp.log.lag = lag(total.exp.log, order_by=year)) %>%
  mutate(dev.exp.log.lag = lag(dev.exp.log, order_by=year)) %>%
  mutate(admin.exp.log.lag = lag(admin.exp.log, order_by=year)) %>%
  mutate(admin.exp.change = admin.exp - admin.exp.lag) %>%
  mutate(admin.exp.change.log = neglog(admin.exp.change)) %>%
  mutate(admin.exp.log.change = admin.exp.log - admin.exp.log.lag) %>%
  mutate(admin.exp.change.pct = admin.exp.change/abs(admin.exp.lag)) %>%
  mutate(total.exp.change = total.exp - total.exp.lag) %>%
  mutate(total.exp.change.log = neglog(total.exp.change)) %>%
  mutate(total.exp.log.change = total.exp.log - total.exp.log.lag) %>%
  mutate(total.exp.change.pct = total.exp.change/abs(total.exp.lag)) %>%
  mutate(dev.exp.change = dev.exp - dev.exp.lag) %>%
  mutate(dev.exp.change.log = neglog(dev.exp.change)) %>%
  mutate(dev.exp.log.change = dev.exp.log - dev.exp.log.lag) %>%
  mutate(dev.exp.change.pct = dev.exp.change/abs(dev.exp.lag)) %>%
  mutate(pct.change.rev = total.exp.change/admin.exp) %>%
  # 2 year lag
  mutate(total.exp.lag2 = lag(total.exp, n=2, order_by=year)) %>%
  mutate(dev.exp.lag2 = lag(dev.exp, n=2, order_by=year)) %>%
  mutate(admin.exp.lag2 = lag(admin.exp, n=2, order_by=year)) %>%
  mutate(total.exp.log.lag2 = lag(total.exp.log, n=2, order_by=year)) %>%
  mutate(dev.exp.log.lag2 = lag(dev.exp.log, n=2, order_by=year)) %>%
  mutate(admin.exp.log.lag2 = lag(admin.exp.log, n=2, order_by=year)) %>%
  mutate(admin.exp.change2 = admin.exp - admin.exp.lag2) %>%
  mutate(admin.exp.change2.log = neglog(admin.exp.change2)) %>%
  mutate(admin.exp.log.change2 = admin.exp.log - admin.exp.log.lag2) %>%
  mutate(admin.exp.change2.pct = admin.exp.change2/abs(admin.exp.lag2)) %>%
  mutate(total.exp.change2 = total.exp - total.exp.lag2) %>%
  mutate(total.exp.change2.log = neglog(total.exp.change2)) %>%
  mutate(total.exp.log.change2 = total.exp.log - total.exp.log.lag2) %>%
  mutate(total.exp.change2.pct = total.exp.change2/abs(total.exp.lag2)) %>%
  mutate(dev.exp.change2 = dev.exp - dev.exp.lag2) %>%
  mutate(dev.exp.change2.log = neglog(dev.exp.change2)) %>%
  mutate(dev.exp.log.change2 = dev.exp.log - dev.exp.log.lag2) %>%
  mutate(dev.exp.change2.pct = dev.exp.change2/abs(dev.exp.lag2))

# indicators for southern provinces
south <- read.csv("south.csv", stringsAsFactors=F)

plan <- merge(plan, south, by="prov", all.x=T)

final <- merge(final, south, by="prov", all.x=T)

prov <- merge(prov, south, by="prov", all.x=T)

# merge in revenue variables from plan to prov
prov <- prov %>%
  merge(plan %>% select(total.rev, total.rev.change, total.rev.lag, total.rev.log, total.rev.log.lag, prov, year), 
        by = c("prov", "year"),
        all.x = TRUE)

# save output
save(final, file = "National_Budget_Final_cleaned.RData")
save(plan, file = "National_Budget_Plan_cleaned.RData")
save(final, plan, file = "Vietnam_National_Budget.RData")

# some plots

ggplot(final, aes(y=net.trans/total.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(final, aes(y=total.rev, x = factor(year), group=prov)) +
  geom_line() +
  facet_wrap(. ~ prov, scales = "free_y") +
  theme_bw()

ggplot(final, aes(y=total.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(plan, aes(y=total.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(plan, aes(y=total.rev, x = factor(year), group=prov)) +
  geom_line() +
  facet_wrap(. ~ prov, scales = "free_y") +
  theme_bw()

ggplot(plan, aes(y=pct.change.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(prov, aes(y=dev.exp, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()
