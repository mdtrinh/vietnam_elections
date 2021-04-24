library(xlsx)
library(zoo)
library(tidyr)

setwd("../Results")
source("../../Code/Spring 2021/Functions.R")

###### 2007 ######

#### Import Results

result2007 <- read.xlsx2("Result_2007_no_diacritics.xlsx", stringsAsFactors=F, sheetIndex=1)
names(result2007) <- c("prov", "district", "district.name", "name", "percentage")

### clean relevant variables

result2007$prov <- fill.blanks(result2007$prov)

result2007$prov <- vietnamcode(result2007$prov, origin = "province_name", destination = "province_name")

result2007$district <- fill.blanks(result2007$district)

result2007$district.name <- fill.blanks(result2007$district.name)

result2007$name <- gsub("Linh muc ", "", gsub("\\s\\(.*\\)", "", result2007$name))

result2007$percentage <- as.numeric(gsub(",", ".", gsub("[a-zA-Z%\\s]*", "", result2007$percentage)))

###### 2011 ######

#### Import Results

result2011 <- read.xlsx2("Result_2011_no_diacritics.xlsx", stringsAsFactors=F, sheetIndex=1)[,2:6]
names(result2011) <- c("name", "birthdate", "gender", "prov", "percentage")

### clean relevant variables

result2011$name <- gsub("Linh muc ", "", gsub("\\s\\(.*\\)", "", result2011$name))

result2011$prov <- vietnamcode(result2011$prov, origin = "province_name", destination = "province_name")

result2011$birthyear <- year(result2011$birthdate)
result2011$percentage <- as.numeric(gsub(",", ".", gsub("[a-zA-Z%\\s]*", "", result2011$percentage)))

###### 2016 ######

#### Import Results

result2016 <- read.xlsx2("Result_2016_no_diacritics.xlsx", stringsAsFactors=F, sheetIndex=1)
names(result2016) <- c("prov", "district", "id", "name", "birthdate", "gender", "vote", "percentage", "result")

### clean relevant variables

result2016$name <- gsub("Linh muc ", "", gsub("\\s\\(.*\\)", "", result2016$name))

result2016$prov <- vietnamcode(result2016$prov, origin = "province_name", destination = "province_name")

result2016$birthyear <- year(result2016$birthdate)
result2016$percentage <- as.numeric(result2016$percentage)
