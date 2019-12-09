library(haven)
library(xlsx)
library(dplyr)
library(ggplot2)
library(stringr)
library(vietnamcode)
library(vietnamdata)

setwd("../Candidate Profile/")
source("../../Code/Spring 2018/Functions.R")

# helper function to identify candidate's last term in office
lastterm <- function(x) {
    terms <- unlist(stringr::str_split(x, ","))
    terms <- gsub(" -", " ", terms)
    terms <- gsub("-", " ", terms)

    last <- ifelse(str_trim(terms[length(terms)])=="", str_trim(terms[length(terms)-1]), str_trim(terms[length(terms)]))
    return(last)
}

##### 2007 ######

#### Import Profiles

profile2007 <- read.xlsx2("Profile_2007_coded.xls", stringsAsFactors=F, sheetIndex=1)

names(profile2007)[1:17] <- c("prov", "district", "id", "name",
                              "birthdate", "gender", "hometown", "residency",
                              "ethnicity", "religion", "education", "professional.lvl",
                              "occupation", "job.location", "party.entrance.date", "prev.terms",
                              "people.council")
head(profile2007)


### clean some string variables
profile2007$name <- str_trim(gsub("\\(.*\\)", "", profile2007$name))

profile2007$prov <- vietnamcode(profile2007$prov, origin = "province_name", destination = "province_name")

### create relevant variables - names follow Malesky's
profile2007$age <- 2007 - year(profile2007$birthdate)

profile2007$male <- as.numeric(str_trim(profile2007$gender)=="Nam")

profile2007$minority <- as.numeric(str_trim(profile2007$ethnicity)!="Kinh")

profile2007$religion_any <- as.numeric(str_trim(profile2007$religion)!="Khong")

profile2007$education <- str_trim(profile2007$education)
# note: Malesky coded "Vien si" as Assistant Professor and miscoded some College cases as Professor
# note: Malesky coded "Cao Dang", "Tot Nghiep truong si quan phao binh" as "High School or below"
profile2007$degree[grepl("^[0-9]|[0-9]$|[Tr]ung cap|[Tr]ung hoc", profile2007$education)|profile2007$education==""] <- "High School or Below"
profile2007$degree[grepl("^[Dd]ai hoc|^[Cc]ao cap.|^[Tt]rung [Cc]ao cap.|^Tot nghiep.|^[Cc]ao dang", profile2007$education)] <- "College"
profile2007$degree[grepl("^[Tt]hac s.", profile2007$education)] <- "Masters"
profile2007$degree[grepl("^[Tt]ien s.|Tren Dai hoc|Sau Dai hoc", profile2007$education)] <- "PhD"
profile2007$degree[grepl("^[Pp]ho [Gg]iao su.", profile2007$education)] <- "Assistant Professor"
profile2007$degree[grepl("^.iao su.|[Vv]ien s.", profile2007$education)] <- "Professor"
profile2007$degree <- ordered(profile2007$degree,
                              levels=c("High School or Below", "College", "Masters", "PhD", "Assistant Professor", "Professor"))

# new (and better) degree variable that uses info from professional.lvl column)
profile2007$professional.lvl <- str_trim(profile2007$professional.lvl)
# note: Malesky coded "Vien si" as Assistant Professor and miscoded some College cases as Professor
# note: Malesky coded "Cao Dang", "Tot Nghiep truong si quan phao binh" as "High School or below"
profile2007$degree2[grepl("[0-9]/[0-9]|[Tr]ung cap|[Tr]ung hoc|quan ly [Gg]iam doc", profile2007$professional.lvl)|profile2007$professional.lvl==""] <- "High School or Below"
profile2007$degree2[grepl("^[Dd]ai hoc|^[Tt]rung [Cc]ao cap.|[Cc]ao cap.+chinh tri|^[Cc]ao dang|[Cc]u nhan|Bac s[yi]|BS", profile2007$professional.lvl)] <- "College"
profile2007$degree2[grepl("[Tt]hac s.", profile2007$professional.lvl)] <- "Masters"
profile2007$degree2[grepl("[Tt]ien s.|Tren Dai hoc|Sau Dai hoc", profile2007$professional.lvl)] <- "PhD"
profile2007$degree2[grepl("[Pp]ho [Gg]iao su.", profile2007$professional.lvl)] <- "Assistant Professor"
profile2007$degree2[grepl("^.iao su.|[Vv]ien s.", profile2007$professional.lvl)] <- "Professor"
profile2007$degree2 <- ordered(profile2007$degree2,
                              levels=c("High School or Below", "College", "Masters", "PhD", "Assistant Professor", "Professor"))
profile2007$degree2 <- pmax(profile2007$degree, profile2007$degree2, na.rm=T) # verified: in cases of discrepancies, higher value is correct

profile2007$party_yearentrance <- year(profile2007$party.entrance.date)

profile2007$party <- ifelse(is.na(profile2007$party_yearentrance),0,1)

profile2007$years_party <- 2007 - profile2007$party_yearentrance

profile2007$incumbencyterms <- sapply(str_split(profile2007$prev.terms, ","), length)*(profile2007$prev.terms!="")
profile2007$incumbent_dummy <- as.numeric(profile2007$incumbencyterms > 0)

# local legislature = 1 if in provincial legislature (or "municipal" legislature for Cat I cities)
profile2007$locallegislature	<- ifelse(grepl(".[Tt]inh.", profile2007$people.council) |
  (grepl(".[Tt]hanh pho.", profile2007$people.council) &
     profile2007$prov %in%
     c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2007$politburo <- ifelse(grepl("[Bb]o [Cc]hinh tri|[Tt]ong [Bb]i thu", profile2007$occupation), 1, 0)

profile2007$partysecretariat <- ifelse(grepl("[Bb]i thu (TW|[Tt]rung [Uu]ong) [Dd]ang|[Tt]ong [Bb]i thu", profile2007$occupation), 1, 0)

# needs some more work - check carefully cases that are not
profile2007$centralcommittee <- ifelse(grepl("((BCH|[Bb]an [Cc]hap [Hh]anh|[Bb]i thu|[Uu]y vien du khuyet)( )?(TW|[Tt]rung [Uu]ong) [Dd]ang)|([Bb]o [Tt]ruong)", profile2007$occupation) |
                                         (profile2007$politburo==1), 1, 0)

profile2007$localpartysecretariat  <- ifelse(grepl("[Tt]inh [Uu]y", profile2007$occupation) |
  (grepl("[Tt]hanh [Uu]y", profile2007$occupation) &
     profile2007$prov %in%
     c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2007$localpcom <- ifelse(grepl("(UBND|[Uu]y ban nhan dan) tinh", profile2007$occupation) |
                                  (grepl("(UBND|[Uu]y ban nhan dan) (TP|[Tt]hanh pho)", profile2007$occupation) &
                                     profile2007$prov %in%
                                     c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2007$business <- as.numeric(profile2007$business)

profile2007$centralnominated <- as.numeric(profile2007$centralnominated)

profile2007 <- profile2007 %>%
  group_by(prov,district) %>%
  dplyr::mutate(candidates_district =as.numeric(max(id)==5))

profile2007$homeprov <- sapply(profile2007$hometown, lastterm)
profile2007$homeprov <- vietnamcode(profile2007$homeprov, origin = "province_name", destination = "province_name")
# district can be matched using last two words
profile2007$que_districtsame <- as.numeric(word(profile2007$homeprov, start=-2, end=-1) == word(profile2007$prov, start=-2, end=-1))

profile2007 <- profile2007 %>%
  dplyr::mutate(power = locallegislature + politburo + centralcommittee +
           localpartysecretariat + localpcom + centralnominated + incumbent_dummy) %>%
  dplyr::group_by(prov, district) %>%
  dplyr::mutate(power_total = sum(power) + 1 - power)

##### 2011 ######

#### Import Profiles

profile2011 <- read.xlsx2("Profile_2011_coded.xls", stringsAsFactors=F, sheetIndex=1)

names(profile2011)[1:17] <- c("prov", "district", "id", "name",
                              "birthdate", "gender", "hometown", "residency",
                              "ethnicity", "religion", "education", "professional.lvl",
                              "occupation", "job.location", "party.entrance.date", "prev.terms",
                              "people.council")
head(profile2011)

### clean some string variables
profile2011$name <- str_trim(gsub("\\(.*\\)", "", profile2011$name))

profile2011$prov <- vietnamcode(profile2011$prov, origin = "province_name", destination = "province_name")

### create relevant variables - names follow Malesky's
profile2011$birthyear <- year(profile2011$birthdate)

profile2011$age <- 2011 - year(profile2011$birthdate)

profile2011$male <- as.numeric(str_trim(profile2011$gender)=="Nam")

profile2011$minority <- as.numeric(str_trim(profile2011$ethnicity)!="Kinh")

profile2011$religion_any <- as.numeric(str_trim(profile2011$religion)!="Khong")

profile2011$degree <- str_trim(profile2011$education)
# note: Malesky coded "Vien si" as Assistant Professor and miscoded some College cases as Professor
profile2011$degree[grepl("^.iao su.|[Vv]ien s.", profile2011$degree)] <- "Professor"
profile2011$degree[grepl("^[Pp]ho [Gg]iao su.", profile2011$degree)] <- "Assistant Professor"
profile2011$degree[grepl("^[Tt]ien s.|Tren Dai hoc|Sau Dai hoc", profile2011$degree)] <- "PhD"
profile2011$degree[grepl("^[Tt]hac s.", profile2011$degree)] <- "Masters"
# note: Malesky coded "Cao Dang", "Tot Nghiep truong si quan phao binh" as "High School or below"
profile2011$degree[grepl("^[Dd]ai hoc|^[Cc]ao cap.|^[Tt]rung [Cc]ao cap.|^Tot nghiep.|^[Cc]ao dang", profile2011$degree)] <- "College"
profile2011$degree[grepl("^[0-9]|[0-9]$|[Tr]ung cap|[Tr]ung hoc", profile2011$degree)|profile2011$degree==""] <- "High School or Below"
profile2011$degree <- ordered(profile2011$degree,
                              levels=c("High School or Below", "College", "Masters", "PhD", "Assistant Professor", "Professor"))

# new (and better) degree variable that uses info from professional.lvl column)
profile2011$professional.lvl <- str_trim(profile2011$professional.lvl)
# note: Malesky coded "Vien si" as Assistant Professor and miscoded some College cases as Professor
# note: Malesky coded "Cao Dang", "Tot Nghiep truong si quan phao binh" as "High School or below"
profile2011$degree2[grepl("[0-9]/[0-9]|[Tr]ung cap|[Tr]ung hoc|quan ly [Gg]iam doc", profile2011$professional.lvl)|profile2011$professional.lvl==""] <- "High School or Below"
profile2011$degree2[grepl("^[Dd]ai hoc|^[Tt]rung [Cc]ao cap.|[Cc]ao cap.+chinh tri|^[Cc]ao dang|[Cc]u nhan|Bac s[yi]|BS", profile2011$professional.lvl)] <- "College"
profile2011$degree2[grepl("[Tt]hac s.", profile2011$professional.lvl)] <- "Masters"
profile2011$degree2[grepl("[Tt]ien s.|Tren Dai hoc|Sau Dai hoc", profile2011$professional.lvl)] <- "PhD"
profile2011$degree2[grepl("[Pp]ho [Gg]iao su.", profile2011$professional.lvl)] <- "Assistant Professor"
profile2011$degree2[grepl("^.iao su.|[Vv]ien s.", profile2011$professional.lvl)] <- "Professor"
profile2011$degree2 <- ordered(profile2011$degree2,
                               levels=c("High School or Below", "College", "Masters", "PhD", "Assistant Professor", "Professor"))
profile2011$degree2 <- pmax(profile2011$degree, profile2011$degree2, na.rm=T) # verified: in cases of discrepancies, higher value is correct


profile2011$party_yearentrance <- year(profile2011$party.entrance.date)

profile2011$party <- ifelse(is.na(profile2011$party_yearentrance),0,1)

profile2011$years_party <- 2011 - profile2011$party_yearentrance

profile2011$incumbencyterms <- sapply(str_split(profile2011$prev.terms, ","), length)*(profile2011$prev.terms!="")
profile2011$incumbent_dummy <- as.numeric(profile2011$incumbencyterms > 0)

# local legislature = 1 if in provincial legislature (or "municipal" legislature for Cat I cities)
profile2011$locallegislature  <- ifelse(grepl(".[Tt]inh.", profile2011$people.council) |
                                          (grepl(".[Tt]hanh pho.", profile2011$people.council) &
                                             profile2011$prov %in%
                                             c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2011$politburo <- ifelse(grepl("[Bb]o [Cc]hinh tri|[Tt]ong [Bb]i thu", profile2011$occupation), 1, 0)

profile2011$partysecretariat <- ifelse(grepl("[Bb]i thu (TW|[Tt]rung [Uu]ong) [Dd]ang|[Tt]ong [Bb]i thu", profile2011$occupation), 1, 0)

# needs some more work - check carefully cases that are not
profile2011$centralcommittee <- ifelse(grepl("((BCH|[Bb]an [Cc]hap [Hh]anh|[Bb]i thu|[Uu]y vien du khuyet)( )?(TW|[Tt]rung [Uu]ong) [Dd]ang)|([Bb]o [Tt]ruong)", profile2011$occupation) |
                                         (profile2011$politburo==1), 1, 0)

profile2011$localpartysecretariat  <- ifelse(grepl("[Tt]inh [Uu]y", profile2011$occupation) |
                                               (grepl("[Tt]hanh [Uu]y", profile2011$occupation) &
                                                  profile2011$prov %in%
                                                  c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2011$localpcom <- ifelse(grepl("(UBND|[Uu]y ban nhan dan) tinh", profile2011$occupation) |
                                  (grepl("(UBND|[Uu]y ban nhan dan) (TP|[Tt]hanh pho)", profile2011$occupation) &
                                     profile2011$prov %in%
                                     c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2011$business <- as.numeric(profile2011$business)

profile2011$centralnominated <- as.numeric(profile2011$centralnominated)

profile2011 <- profile2011 %>%
  group_by(prov,district) %>%
  dplyr::mutate(candidates_district = ifelse(max(id)==3, 2,
                                             ifelse(max(id) == 5, 1,
                                                    0)))

profile2011$homeprov <- sapply(profile2011$hometown, lastterm)
profile2011$homeprov <- vietnamcode(profile2011$homeprov, origin = "province_name", destination = "province_name")
# district can be matched using last two words
profile2011$que_districtsame <- as.numeric(word(profile2011$homeprov, start=-2, end=-1) == word(profile2011$prov, start=-2, end=-1))

profile2011 <- profile2011 %>%
  dplyr::mutate(power = locallegislature + politburo + centralcommittee +
                  localpartysecretariat + localpcom + centralnominated + incumbent_dummy) %>%
  dplyr::group_by(prov, district) %>%
  dplyr::mutate(power_total = sum(power) + 1 - power)

##### 2016 ######

#### Import Profiles

profile2016 <- read.xlsx2("Profile_2016_coded.xls", stringsAsFactors=F, sheetIndex=1)

names(profile2016)[1:19] <- c("prov", "district", "id", "name",
                              "birthdate", "gender", "hometown", "residency",
                              "ethnicity", "religion", "education_basic", "education_higher", "professional.lvl", "theory.lvl",
                              "occupation", "job.location", "party.entrance.date", "prev.terms",
                              "people.council")
head(profile2016)

### clean some string variables
profile2016$name <- str_trim(gsub("\\(.*\\)", "", profile2016$name))

profile2016$prov <- vietnamcode(profile2016$prov, origin = "province_name", destination = "province_name")

### create relevant variables - names follow Malesky's
profile2016$birthyear <- year(profile2016$birthdate)

profile2016$age <- 2016 - year(profile2016$birthdate)

profile2016$male <- as.numeric(str_trim(profile2016$gender)=="Nam")

profile2016$minority <- as.numeric(str_trim(profile2016$ethnicity)!="Kinh")

profile2016$religion_any <- as.numeric(str_trim(profile2016$religion)!="Khong")

# degree variable similar to 2011 and 2007 not possible

profile2016$professional.lvl <- str_trim(profile2016$professional.lvl)
profile2016$professional.lvl[which(profile2016$professional.lvl %in% c("", "Khong", "Nghe si nhan dan"))] <-
  profile2016$education_higher[which(profile2016$professional.lvl %in% c("", "Khong", "Nghe si nhan dan"))]

profile2016$degree2[grepl("[0-9]/[0-9]|[Tr]ung cap|[Tr]ung hoc|quan ly [Gg]iam doc", profile2016$professional.lvl)|profile2016$professional.lvl==""] <- "High School or Below"
profile2016$degree2[grepl("^[Dd]ai hoc|^[Tt]rung [Cc]ao cap.|[Cc]ao cap.+chinh tri|[Cc]ao dang|[Cc]u nhan|Bac s[yi]|BS|Duoc si|[Kk][yi] su|[Hh]oc [Vv]ien", profile2016$professional.lvl)] <- "College"
profile2016$degree2[grepl("[Tt]hac s.", profile2016$professional.lvl)] <- "Masters"
profile2016$degree2[grepl("[Tt]ien s.|Tren Dai hoc|Sau Dai hoc", profile2016$professional.lvl)] <- "PhD"
profile2016$degree2[grepl("[Pp]ho [Gg]iao su.", profile2016$professional.lvl)] <- "Assistant Professor"
profile2016$degree2[grepl("^.iao su.|[Vv]ien s.", profile2016$professional.lvl)] <- "Professor"
profile2016$degree2[is.na(profile2016$degree2)] <- "High School or Below" # verified: all remaining NAs are high school or below
profile2016$degree2 <- ordered(profile2016$degree2,
                               levels=c("High School or Below", "College", "Masters", "PhD", "Assistant Professor", "Professor"))


profile2016$party_yearentrance <- year(profile2016$party.entrance.date)

profile2016$party <- ifelse(is.na(profile2016$party_yearentrance),0,1)

profile2016$years_party <- 2016 - profile2016$party_yearentrance

profile2016$incumbencyterms <- sapply(str_split(profile2016$prev.terms, ","), length)*(profile2016$prev.terms!="")
profile2016$incumbent_dummy <- as.numeric(profile2016$incumbencyterms > 0)

# local legislature = 1 if in provincial legislature (or "municipal" legislature for Cat I cities)
profile2016$locallegislature  <- ifelse(grepl(".[Tt]inh.", profile2016$people.council) |
                                          (grepl(".[Tt]hanh pho.", profile2016$people.council) &
                                             profile2016$prov %in%
                                             c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2016$politburo <- ifelse(grepl("[Bb]o [Cc]hinh tri|[Tt]ong [Bb]i thu", profile2016$occupation), 1, 0)

# note: Nguyen Van Binh was not member of party secretariat until June 2016; Luong Cuong was not in NA
profile2016$partysecretariat <- ifelse(grepl("[Bb]i thu (TW|[Tt]rung [Uu]ong) [Dd]ang|[Tt]ong [Bb]i thu|[Tt]huong truc [Bb]an [Bb]i thu,", profile2016$occupation), 1, 0)
# needs some more work - check carefully cases that are not
profile2016$centralcommittee <- ifelse(grepl("((BCH|[Bb]an [Cc]hap [Hh]anh|[Bb]i thu|[Uu]y vien du khuyet|[Uu]y vien)( )?(TW|[Tt]rung [Uu]ong) [Dd]ang)|([Bb]o [Tt]ruong)", profile2016$occupation) |
                                         (profile2016$politburo==1), 1, 0)

profile2016$localpartysecretariat  <- ifelse(grepl("[Tt]inh [Uu]y", profile2016$occupation) |
                                               (grepl("[Tt]hanh [Uu]y", profile2016$occupation) &
                                                  profile2016$prov %in%
                                                  c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2016$localpcom <- ifelse(grepl("(UBND|[Uu]y ban nhan dan) tinh", profile2016$occupation) |
                                  (grepl("(UBND|[Uu]y ban nhan dan) (TP|[Tt]hanh pho)", profile2016$occupation) &
                                     profile2016$prov %in%
                                     c("Ha Noi", "Ho Chi Minh City", "Hai Phong", "Da Nang", "Can Tho")),1,0)

profile2016$business <- as.numeric(profile2016$business)

profile2016$centralnominated <- as.numeric(profile2016$centralnominated==1)

profile2016 <- profile2016 %>%
  group_by(prov,district) %>%
  dplyr::mutate(candidates_district = ifelse(max(id)==3, 2,
                                             ifelse(max(id) == 5, 1,
                                                    0)))
# 2 districts in Soc Trang with 2-5 ratio instead of 3-5
profile2016$candidates_district[profile2016$prov=="Soc Trang" & profile2016$district>1] <- -1

profile2016$homeprov <- sapply(profile2016$hometown, lastterm)
profile2016$homeprov <- vietnamcode(profile2016$homeprov, origin = "province_name", destination = "province_name")
# district can be matched using last two words
profile2016$que_districtsame <- as.numeric(word(profile2016$homeprov, start=-2, end=-1) == word(profile2016$prov, start=-2, end=-1))

profile2016 <- profile2016 %>%
  dplyr::mutate(power = locallegislature + politburo + centralcommittee +
                  localpartysecretariat + localpcom + centralnominated + incumbent_dummy) %>%
  dplyr::group_by(prov, district) %>%
  dplyr::mutate(power_total = sum(power) + 1 - power)
