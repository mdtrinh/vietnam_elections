library(dplyr)
library(BenfordTests)

#setwd("/media/dropbox/dropbox/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
setwd("C:/Users/Minh Trinh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("D:/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")
#setwd("C:/Users/Nga Nguy/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Working Data")

source("../../Code/Spring 2018/Merge_All.R")

### MOVE THIS TO CLEAN_TURNOUT LATER
turnout2011 <- read.csv("../Results/Turnout_2011.csv")

turnout2011 <- turnout2011 %>%
  mutate(turnout = as.numeric(sub("%", "", sub(",", ".", turnout)))) %>%
  mutate(valid.votes = as.numeric(sub("\\s.*)", "", valid.votes))) %>%
  mutate(invalid.votes = as.numeric(sub("\\s.*)", "", invalid.votes))) %>%
  mutate(registered.voters = registered.voters * 1000,
         turnout.voters = turnout.voters * 1000,
         valid.votes = valid.votes * 1000,
         invalid.votes = invalid.votes * 1000^(invalid.votes%%1!=0))

### Visual Inspection

# PMF (actually PDF, for simplicity) and SD of Benford distribution
pbenford <- function(d, digit=1, base=10) {
  
  # calculate probability for individual digit using formula derived from Benford's law
  if(digit <= 5) {
    # probability is calculated by summing over all the digits that come before e.g. 100x - 999x
    range <- unique(c(floor(base^(digit-2)):floor(base^(digit-1) -1)))
    p <- sum(sapply(range, function(j) log(1 + 1/(10*j + d), base=10)))    
  }
  # for digit beyond the fifth, simply assume Uniform (close enough approx)
  else { 
    p <- punif(d, min=0, max=base-1)
  }
  return(p)
  
  # reference: http://press.princeton.edu/chapters/s10526.pdf
}

# standard deviation is calculated using Bernoulli
sdbenford <- function(d,n,digit=1,base=10) {sqrt(pbenford(d,digit,base)*(1-pbenford(d,digit,base))/n)}

# function to conduct digit-based tests using Benford's Law and plot result
digit_test <- function(num, digit=2, plot=TRUE, axe.y=FALSE, title=NULL, xlab=NULL, ylab=NULL) {
  require(ggplot2)
  
  # turn number into digits
  digits <- lapply(num, function(x) as.numeric(unlist(strsplit(as.character(x), ""))))
  
  # extract max number of digits, fill shorter numbers with NA
  length.max <- max(sapply(digits, length))
  digits <- lapply(digits, function(x) {
    length(x) <- length.max
    return(x)
  })
  
  # turn into matrix
  digits <- do.call(rbind,digits)
  colnames(digits) <- c(1:length.max)
  
  # number of obs
  n <- length(digits[,digit])
  
  # null distribution of digits
  if(digit == 1) {
    min <- 1
    df <- 8
  } else {
    min <- 0
    df <- 9
  }
  null <- sapply(c(min:9), function(d) pbenford(d, digit))
  null.lower <- sapply(c(min:9), function(d) pbenford(d, digit) - 1.96*sdbenford(d, n, digit))
  null.upper <- sapply(c(min:9), function(d) pbenford(d, digit) + 1.96*sdbenford(d, n, digit))
  
  # obtain empirical distribution of digits
  freq <- table(factor(digits[,digit], levels = min:9))
  
  # Chi square test (not accounting for multiple testing)
  chisq <- sum((freq - n*null)^2/(n*null))
  chisq.p <- pchisq(chisq, df, lower.tail = F)
  
  # KS test
  ks <- ks.test(freq, n*null)$statistic
  ks.p <- ks.test(freq, n*null)$p.value
  
  # plot result
  if(axe.y) {
    axes <- theme(axis.text.x=element_blank(), 
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none")
  } else {
    axes <- theme(axis.text=element_blank(), 
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none")
  }
  
  p <- ggplot(as.data.frame(cbind(x = c(min:9), freq = freq/n, null = null, lower = null.lower, upper = null.upper))) +
    geom_bar(aes(x=x, y=freq), stat="identity", fill="grey") +
    geom_line(aes(x=x, y = null), linetype="dashed", colour="black") +
    geom_ribbon(aes(x=x, ymin=lower, ymax=upper), alpha = .4) +
    annotate("text", 
             label = paste("KS p value:", sprintf("%.3f", round(ks.p, digits = 3))), 
             x = 9, y = Inf, hjust = 1, vjust = 3) +
    annotate("text",
             label = paste("Chi-sq p value:", sprintf("%.3f", round(chisq.p, digits = 3))),
             x = 9, y = Inf, hjust = 1, vjust = 4.5) +
    scale_x_continuous(breaks = c(min:9), labels = c(min:9)) + 
    scale_y_continuous(limits=c(0,0.4)) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    axes
  
  # return result
  result <- list(chisq = chisq, 
                 chisq.p = chisq.p,
                 ks = ks,
                 ks.p = ks.p)
  
  if(plot==TRUE) {
    print(result)
    
    return(p)
  } else {
    return(result)
  }
}

digit_test(turnout2011$turnout.voters, digit=1)
digit_test(turnout2011$turnout.voters, digit=2)
digit_test(turnout2011$turnout.voters, digit=3)

digit_test(turnout2011$invalid.votes, digit=1)
digit_test(turnout2011$invalid.votes, digit=2)
digit_test(turnout2011$invalid.votes, digit=3)

digit_test(result2016$vote, digit=1)
digit_test(result2016$vote, digit=2)
digit_test(result2016$vote, digit=3)


### Chi-square test as benchmarks

# note that only invalid votes should have first digit followed by Benford's laws
chisq.benftest(turnout2011$invalid.votes, digits=1)
chisq.benftest(turnout2011$invalid.votes, digits=2)
