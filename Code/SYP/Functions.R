### DATA CLEANING HELPER FUNCTIONS

# helper function to identify row with non valid ID
numeric <- function(x) {
    which(!is.na(suppressWarnings(as.numeric(as.character(x)))))
}

# helper function to identify rows with numeric values -- a non-exported function from vietnamdata
find.numeric <- vietnamdata:::find.numeric

# helper function to identify NaN in a data frame
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

# helper function to "fill down the blanks"
fill.blanks <- function(x) {
    x[x==""] <- NA
    x <- na.locf(x)
}

# helper function to perform negative log transform
neglog <- function(x) sign(x) * (log(abs(x+.5)))

# helper function to clean province names using look up table
lookup.clean <- function(x, dictionary){
    for(i in 1:nrow(dictionary)){
        org <- dictionary$original[i] #table must contain original and fixed column
        fix <- dictionary$fixed[i]
        
        x[as.character(parse(text=paste0("'", x, "'"))) == org] <- fix
    }
    
    return(x)
}

# helper function to find best string match using string distance
bestmatch <- function(string, stringVector){
    stringdist::amatch(string, stringVector, method="lv", maxDist=1)
}


# helper function to identify candidate's last term in office

lastterm <- function(x) {
    terms <- unlist(stringr::str_split(x, ","))
    terms <- gsub(" -", " ", terms)
    terms <- gsub("-", " ", terms)
    
    last <- ifelse(str_trim(terms[length(terms)])=="", str_trim(terms[length(terms)-1]), str_trim(terms[length(terms)]))
    return(last)
}

# helper function to obtain year from date
year <- function(date) {
    unname(sapply(date, function(d) {
        if(grepl("/", d)==TRUE) {
            date <- as.Date(str_trim(d), "%d/%m/%Y")
        } else if(nchar(str_trim(d))==4){
            date <- as.Date(str_trim(d), "%Y")
        } else {
            date <- as.Date(as.numeric(d), origin="1900-01-01")
        }
        year <- as.numeric(substr(date,1,4))
        return(year)  
    }))
}

## Regression functions

### helper function

ritest <- function(data, year, treat, variable){
    y <- unlist(data[data$year==year, variable])
    d <- unlist(data[data$year==year, treat])
    
    omni.ate(y, d, genperms(d))
}

## 18/02/26 Update: Comment out the below since we are using the version from vietnamdata package now

# rireg<- function(data, outcome, treatment, covs, block = NULL, clus = NULL) {
#     
#     # run regression once to get point estimate
#     full <- lm(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")), data = data)
#     beta.actual <- coef(summary(full))[treatment, 1]
#     p.actual <- coef(summary(full))[treatment, 4]
#     
#     # purge the outcome variable of covariate-based noise
#     if(!is.null(covs)){
#         purge <- lm(as.formula(paste(outcome, paste(covs, collapse = "+"), sep = "~")), data = data)
#         y.tilde <- resid(purge)  
#     } else {
#         y.tilde <- data[[outcome]]
#     }
#     
#     
#     # regress the purged outcome on permutations of treatment variable
#     if(!is.null(block)){
#         block <- as.integer(as.factor(data[[block]]))
#     }
#     if(!is.null(clus)){
#         clus <- as.integer(as.factor(data[[clus]]))
#     }
#     perm <- genperms(data[[treatment]], blockvar = block, clustvar = clus)
#     
#     beta <- c()
#     p <- c()
#     
#     for(i in 1:ncol(perm)) {
#         treatment.perm <- perm[,i]
#         
#         fit <- lm(y.tilde ~ treatment.perm)
#         
#         if(!is.na(coef(fit)["treatment.perm"])){
#             beta[i] <- coef(summary(fit))["treatment.perm", 1]
#             p[i] <- coef(summary(fit))["treatment.perm", 4]
#         } else {
#             beta[i] <- NA
#             p[i] <- NA
#         }
#     }
#     
#     beta.greater <- mean(beta > beta.actual, na.rm=T)
#     beta.smaller <- mean(beta < beta.actual, na.rm=T)
#     p.smaller <- mean(p < p.actual, na.rm=T)
#     
#     return(list(beta = beta,
#                 beta.actual=beta.actual,
#                 beta.greater=beta.greater,
#                 beta.smaller=beta.smaller,
#                 p.actual=p.actual,
#                 p.smaller=p.smaller))
# }
# 
# riwfe <- function(data, outcome, treatment, covs, block = NULL, clus=NULL, unit.index, time.index, method, qoi="ate", estimator = NULL, unbiased.se=TRUE) {
#     
#     # run true regression once to get point estimate
#     full <- wfe(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")), 
#                 treat = treatment,
#                 unit.index = unit.index,
#                 time.index = time.index,
#                 method = method,
#                 qoi = qoi,
#                 unbiased.se = unbiased.se,
#                 estimator = estimator,
#                 data = data)
#     beta.actual <- coef(summary(full))[treatment, 1]
#     p.actual <- coef(summary(full))[treatment, 4]
#     
#     # regress the purged outcome on permutations of treatment variable
#     if(!is.null(block)){
#         block <- as.integer(as.factor(data[[block]]))
#     }
#     if(!is.null(clus)){
#         clus <- as.integer(as.factor(data[[clus]]))
#     }
#     perm <- genperms(data[[treatment]], blockvar = block, clustvar = clus, maxiter = 1000)
#     
#     beta <- c()
#     p <- c()
#     
#     for(i in 1:ncol(perm)) {
#         treatment.perm <- perm[,i]
#         data[[treatment]] <- treatment.perm
#         
#         fit <- wfe(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")), 
#                    treat = treatment,
#                    unit.index = unit.index,
#                    time.index = time.index,
#                    method = method,
#                    qoi = qoi,
#                    unbiased.se = unbiased.se,
#                    estimator = estimator,
#                    data = data)
#         
#         if(!is.na(coef(fit)[treatment])){
#             beta[i] <- coef(summary(fit))[treatment, 1]
#             p[i] <- coef(summary(fit))[treatment, 4]
#         } else {
#             beta[i] <- NA
#             p[i] <- NA
#         }
#     }
#     
#     beta.greater <- mean(beta > beta.actual, na.rm=T)
#     beta.smaller <- mean(beta < beta.actual, na.rm=T)
#     p.smaller <- mean(p < p.actual, na.rm=T)
#     
#     return(list(beta = beta,
#                 beta.actual=beta.actual,
#                 beta.greater=beta.greater,
#                 beta.smaller=beta.smaller,
#                 p.actual=p.actual,
#                 p.smaller=p.smaller))
# }
# 
# rireg.plot <- function(rireg.obj, title = NULL, xlab = NULL, ylab=NULL, scale=F, xmin, xmax, axe.y=F){
#     beta.null <- data.frame(beta = rireg.obj$beta)
#     beta.actual <- rireg.obj$beta.actual
#     
#     if(scale){
#         beta.actual <- (beta.actual - mean(beta.null$beta))/sd(beta.null$beta)
#         beta.null$beta <- (beta.null$beta - mean(beta.null$beta))/sd(beta.null$beta)
#     }
#     
#     if(axe.y) {
#         axes <- theme(axis.text.x=element_blank(), 
#                       axis.ticks=element_blank(),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank(),legend.position="none")
#     } else {
#         axes <- theme(axis.text=element_blank(), 
#                       axis.ticks=element_blank(),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank(),legend.position="none")
#     }
#     
#     ggplot(beta.null, aes(x = beta)) + 
#         stat_bin(aes(y=..count../sum(..count..)), fill = "grey") +
#         geom_vline(aes(xintercept = beta.actual), colour="red") +
#         geom_vline(aes(xintercept = 0), linetype="dashed", colour="black") +
#         ggtitle(title) +
#         xlab(xlab) +
#         ylab(ylab) +
#         theme_bw() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         axes +
#         coord_flip(xlim = c(xmin,xmax))
# }
# 
# ### Synthetic control functions
# ### helper functions
# 
# ## calculate ATT using synthetic control, with options for parallelization using snowfall
# Synth.att <- function(data, outcome, treatment, covs,
#                       treatment.year, pretreatment.year, posttreatment.year=NULL,
#                       unit.variable, unit.names.variable, time.variable, 
#                       include.past.Y = TRUE, snowfall=FALSE) {
#     require(Synth)
#     
#     # check whether past outcomes is used as predictors
#     if(include.past.Y) {
#         predictors <- c(outcome, treatment, covs)
#     } else {
#         predictors <- c(treatment, covs)
#     }
#     
#     # get treatment and control identifiers
#     treatment.identifier <- data[[unit.variable]][which(data[[treatment]] == 1 & data[[time.variable]] == treatment.year)]
#     #names(treatment.identifier) <- data[[unit.names.variable]][treatment.identifier]
#     control.identifier <- data[[unit.variable]][which(data[[treatment]] == 0 & data[[time.variable]] == treatment.year)]
#     #names(control.identifier) <- data[[unit.names.variable]][control.identifier]
#     
#     # prepare parallel computing
#     if(snowfall){
#         require(snowfall)
#         sfInit(parallel = TRUE, cpus = parallel::detectCores())
#         sfExport(list=c("data", "outcome", "treatment", "covs", 
#                         "treatment.year", "pretreatment.year", "posttreatment.year",
#                         "unit.variable", "unit.names.variable", "time.variable", 
#                         "include.past.Y", "treatment.identifier", "control.identifier"))
#         sfLibrary(Synth)
#         
#         apply.fun <- sfSapply
#     }
#     else{
#         apply.fun <- sapply
#     }
#     
#     tau <- apply.fun(1:length(treatment.identifier), function (i) {
#         # run Synth once to get point estimate
#         dataprep.obj <- dataprep(foo = data,
#                                  predictors = predictors, # can add history of treatment and outcomes
#                                  predictors.op = "mean",
#                                  dependent = outcome,
#                                  unit.variable = unit.variable,
#                                  unit.names.variable = unit.names.variable,
#                                  time.variable = time.variable,
#                                  treatment.identifier = treatment.identifier[i],
#                                  controls.identifier = control.identifier,
#                                  time.predictors.prior = pretreatment.year,
#                                  time.optimize.ssr = pretreatment.year,
#                                  time.plot=c(pretreatment.year,treatment.year,posttreatment.year))
#         
#         synth.out <- synth(dataprep.obj)
#         # extract estimates
#         y1 <- dataprep.obj$Y1plot
#         y0 <- dataprep.obj$Y0plot %*% synth.out$solution.w
#         
#         # effect is more like a dif-in-dif, since sometimes the synthetic control does not track too well
#         tau <- (mean(y1[as.character(c(treatment.year, posttreatment.year)),]) - 
#                     mean(y1[as.character(c(pretreatment.year)),])) - 
#             (mean(y0[as.character(c(treatment.year, posttreatment.year)),]) - 
#                  mean(y0[as.character(c(pretreatment.year)),])) 
#         
#         return(tau)
#     })
#     
#     if(snowfall){
#         # result from snowfall is in list form
#         tau <- unlist(tau, recursive = F)
#         sfStop() 
#     }
#     return(list(tau.i = tau,
#                 tau.att = mean(tau)))  
# }
# 
# ## RI on Synthetic control ATT, with options for parallelization using snowfall
# riSynth <- function(data, outcome, treatment, covs,
#                     treatment.year, pretreatment.year, posttreatment.year=NULL,
#                     unit.variable, unit.names.variable, time.variable, 
#                     include.past.Y = TRUE, snowfall=FALSE) {
#     require(Synth)
#     require(ri)
#     
#     # loop over all treated units
#     tau.actual <- Synth.att(data, outcome, treatment, covs, 
#                             treatment.year, pretreatment.year, posttreatment.year,
#                             unit.variable, unit.names.variable, time.variable, 
#                             include.past.Y, snowfall)
#     
#     # generate permutations of treatment variable
#     perm <- genperms(data[[treatment]][which(data[[time.variable]] == treatment.year)], maxiter = 1000)
#     
#     if(snowfall){
#         require(snowfall)
#         sfInit(parallel = TRUE, cpus = parallel::detectCores())
#         sfExport(list=c("data", "outcome", "treatment", "covs", 
#                         "treatment.year", "pretreatment.year", "posttreatment.year",
#                         "unit.variable", "unit.names.variable", "time.variable", 
#                         "include.past.Y", "Synth.att", "perm"))
#         sfLibrary(Synth)
#         
#         apply.fun <- sfLapply
#     } else {
#         apply.fun <- lapply
#     }
#     tau <- apply.fun(1:ncol(perm), function (i) {
#         treatment.perm <- perm[,i]
#         data[[treatment]][which(data[[time.variable]] == treatment.year)] <- treatment.perm
#         
#         tau <- tryCatch(Synth.att(data, outcome, treatment, covs,
#                                   treatment.year, pretreatment.year, posttreatment.year,
#                                   unit.variable, unit.names.variable, time.variable, 
#                                   include.past.Y, snowfall = snowfall),
#                         error = function(e) NULL)
#         return(tau)
#     })
#     
#     if(snowfall){
#         # result from snowfall is in list form
#         sfStop() 
#     }
#     
#     tau.att <- unlist(lapply(tau, function(x) x["tau.att"]), use.names = F)
#     tau.i <- do.call(rbind,unlist(lapply(tau, function(x) x["tau.i"]), recursive=F, use.names = F))
#     
#     tau.att.greater <- mean(tau.att > tau.actual$tau.att, na.rm=T)
#     tau.att.smaller <- mean(tau.att < tau.actual$tau.att, na.rm=T)
#     
#     return(list(tau.att.actual=tau.actual$tau.att,
#                 tau.i.actual=tau.actual$tau.i,
#                 tau.att=tau.att,
#                 tau.i=tau.i,
#                 tau.att.greater=tau.att.greater,
#                 tau.att.smaller=tau.att.smaller))
# }
# 
# riSynth.plot <- function(riSynth.obj, title = NULL, xlab = NULL, ylab=NULL, scale=F, xmin, xmax, att=T, axe.y=F){
#     tau.att.null <- data.frame(tau.att = riSynth.obj$tau.att)
#     tau.att.actual <- riSynth.obj$tau.att.actual
#     
#     if(scale){
#         tau.att.actual <- (tau.att.actual - mean(tau.att.null$tau.att))/sd(tau.att.null$tau.att)
#         tau.att.null$tau.att <- (tau.att.null$tau.att - mean(tau.att.null$tau.att))/sd(tau.att.null$tau.att)
#     }
#     
#     if(axe.y) {
#         axes <- theme(axis.text.x=element_blank(), 
#                       axis.ticks=element_blank(),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank(),legend.position="none")
#     } else {
#         axes <- theme(axis.text=element_blank(), 
#                       axis.ticks=element_blank(),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank(),legend.position="none")
#     }
#     
#     ggplot(tau.att.null, aes(x = tau.att)) + 
#         stat_bin(aes(y=..count../sum(..count..)), fill = "grey") +
#         geom_vline(aes(xintercept = tau.att.actual), colour="red") +
#         geom_vline(aes(xintercept = 0), linetype="dashed", colour="black") +
#         ggtitle(title) +
#         xlab(xlab) +
#         ylab(ylab) +
#         theme_bw() +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         axes +
#         coord_flip(xlim = c(xmin,xmax))
# }

