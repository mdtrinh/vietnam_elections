### Functions to plot RDD results
# adapted from vietnamdata::plot.riFit()
ri_plot <- function(x, title = NULL, xlab = NULL, ylab=NULL, scale=F, xmin, xmax, axe.y=F, ...){
  beta.null <- data.frame(beta = x$beta[-1])
  beta.actual <- x$beta[1]
  
  if(missing(xmin) | missing(xmax)) {
    xmin <- min(beta.null$beta) - sd(beta.null$beta)
    xmax <- max(beta.null$beta) + sd(beta.null$beta)
  }
  if(scale){
    beta.actual <- (beta.actual - mean(beta.null$beta))/sd(beta.null$beta)
    beta.null$beta <- (beta.null$beta - mean(beta.null$beta))/sd(beta.null$beta)
  }
  
  if(axe.y) {
    axes <- ggplot2::theme(axis.text.x=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),legend.position="none")
  } else {
    axes <- ggplot2::theme(axis.text=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),legend.position="none")
  }
  
  ggplot2::ggplot(beta.null, aes(x = beta)) +
    ggplot2::stat_bin(aes(y=..count../sum(..count..)), fill = "grey", ...) +
    ggplot2::geom_vline(aes(xintercept = beta.actual), colour="red") +
    ggplot2::geom_vline(aes(xintercept = 0), linetype="dashed", colour="black") +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5,
                                             size = 10, 
                                             face = "bold")) +
    axes +
    ggplot2::coord_flip(xlim = c(xmin,xmax))
}

# function to create a textGrob listing p-values
ri_annotate <- function(x, show_beta = TRUE, show_wilcox = TRUE) {
  beta.null <- na.omit(x$beta[-1])
  beta.actual <- x$beta[1]
  
  p_beta <- 2*min(mean(beta.actual > beta.null),
                  mean(beta.actual < beta.null))
  
  wilcox.null <- x$wilcox[-1]
  wilcox.actual <- x$wilcox[1]
  
  p_wilcox <- 2*min(mean(wilcox.actual > wilcox.null),
                    mean(wilcox.actual < wilcox.null))
  
  if(show_beta == TRUE & show_wilcox == TRUE) {
    textGrob(paste("Effect p-value: ", round(p_beta, digits = 2), "\n",
                   "Wilcoxson p-value: ", round(p_wilcox, digits = 2)), 
             gp=gpar(fontsize=10))
  } else if(show_beta == TRUE) {
    textGrob(paste("Effect p-value: ", round(p_beta, digits = 2), "\n"),
             gp=gpar(fontsize=10))
  } else if(show_wilcox == TRUE) {
    textGrob(paste("Wilcoxson p-value: ", round(p_wilcox, digits = 2), "\n"),
             gp=gpar(fontsize=10))
  } else {
    stop("Either show_beta or show_wilcox must be TRUE!")
  }
}

# function to draw a simple y axe
y_axe <- function(min, max, flip=TRUE) {
  # flip = TRUE for sideway graphs
  if(flip){
    coord <- coord_flip(xlim = c(xmin = min,xmax = max))
    margin <- unit(c(5.5,0,5.5,5.5), "points")
  } else {
    coord <- coord_cartesian(ylim = c(ymin = min,ymax = max))
    margin <- unit(c(0,5.5,5.5,5.5), "points")
  }
  
  ggplot(data.frame(x=1,y=1), aes(x,y)) +
    geom_blank() +
    ggtitle("") +
    theme_bw() +
    theme(axis.line.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.line.y=element_blank(),
          #axis.line.y=element_line(color = "black"),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = margin,
          plot.title = element_text(hjust = 0.5,
                                    size = 10, 
                                    face = "bold")) +
    coord
}


### Functions to plot gsynth results

# wrapper function to plot a gsynth result using my chosen aesthetics
gsynth_plot <- function(synth_obj, title = NULL, xlab = NULL, ylab=NULL, xmin, xmax, ymin, ymax, id=NULL,...) {
  
  if(missing(xmin) | missing(xmax)) {
    xlim <- NULL
  } else {
    xlim <- c(xmin, xmax)
  }
  
  if(missing(ymin) | missing(ymax)) {
    ylim <- NULL
  } else {
    ylim <- c(ymin, ymax)
  }
  
  plot <- plot(synth_obj, xlab = xlab, ylab = ylab, main = title, id = id,...) +
    #xlab(xlab) +
    #ylab(ylab) +
    #ggtitle(title) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(xmin, xmax, 1)) +
    # override default xlim ylim behavior of plot.gsynth
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_bw() +
    theme(plot.title = element_blank(),
          #axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position="none")
  print(plot)
}

x_axe <- function(min, max, flip=FALSE) {
  # flip = TRUE for sideway graphs
  if(flip){
    coord <- coord_flip(ylim = c(ymin = min,ymax = max))
  } else {
    coord <- coord_cartesian(xlim = c(xmin = min,xmax = max))
  }
  
  ggplot(data.frame(x=1,y=-10), aes(x,y)) +
    geom_blank() +
    ggtitle(" ") +
    theme_bw() +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_text(colour = "white"),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.line.x=element_line(colour = "black"),
          axis.title.x=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    coord
}


### Functions to plot Fisher randomization results

# function to export just the estimate and p-value of fisher.test
fisher_display <- function(x) {
  odds <- round(x$estimate, digits = 2)
  
  p.value <- x$p.value
  
  stars <- symnum(x$p.value, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", " "))
  
  return(list(odd_ratio = odds, p.value = p.value, stars = stars))
}
