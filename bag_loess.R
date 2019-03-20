library(ElemStatLearn)
library(lattice)
library(gridExtra)
source("eye_candy_theme.R")
set.seed(105)
data("ozone")
n <- nrow(ozone)
nlines <- 1000
spns <- c(0.2, 0.3, 0.4, 0.5)
ll <- array(NA, dim = c(nlines, n, 4))
for (j in 1:4) {
  for(i in 1:nlines){
    ss <- sample(1:dim(ozone)[1], replace=TRUE)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone,data=ozone0,span=spns[j])
    ll[i, , j] <- predict(loess0,newdata=data.frame(ozone=1:n))
  }  
}

bag_lines_plot <- function(j) {
  blines_plot <- xyplot(temperature~ozone, data = ozone
      , pch = 19, col = myPalDark[4]
      , bagLines = ll[, , j]
      , prepanel = function(x,y, bagLines) {
        list(xlim = c(1, max(x))
             , ylim = c(min(bagLines, na.rm = TRUE)
                        , max(bagLines, na.rm = TRUE)))
        }
      , panel = function(x,y, ...) {
        for (i in 1:nlines) {
           panel.lines(1:n,ll[i, , j], alpha = 0.025, col = myPal[1])
        }
        panel.xyplot(x,y, ...)
        panel.lines(1:n,apply(ll[, , j],2,mean),col=myPalDark[5],lwd=3)
      }
       , scales = MyLatticeScale
       , settings = MyLatticeTheme
      )
  return(blines_plot)
}

plots <- list()
for (j in 1:4) {
  plots[[j]] <- bag_lines_plot(j)  
}

grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=2, ncol=2)
