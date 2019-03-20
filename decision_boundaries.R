library(lattice)
library(latticeExtra)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
source("eye_candy_theme.R")
set.seed(10)
x1.1 <- rbeta(100, 0.2, 0.2) + rnorm(100, 0, 0.05)
# x2.1 <- ((x1.1/3)^8  + rnorm(100, 0.5, sd = 0.25))

x2.1 <-  cos(2 * pi * x1.1) * 2 + rnorm(100, 3, 0.5)

x1.2 <- (-x1.1 + 1.167) * 6

x1.1 <- x1.1 * 6 + 3.25

x2.2 <- -x2.1 + 9.5

x.1 <- cbind(x1.1, x2.1)
x.2 <- cbind(x1.2, x2.2)
xban <- rbind(x.1, x.2)

xban <- data.frame(xban)
names(xban) <- c("x1", "x2")
xban$y <- c(rep(1,100),rep(-1,100))
xban$y2 <- ifelse(xban$y == -1, 0, 1)
xban <- rbind(xban, data.frame(x1=c(6, 2, 8.2)
                               , x2=c(4, 4, 5)
                               , y=c(1, 1, -1)
                               , y2=c(1, 1, 0)))

pchx <- 1.5

xyplot(x2~x1, xban
       , col = xban$y + 3
       , pch = xban$y + 3
       , cex = pchx)

svm.fit <- svm(y~x1+x2, data = xban, type = "C"
               , kernel = "rad")
glm.fit <- glm(y2~x1+x2, data = xban, family = binomial())

rf.fit <- randomForest(factor(y2)~x1+x2, data = xban, ntree=1000)

tree.fit <- rpart(factor(y2)~x1+x2, data = xban
                  , method = "class"
                  , control = rpart.control(minsplit = 1
                                            , maxcompete = 1))

db_plot <- function(predmat, titl) {
  levelplot(y~x1*x2
            , predmat
            , main = titl
            , xlim = c(-0.5, 10.5)
            , ylim = c(-0.5, 10.5)
            , colorkey = NULL
            , par.settings = MyLatticeTheme
            , alpha.regions = 0.5
            , scales = MyLatticeScale
)}

decision_boundary_plot <- function(predmat
                                   , training_set
                                   , titl) {
  
  plot_out <- db_plot(predmat, titl) +
    as.layer(xyplot(x2~x1, training_set
                    , col = training_set$y + 3
                    , pch = training_set$y + 3
                    , cex = pchx)) +
    as.layer(contourplot(y~x1*x2
                         , predmat
                         , labels = FALSE
                         , cuts = 1))
  
  return(plot_out)
}

predpts <- 400
predmat <- expand.grid(x1 = seq(0, 10, length.out = predpts), x2 = seq(0, 10, length.out = predpts))

predmat$y <- factor(ifelse(predict(glm.fit, newdata = predmat, type = "response") > 0.5, 1, -1))
plot1 <- decision_boundary_plot(predmat, xban
                                , "Logistic")

predmat$y <- factor(ifelse(as.numeric(as.vector(predict(tree.fit, newdata = predmat)[,2])) > 0.5, 1, -1))
plot2 <- decision_boundary_plot(predmat, xban
                                , "Decision Tree")

predmat$y <- predict(svm.fit, newdata = predmat)
plot3 <- decision_boundary_plot(predmat, xban
                                , "Support Vector Machine")

predmat$y <- factor(ifelse(as.numeric(as.vector(predict(rf.fit, newdata = predmat))) > 0.5, 1, -1))
plot4 <- decision_boundary_plot(predmat, xban
                                , "Random Forest")

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)


rpart.plot(tree.fit)

instances <- 1:nrow(xban)
bag_trees <- function(n) {
  
  fits <- list()
  for (i in 1:n) {
    boo_xban <- xban[sample(instances, replace = TRUE), ]
    fits[[i]] <- rpart(factor(y2)~x1+x2, data = boo_xban
                       , method = "class"
                       , model = TRUE
                       , control = rpart.control(minsplit = 1
                                                 , maxcompete = 1))
    
  }
  return(fits)
}
b_trees <- bag_trees(4)                                                

par(mfrow=c(2, 2), bty = "o")
rpart.plot(b_trees[[1]])
rpart.plot(b_trees[[2]])
rpart.plot(b_trees[[3]])
rpart.plot(b_trees[[4]])
par(mfrow=c(1, 1))


predpts <- 351
predmat <- expand.grid(x1 = seq(0, 10, length.out = predpts), x2 = seq(0, 10, length.out = predpts))

predmat$y <- factor(ifelse(as.numeric(as.vector(predict(b_trees[[1]], newdata = predmat)[,2])) > 0.5, 1, -1))
plot1t <- decision_boundary_plot(predmat, xban
                                 , NULL)
predmat$y <- factor(ifelse(as.numeric(as.vector(predict(b_trees[[2]], newdata = predmat)[,2])) > 0.5, 1, -1))
plot2t <- decision_boundary_plot(predmat, xban
                                 , NULL)
predmat$y <- factor(ifelse(as.numeric(as.vector(predict(b_trees[[3]], newdata = predmat)[,2])) > 0.5, 1, -1))
plot3t <- decision_boundary_plot(predmat, xban
                                 , NULL)
predmat$y <- factor(ifelse(as.numeric(as.vector(predict(b_trees[[4]], newdata = predmat)[,2])) > 0.5, 1, -1))
plot4t <- decision_boundary_plot(predmat, xban
                                 , NULL)

grid.arrange(plot1t, plot2t, plot3t, plot4t, ncol = 2, nrow = 2)

txtx <- 2
predmat$y <- factor(ifelse(as.numeric(as.vector(predict(rf.fit, newdata = predmat))) > 0.5, 1, -1))
plota <- decision_boundary_plot(predmat, xban
                                , NULL)
plot1a <- plota +
  as.layer(xyplot(x2~x1
                  , predmat
                  , panel = function(x, y) {
                    panel.rect(0,0,1.7,10
                               , col=2
                               , lty=2
                               , alpha = 0.4)
                    panel.points(x = 1, 9
                                 , pch = 19
                                 , col = "black"
                                 , cex = 2)
                    panel.text(x = 2, y = 8
                               , cex = txtx
                               , pos = 4
                               , expression(x1 < 1.78 %->% "red"))
                  }))

plot2a <- plota +
  as.layer(xyplot(x2~x1
                  , predmat
                  , panel = function(x, y) {
                    panel.rect(1.7,0,10,2.9
                               , col = 4
                               , lty = 2
                               , alpha = 0.4)
                    panel.points(x = 9, 2.5
                                 , pch = 19
                                 , col = "black"
                                 , cex = 2)
                    panel.arrows(x0 = c(2.1, 2.1)
                                 , y0 = c(0.5, 5.5)
                                 , x1 = c(2.1, 2.1)
                                 , y1 = c(2.9, 3.2)
                                 , lwd = 2
                                 , length = 0.15)
                    panel.text(x = 2, y = 8
                               , cex = txtx
                               , pos = 4
                               , expression(paste(x1 > 1.78, " & ", x2 < 2.9 %->% "blue")))
                    panel.text(x = 2.5, y = 1
                               , cex = txtx
                               , pos = 4
                               , expression(paste("cost of encroaching\ninto red space @", x2 < 3.3, " ?")))
                  }))

plot3a <- plota +
  as.layer(xyplot(x2~x1
                  , predmat
                  , panel = function(x, y) {
                    panel.rect(5.15,5,8.15,5.2
                               , col = 2
                               , lty = 2
                               , alpha = 0.4)
                    panel.points(x = 7, 5.1
                                 , pch = 19
                                 , col = "black"
                                 , cex = 2)
                    panel.text(x = 2, y = 2
                               , cex = txtx
                               , pos = 4
                               , "best precision")
                    
                  }))

plot4a <- plota +
  as.layer(xyplot(x2~x1
                  , predmat
                  , panel = function(x, y) {
                    panel.rect(5.15,5,8.15,10
                               , col = 2
                               , lty = 2
                               , alpha = 0.4)
                    panel.points(x = 7, 5.1
                                 , pch = 19
                                 , col = "black"
                                 , cex = 2)
                    panel.text(x = 2, y = 2
                               , cex = txtx
                               , pos = 4
                               , "best coverage")
                    
                  }))

grid.arrange(plot1a, plot2a, plot3a, plot4a
             , ncol = 2, nrow = 2)

levelplot(y~x1*x2
          , predmat
          , labels = FALSE
          , par.settings = MyLatticeTheme)