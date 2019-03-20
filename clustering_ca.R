library(dplyr)
library(ca)
source("eye_candy_theme.R")
my_data <- read.csv(gzfile("nursery.csv.gz"))

# nursery data set
my_data <- my_data %>% filter(decision != "very_recom"
                              , decision != "not_recom") %>%
 mutate(health = factor(health), decision=factor(decision)
        , form = factor(ifelse(as.character(form)=="completed", "complete", as.character(form))))


# recid data set
my_data <- read.csv(gzfile("rcdv.csv.gz"))
my_data <- my_data %>% transmute(year = factor(year)
                                 , race = factor(ifelse(white==1, "caucasian", "africanAmerican"))
                                 , alchy = factor(ifelse(alchy==1, "alch_dep", "non_alch_dep"))
                                 , junky = factor(ifelse(junky==1, "drug_dep", "non_drug_dep"))
                                 , sentence = factor(ifelse(super==1, "supervised", "unsupervised"))
                                 , married = factor(ifelse(married==1, "married", "unmarried"))
                                 , felon = factor(ifelse(felon==1, "felony", "midemeanor"))
                                 , workrel = factor(ifelse(workrel==1, "participated", "not_particip"))
                                 , propty = factor(ifelse(propty==1, "property", "non_property"))
                                 , person = factor(ifelse(person==1, "person", "non_preson"))
                                 , gender = factor(ifelse(male==1, "male", "female"))
                                 , age = factor(ifelse(age/12 > 30, "mature", "young"))
                                 , follow_up = factor(ifelse(follow>5, "long_term", "short_term"))
                                 , time_served = factor(ifelse(log(tservd) > 0, ">year", "<year"))
                                 , record = factor(ifelse(missingness==1, "missing", "complete"))
                                 , recid = factor(ifelse(recid=="Y", "reoffend", "not_reoffend")))

nlev <- 2 # each has two levels
cols <- myPalDark # 5 dimension

analyse <- c("race", "age", "time_served", "married", "recid")
analyse <- sort(analyse)

my_data.mca <- mjca(my_data[, analyse]) # ca library

# "blank plot"
res <- plot(my_data.mca, labels=0, pch='.', cex.lab=1.2)

# combine Dims, factor names and levels
coords <- data.frame(res$cols, my_data.mca$factors)
coords <- coords[ order(coords[,"factor"], coords[,"level"]), ]

# place the points with separate plot chars and colours
points(coords[,1:2], pch=rep(19:15, each=nlev), col=rep(cols, each= nlev), cex=1.2)

# place the text
text(coords[,1:2], labels=coords$level, col=rep(cols, each= nlev)
     , pos=1
     , cex=1.1, xpd=TRUE)

# add segement from the origin to channels
segments(0, 0, coords[,"Dim1"], coords[, "Dim2"], col =rep(cols, each= nlev), lwd = 1, lty = 1)

# add a legend
legend("bottomright", legend=analyse,
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=19:15,
       bg="gray95")


## ----tv_2wayca-----------------------------------------------------------
# Flatten to 2-D by stacking Time onto Day
# Note: The data shaping choice here controls the specifics of the analysis.
my_datas <- as.matrix(structable(Network~Time+Day, my_data))
my_datas

# Create the Correspondence Analysis objects
my_datas.ca <- ca(my_datas)

# Generate the plot
res.ca <- plot(my_datas.ca)
# add some segments from the origin to make things clearer
segments(0, 0, res.ca$cols[,1], res.ca$cols[,2], col = "red", lwd = 1)
segments(0, 0, res.ca$rows[,1], res.ca$rows[,2], col = "blue", lwd = 0.5, lty = 3)

## ----TV_monday-----------------------------------------------------------
t(my_data[1,,]) # Monday
my_data[,2,1] # Every day, 9pm, ABC

## ----Titanic_mca---------------------------------------------------------
# one more mca from a 4-D dataset
# this is simpler than it looks
# all the magic happens here
titanic.mca <- mjca(Titanic)

# saving the plot object supplies the coordinate positions
res <- plot(titanic.mca, labels=0, pch='.', cex.lab=1.2)

# extract factor names and levels
coords <- data.frame(res$cols, titanic.mca$factors)

# everything else is handling base R plotting stuff
cols <- c("blue", "red", "brown", "black")
nlev <- c(4,2,2,2)
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
pos <- c(3,1,1,3)
text(coords[,1:2], labels=coords$level, col=rep(cols, nlev), pos=rep(pos,nlev), cex=1.1, xpd=TRUE)
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Class", lty=1, lwd=2, col="blue")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Sex",  lty=1, lwd=2, col="red")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Age",  lty=1, lwd=2, col="brown")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Survived",  lty=1, lwd=2, col="black")

legend("topleft", legend=c("Class", "Sex", "Age", "Survived"),
       title="Factor", title.col="black",
       col=cols, text.col=cols, pch=16:19,
       bg="gray95", cex=1.2)




summary(my_data)

## ----nurs_randfor--------------------------------------------------------
# train test split
set.seed(54321)
idx <- sample(c(TRUE, FALSE)
              , size = nrow(my_data)
              , prob = c(0.7, 0.3)
              , replace = TRUE)

train <- my_data[idx, ]
test <- my_data[!idx, ]

# train rf accepting all the defaults
rf <- randomForest(decision~., data=train)

# get the predictions on new data
preds <- predict(rf, newdata = test)

# confusion matrix
confmat <- with(test, table(preds, decision))
confmat
sum(diag(confmat)/sum(confmat))

## ----strucplot_confmat---------------------------------------------------
# strucplot tile - I like this one best
tile(confmat, labeling = labeling_values)

## ----nurs_varimp---------------------------------------------------------
varImpPlot(rf)

## ----nurs_firstvar-------------------------------------------------------
# unfortunately, there is no cotabplot option for tile
# this has to be done manually to make any sense

# train test split
set.seed(54321)
idx <- sample(c(TRUE, FALSE)
              , size = nrow(my_data)
              , prob = c(0.7, 0.3)
              , replace = TRUE)

train <- my_data[idx, ]
test <- my_data[!idx, ]

