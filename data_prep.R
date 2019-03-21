library(dplyr)

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

# write.csv(my_data, "recid_factors.csv")


computer.data <- read.csv("machine.data"
                          , header = FALSE
                          , stringsAsFactors = FALSE)

names(computer.data) <- c("VendorName"
                          , "ModelName"
                          , "MYCT"
                          , "MMIN"
                          , "MMAX"
                          , "CACH"
                          , "CHMIN"
                          , "CHMAX"
                          , "PRP"
                          , "ERP")
summary(computer.data)
computer.data.plotting <- computer.data[, -c(1, 2)]
computer.data.plotting.log.y <- cbind(computer.data.plotting[, 1:6]
                                      , log(computer.data.plotting[, 7:8]))
computer.data.plotting.log.xy <- cbind(log(computer.data.plotting[, c("MYCT"
                                                                      , "MMIN"
                                                                      , "MMAX")])
                                       , log(1 + computer.data.plotting[, c("CACH"
                                                                            , "CHMIN"
                                                                            , "CHMAX")])
                                       , log(computer.data.plotting[, 7:8])
)

splom(computer.data.plotting
      , pscales = 0)
splom(computer.data.plotting.log.y
      , pscales = 0)
splom(computer.data.plotting.log.xy
      , pscales = 0)


computer.data.log <- cbind(computer.data[, c(1, 2)]
                           , computer.data.plotting.log.xy)