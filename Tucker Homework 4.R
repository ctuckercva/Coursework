#Q1
xvallm <- function (data, ycol, predvars, p, meanabs = TRUE) {

  
  smp_size <- floor(p * nrow(data))
  
  ## set the seed to make the partition reproductible
  #set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  valid <- data[-train_ind, ]  
  
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid[,ycol]
  if (meanabs) return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
  
}

library(regtools)
library(freqparcoord)
data(mlb)

for (ii in 1:10){
  print(xvallm(mlb , 5 , c ( 4 , 6 ) , 2/3))
}

#Q2

data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1, 12, 9, 13, 14, 15, 8)]
pe <- as.matrix(pe)
oldmod <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng)
newmod <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + fem*age + fem*age2, data = prgeng)
newdata <- data.frame(age = 32, age2 = 1024, fem = 1, ms = 1, phd = 0, wkswrkd = 0)

#effect of changes on 32 y/o female with MS
predict(newmod, newdata) - predict(oldmod, newdata)

#q3

bodyfat <- read.csv("../Anaconda3/envs/classwork/data/MLDatasets/bodyfat.csv")
head(bodyfat)
lm(density ~ age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist, data = bodyfat)

#indirect methods are feasible only inasmuch as the predicted variable can be accurately assessed 
#via a different method (in this case by hydrostatic measurement, for example)


#Q4.a Average population height is the average height of men, multiplied by the proportion of the population that
#is male, plus the same for females

#Q4.b Same calculation, replacing height with proportion > 70.


#Chapter 2
#Q1:  Duly considered!

#Q2

summary(oldmod)
summary(newmod)
confint(oldmod)

#Q3
thirdmod <- lm (wageinc ~ age + age2 + wkswrkd + ms + phd + fem + fem*ms + fem*phd, data = prgeng)
summary(thirdmod)
confint(thirdmod)


#Q4
shar <- read.csv("../Anaconda3/envs/classwork/data/MLDatasets/day.csv")
year <- shar$yr
head(shar[year == 2,])
shar$temp2 <- shar$temp^2
shar$clearday <- as.integer(shar$weathersit == 1)
bikemod0 <- lm(registered ~ temp + temp2 + workingday + clearday, data = shar[year == 0,])
summary(bikemod0)

#dataset only has 2 years, not the 3 claimed in the book:
aggregate(registered ~ yr, data = shar, FUN = function(x){NROW(x)})

bikemod1 <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = shar)
summary(bikemod1)
confint(bikemod1)


