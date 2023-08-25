library(partykit)
library(datasets)
library(rpart)
library(dplyr)
library(randomForest)

#Data manipulation/cleaning
aq <- na.omit(airquality)
aq$Month = as.factor(aq$Month)
aq$Day = as.factor(aq$Day)

set.seed(298192001)
#Create a train-test split
samp <- sample(x=1:nrow(aq), size = .7*nrow(aq), replace = FALSE)
aqtrain <- aq[samp,-2]
aqtest <- aq[-samp,-2]
summary(aqtrain)
summary(aqtest)

#3D visualization
c = ifelse(aq$Temp >= 80, "red", "black" )
setTred = aq %>% filter(Temp >=80)
setTblack = aq%>% filter(Temp<80)
lmred = lm(data = setTred, Ozone ~ Wind)
lmblack = lm(data=setTblack, Ozone ~ Wind)
xyzred = xyz.coords(x=lmred$model$Wind, y=lmred$fitted.values, z = setTred$Ozone)
library(rgl)
plot3d(x=aq$Wind, y=aq$Temp, z=aq$Ozone, xlab="Wind", ylab="Temp", zlab="Ozone", col = c)
#planes3d(a=coef(lmred)["Wind"], c= coef(lmred)["Intercept"])

#2D visualization
par(mfrow=c(1,3))
plot(x=aq$Temp, y=aq$Ozone, xlab = "Temp", ylab="Ozone")
plot(x=aq$Wind, y=aq$Ozone, xlab = "Wind", ylab="Ozone")
plot(x=aq$Month, y=aq$Ozone, xlab = "Month", ylab="Ozone")

#Conditional inference tree (not a MOB tree, similar to CART tree but 
# use inference to control tree size)
aqtree = ctree(Ozone ~ ., data = aqtrain)#,   
               #control = ctree_control(maxsurrogate = 3))
aqtree
plot(aqtree, type = "simple")

#CART tree
aqrpart = rpart(Ozone ~Temp+Wind+Month, data=aqtrain, cp=0)
cpt = aqrpart$cptable
#pruning
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)
aqrpartmin = prune(aqrpart, cp=cp.min)
se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- cpt[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)
aqrpartse = prune(aqrpart, cp=cp.1se)

aqrpartse
aqrpartmin
#same tree
aqrpart

#Random forest
aqrf = randomForest(data=aq, Ozone~Wind+Temp+Month)
plot(aqrf)

#MOB tree
aqmob = glmtree(Ozone~ Wind | Temp+Month, data=aqtrain)
aqmob
plot(aqmob)
seekViewport("node_terminal4plot")
grid.text("Wind",
          x = unit(0.5, "npc"), y = unit(-3, "lines"))

#Tree performance metrics - comparing methods
MSPE.aqtree = mean((predict(aqtree, newdata = aqtest) - aqtest$Ozone)^2)
MSE.aqtree = mean((predict(aqtree, newdata = aqtrain) - aqtrain$Ozone)^2)
MSPE.aqmob = mean((predict(aqmob, newdata = aqtest) - aqtest$Ozone)^2) 
MSE.aqmob = mean((predict(aqmob, newdata = aqtrain) - aqtrain$Ozone)^2)

MSPE.aqrpartmin = mean((predict(aqrpartmin, newdata = aqtest) - aqtest$Ozone)^2)
MSPE.aqrpartse = mean((predict(aqrpartse, newdata=aqtest) - aqtest$Ozone)^2)

#Comparing to a linear model
aqlm = lm(Ozone ~ Wind+Temp+Month, data = aqtrain)
summary(aqlm)
MSPE.aqlm = mean((predict(aqlm, newdata = aqtest) - aqtest$Ozone)^2) 
MSE.aqlm = mean((predict(aqlm, newdata=aqtrain) - aqtrain$Ozone)^2)

#bootstrap validation error
b=1
B=10

MSPE.boottree = vector(length=B)
MSE.boottree = vector(length=B)
MSPE.bootmob = vector(length=B)
MSE.bootmob = vector(length=B)
MSPE.bootlm = vector(length=B)
MSE.bootlm = vector(length = B)
MSPE.bootrpartmin = vector(length=B)
MSE.bootrpartmin = vector(length=B)
MSPE.bootrpartse = vector(length=B)
MSE.bootrpartse = vector(length=B)
MSPE.bootrf = vector(length=B)
MSE.bootrf = vector(length=B)

set.seed(123)
while(b <= B){
  resamp = sample(1:nrow(aq), size=nrow(aq), replace = TRUE)
  boottrain = aq[resamp, ]
  boottest = aq[-unique(resamp),]
  
  boottree = ctree(Ozone ~ Wind+Temp+Month, data = boottrain)
  bootlm = lm(Ozone ~ Wind+Temp+Month, data = boottrain)
  bootmob = glmtree(Ozone ~ Wind | Temp+Month, data=boottrain)
  bootrf = randomForest(data=boottrain, Ozone~Wind+Temp+Month)
  bootrpart = rpart(Ozone ~Temp+Wind+Month, data=boottrain, cp=0)
  cpt = bootrpart$cptable
  minrow <- which.min(cpt[,4])
  # Take geometric mean of cp values at min error and one step up 
  cplow.min <- cpt[minrow,1]
  cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
  cp.min <- sqrt(cplow.min*cpup.min)
  bootrpartmin = prune(bootrpart, cp=cp.min)
  se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
  # Take geometric mean of cp values at min error and one step up 
  cplow.1se <- cpt[se.row,1]
  cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
  cp.1se <- sqrt(cplow.1se*cpup.1se)
  bootrpartse = prune(bootrpart, cp=cp.1se)
  
  MSPE.boottree[b] = mean((predict(boottree, newdata = boottest) - boottest$Ozone)^2)
  MSE.boottree[b] = mean((predict(boottree, newdata = boottrain) - boottrain$Ozone)^2)
  MSPE.bootmob[b] = mean((predict(bootmob, newdata = boottest) - boottest$Ozone)^2) 
  MSE.bootmob[b] = mean((predict(bootmob, newdata = boottrain) - boottrain$Ozone)^2)
  MSPE.bootlm[b] = mean((predict(bootlm, newdata = boottest) - boottest$Ozone)^2) 
  MSE.bootlm[b] = mean((predict(bootlm, newdata=boottrain) - boottrain$Ozone)^2)
  MSPE.bootrpartmin[b] = mean((predict(bootrpartmin, newdata = boottest) - boottest$Ozone)^2) 
  MSE.bootrpartmin[b] = mean((predict(bootrpartmin, newdata=boottrain) - boottrain$Ozone)^2)
  MSPE.bootrpartse[b] = mean((predict(bootrpartse, newdata = boottest) - boottest$Ozone)^2) 
  MSE.bootrpartse[b] = mean((predict(bootrpartse, newdata=boottrain) - boottrain$Ozone)^2)
  MSE.bootrf[b] = mean((predict(bootrf, newdata=boottrain) - boottrain$Ozone)^2)
  MSPE.bootrf[b] = mean((predict(bootrf, newdata=boottest) - boottest$Ozone)^2)
  
  b=b+1
}

#Bootstrap errors
MSEbooterrors = data.frame(LM = MSE.bootlm, MOB = MSE.bootmob, Tree= MSE.boottree, Rpartmin = MSE.bootrpartmin,
                           Rpartse = MSE.bootrpartse, RF = MSE.bootrf)
MSPEbooterrors = data.frame(LM = MSPE.bootlm, MOB = MSPE.bootmob, Tree= MSPE.boottree, Rpartmin = MSPE.bootrpartmin,
                            Rpartse = MSPE.bootrpartse, RF=MSPE.bootrf)
minMSPE = apply(as.matrix(MSPEbooterrors), 1, min)

#Visualizing bootstrap errors
boxplot(MSEbooterrors, main="Bootstrap in-sample error")
boxplot(MSPEbooterrors, main="Bootstrap validation error")
boxplot(sqrt(MSPEbooterrors/minMSPE), main="Sqrt relative validation error", ylab="sqrt-error")

#removing 1se rpart, ctree
minMSPE = apply(as.matrix(MSPEbooterrors[,c(1,2,4)]), 1, min)
boxplot(MSEbooterrors[,c(1,2,4,6)], main="Bootstrap in-sample error", xlab="Method", ylab="Error")
boxplot(MSPEbooterrors[,c(1,2,4,6)], main="Bootstrap validation error", xlab="Method", ylab="Error")
boxplot(sqrt(MSPEbooterrors[,c(1,2,4)]/minMSPE), main="Sqrt relative validation error", ylab="sqrt-error")

#fit to full data
aqfulllm = lm(Ozone ~ Wind+Temp+Month, data = aq)
summary(aqfulllm)
lrtest(lm(Ozone~Wind+Temp, data=aq), aqfulllm)

library(car)
marginalModelPlots(aqfulllm)

aqfullmob = glmtree(Ozone~ Wind | Temp+Month, data=aq)
summary(aqfullmob)
plot(aqfullmob)

aqfulltree = ctree(Ozone ~ Wind+Temp+Month, data = aq)
aqfullrpart = rpart(Ozone ~Temp+Wind+Month, data=aq, cp=0)
cpt = aqfullrpart$cptable
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)
aqfullrpartmin = prune(aqfullrpart, cp=cp.min)

plot(as.party(aqfullrpartmin), type = "simple", main = "Pruned tree")
