#importing Hair and Eye Color of Statistics Students
HEC <- read.csv("~/HairEyeColor.csv", header=FALSE)
dim(HEC)
attributes(HEC)
#training set and test set 
#Decision tree
set.seed(1234)
#splitting 80-20% 
samplespace = sample(2,nrow(HEC),replace=TRUE,prob = c(0.8,0.2))
trainingdata = HEC[samplespace==1,]
testdata = HEC[samplespace==2,]
#train a decision tree
library(rpart)
space = V5~V1+V2+V4+V3
space_rpart = rpart(space,data = trainingdata , control = rpart.control(minsplit = 5))
attributes(space_rpart)
print(space_rpart)
plot(space_rpart) 
text(space_rpart,use.n = T)
#choose minimum prediction error
sel = which.min(space_rpart$cptable[,"xerror"])
cp = space_rpart$cptable[sel,"CP"]
sp_prune = prune(space_rpart,cp = cp)
print(sp_prune)
#plotting the decision tree
plot(sp_prune)
text(sp_prune,use.n = T)
#saving initial scater plot to pdf
library(ggplot2)
pdf("plot.pdf")
qplot(V1,V2,data=HEC,facets=V3 ~.)
graphics.off()
#saving decision tree to pdf
pdf("dt.pdf")
plot(sp_prune)
text(sp_prune,use.n = T)
graphics.off()
#prediction result
pre = predict(sp_prune, newdata = testdata)
xlim = range(HEC$V5)
plot(pre ~ V5, data = testdata , xlab= "observed", + ylab = "prediction", ylim = xlim, xlim = xlim)
abline(a=0,b=1)


