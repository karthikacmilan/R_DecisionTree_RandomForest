#splitting 80-20% 
samplespace = sample(2,nrow(HEC),replace=TRUE,prob = c(0.8,0.2))
trainingdata = HEC[samplespace==1,]
testdata = HEC[samplespace==2,]
#random forest
library(randomForest)
ran = randomForest(V3~.,data = trainingdata, ntree = 30, proximity = TRUE)
table(predict(ran),trainingdata$V3)
#print rand for value 
print(ran)
#print the attrbutes 
attributes(ran)
#plot
plot(ran)
#saving to pdf
pdf("randomf.pdf")
plot(ran)
graphics.off()
#find the importance of variables 
importance(ran)
pdf("importance.pdf")
varImpPlot(ran)
#saving to pdf
graphics.off()

