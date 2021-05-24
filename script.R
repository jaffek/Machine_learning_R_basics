dec = "."
delim = ","  
data <- read.csv("***/dane.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
#dataX <- read.csv("***/dane.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
colnames(data) <-
  c("age","job","marital",
    "education","has_credit","balance",
    "housing_loan","personal_loan","contact","day","month","duration","campaign","pdays","previous","poutcome", "Diagnosis")

 
testdata <- read.csv("***/bank_marketing_weka_dataset(1).csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
colnames(testdata) <-
  c("age","job","marital",
    "education","has_credit","balance",
    "housing_loan","personal_loan","contact","day","month","duration","campaign","pdays","previous","poutcome", "Diagnosis")

counts <- table(testdata$Diagnosis, testdata$poutcome)
barplottest <- c(counts[7],counts[8])
lbls <- c("No","Yes")
pct <- round(barplottest/sum(barplottest)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
colors = c("darkblue", "red")
pie(barplottest, labels = lbls,col=colors,main="success")


counts <- table(testdata$Diagnosis, testdata$day)
barplot(counts, xlab="Day", col=c("darkblue","red"),
        legend = rownames(counts))

data55 <- as.matrix(data)
heatmap(data55[,-17],scale="column", Rowv=NA, Colv=NA)


data <- data.frame(lapply(data, as.numeric))install.packages("ggplot2")
data$Diagnosis <- as.factor(data$Diagnosis)
levels(data$Diagnosis) <- c("no","yes")

train_cont <-  caret::trainControl(method = "LOOCV")
models2 <-  caret::train(Diagnosis~., method=c('rf'), data=dataB,trControl = train_cont,verbose = TRUE)


pred2_cf <- models2$pred$pred
test2_cf = models2$pred$obs
confusionMatrix(pred2_cf, test2_cf)



par(mfrow=c(4,4))
for(i in 1:16){
  boxplot(data[[i]] ~ Diagnosis, data = data, main=names(data)[i])
  }; rm(i)

par(mfrow=c(1,1))
corrgram::corrgram(data[,1:16], order=TRUE,
lower.panel=corrgram::panel.ellipse,
upper.panel=corrgram::panel.shade, text.panel=corrgram::panel.txt)


par(mfrow=c(4,4))
for(p in 1:16){
  caret::featurePlot(data[,p],data$Diagnosis,plot="density")
}; rm(p)


pca1 <- prcomp(data[,1:16], center = TRUE, scale. = TRUE)
summary(pca1)


dataB <- caret::downSample(x = data[,-17], y = data$Diagnosis)
colnames(dataB) <-
  c("age","job","marital",
    "education","has_credit","balance",
    "housing_loan","personal_loan","contact","day","month","duration","campaign","pdays","previous","poutcome", "Diagnosis")


#ggplot(data=data, aes(data$marital)) + geom_histogram()


#NbCllust
NbClust::NbClust(dataC, distance="euclidean", min.nc=2, max.nc=4, method="complete", index="all")



ind1 <- caret::createDataPartition(dataB$Diagnosis, p = 0.7, list=FALSE)
train1 <- dataB[ind1,]
test1 <- dataB[-ind1,]


models1 <- lapply(c( 'xgbTree', 'rf', 'svmRadial', 'svmPoly'), function (met) { caret::train(Diagnosis~., method=met, data=train1)})

pred1a <- predict(models1[[1]], test1)
pred1b <- predict(models1[[2]], test1)
pred1c <- predict(models1[[3]], test1)
pred1d <- predict(models1[[4]], test1)

caret::confusionMatrix(pred1a, test1$Diagnosis)
caret::confusionMatrix(pred1b, test1$Diagnosis)
caret::confusionMatrix(pred1c, test1$Diagnosis)
caret::confusionMatrix(pred1d, test1$Diagnosis)


#write.csv(dataX, "datalol.csv",row.names=FALSE)

fit <- neuralnet::neuralnet(Diagnosis ~ duration +
                              contact + pdays,
                            data = train1, hidden = c(10,10), learningrate = 0.1,
                            linear.output = FALSE, algorithm = "backprop",
                            rep = 5, act.fct = "tanh")



control <- caret::trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='random')
set.seed(1)

rf_gridsearch <- caret::train(Diagnosis ~ ., 
                       data = train1[,-18],
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneLength  = 60, 
                       trControl = control)
print(rf_gridsearch)



plot(fit)
tuned_svm <- e1071::tune(svm, Diagnosis ~ ., data=train1, kernel = "polynomial",ranges = list(gamma = 2^(-1:1), degree = 2:4))
print(tuned_svm)

