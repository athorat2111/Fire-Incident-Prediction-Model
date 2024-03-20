#training data
mydata <- read.csv("F:/VIT/SY/SEM2/DS/CP/LitModel-main/content/final_data.csv")
View(mydata)
str(mydata)
mydata$Total.loss <- as.numeric(mydata$Total.loss)
mydata$District <- as.factor(mydata$District)
mydata$Alarm.Date <- as.factor(mydata$Alarm.Date)
mydata$Zip <- as.factor(mydata$Zip)
mydata$Property.Use <- as.factor(mydata$Property.Use)
mydata$Fire <- as.factor(mydata$Fire)
str(mydata)
# Fire : 1- High risk ; 0- low risk

set.seed(123)
train.index<-sample(row.names(mydata),0.6*dim(mydata)[1])
valid.index<-setdiff(row.names(mydata),train.index)
train<-mydata[train.index, ]
validate<-mydata[valid.index, ]
summary(train)

#Plot to visualize the distribution of data
hist(Total.loss)
hist(log(train$Total.loss))


#Matrix plots to check the relation between different predictors with Total losses (run all plots together)
#include percentage of false to true alarms#

par(mfrow=c(2,2))
plot(mydata$Property.Use,log(mydata$Total.loss),main = "Total Losses vs Property use",xlab="Property use",ylab="logarithm of Total losses") #need ggplot#
plot(mydata$District,log(mydata$Total.loss),main = "Total Losses vs District",xlab="District",ylab="logarithm of Total losses")
plot(mydata$Zip,log(mydata$Total.loss),main = "Total Losses vs Zip code",xlab="Zip code",ylab="logarithm of Total losses")
plot(mydata$Incident.Type,log(mydata$Total.loss),main = "Total Losses vs Incident type",xlab="Incident Type",ylab="logarithm of Total losses")


#pivot table to find risk level of incidents in each zip code
library(pivottabler)
par(mfrow=c(1,1))
pvt <- PivotTable$new()
pvt$addData(mydata)
pvt$addColumnDataGroups("Fire")
pvt$addRowDataGroups("Zip")
pvt$defineCalculation(calculationName="Fire", summariseExpression="n()")
pvt$renderPivot()


train$Incident.Type <- as.numeric(train$Incident.Type)
train$Alarm.Date <- as.numeric(train$Alarm.Date)
train$District <- as.numeric(train$District)
train$Zip <- as.numeric(train$Zip)
train$Property.Use <- as.numeric(train$Property.Use)
validate$Incident.Type <- as.numeric(validate$Incident.Type)
validate$Alarm.Date <- as.numeric(validate$Alarm.Date)
validate$District <- as.numeric(validate$District)
validate$Zip <- as.numeric(validate$Zip)
validate$Property.Use <- as.numeric(validate$Property.Use)

#correlations
library(corrplot)
par(mfrow=c(1,1))
correlations <- cor(train[,1:6])
corrplot(correlations, method="circle")


##Logistic Regression
logmodel<-glm(Fire~ Property.Use + Incident.Type + District +Zip, data = train, family = binomial)
summary(logmodel)
par(mfrow=c(2,2))
plot(logmodel)


#evaluate performance 
glm.probs <- predict(logmodel,type = "response")
glm.probs[1:5]
sum(glm.probs[1:5])/5
glm.pred <- ifelse(glm.probs > 0.64, "True", "False")


#performance AUC 

library(ROCR)
library(rpart)
par(mfrow=c(1,1))
pred <- predict(logmodel, newdata =validate,OOB=TRUE)
pred <- prediction(as.numeric(pred), as.numeric(validate$Fire))
perf <- performance(pred,"tpr","fpr")
plot(perf, main="lift curve", colorize=T)

#Auc
auc<- performance(pred,measure = "auc")
auc <- auc@y.values[[1]]
lr<-as.numeric(auc)
lr

#Random Forest
library(party)
library(randomForest)
output.forest <- randomForest(Fire~ Property.Use + Incident.Type + District +Zip, data = train)
print(output.forest)
predictionrf<-predict(output.forest,newdata = validate)
table(predictionrf,validate$Fire)
varImpPlot(output.forest)
results<-cbind(predictionrf,validate$Fire)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
rf_acc<-sum(predictionrf==validate$Fire)/nrow(validate)
rf<-as.numeric(rf_acc)
rf

if(lr>rf){
  print("Logistic Regression gives better results than Random Forests")
}else{
  print("Random Forests gives better results than Logistic Regressions")
}
