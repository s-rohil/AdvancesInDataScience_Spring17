# install.packages("rpart")
# library(rpart)
# install.packages("plyr")
# library(plyr)
file<-file.choose()
train.data<-read.csv(file=file.path(file),stringsAsFactors = FALSE,header = TRUE)
file1<-file.choose()
test.data<-read.csv(file=file.path(file1),stringsAsFactors = FALSE,header = TRUE)
train.data$trainSet<-TRUE
test.data$trainSet<-FALSE
test.data$Survived<- NA

summary(train.data)
summary(test.data)
#Preprocessing of data

#combining and cleaning together
full.data<-rbind(train.data,test.data)

#Cleaning Embarked column
full.data[full.data$Embarked=='',"Embarked"] <- 'S'


#using prediction for cleaning Age and Fare
#Fare
max.limit<-boxplot.stats(full.data$Fare)$stats[5]
max.limit1<-boxplot.stats(full.data$Fare)$stats[4]
avg.limit<-(max.limit+max.limit1)/2
fare.filter<-full.data$Fare<avg.limit
#full.data[filter,]  # not outliers
fare_model<-lm(Fare ~ Pclass+Sex+Age+SibSp+Parch+Embarked,data=full.data[fare.filter,])

fare_newdata<-full.data[is.na(full.data$Fare),c("Pclass","Sex","Age","SibSp","Parch","Embarked")]
fare_predict<-predict(fare_model,newdata = fare_newdata )
full.data[is.na(full.data$Fare),"Fare"] <-fare_predict


#Predicting missing ages
age.limit<-boxplot.stats(full.data$Age)$stats[5]
age.limit1<-boxplot.stats(full.data$Age)$stats[4]
avg.age<-(age.limit+age.limit1)/2
age.filter<-full.data$Age<avg.age
#full.data[filter,]  # not outliers
age_model<-lm(Age ~ Pclass+Sex+SibSp+Parch,data=full.data[age.filter,])

age_newdata<-full.data[is.na(full.data$Age),c("Pclass","Sex","SibSp","Parch")]
age_predict<-predict(age_model,newdata = age_newdata )
full.data[is.na(full.data$Age),"Age"] <-age_predict

#classifying variables as factors
full.data$Pclass<-as.factor(full.data$Pclass) #classify as ordinal. have to do
full.data$Sex<-as.factor(full.data$Sex)
full.data$Embarked<-as.factor(full.data$Embarked)


# separating back into train and test data
train.data<-full.data[full.data$trainSet==TRUE,]
test.data<-full.data[full.data$trainSet==FALSE,]

train.data$Survived<-as.factor(train.data$Survived)

#model for predicting survived
#Survived~Pclass+Age+Sex+Sibsp+Parch+Fare+Embarked,data=train.data
titanic.model<-rpart(Survived~Pclass+Age+Sex+SibSp+Parch+Fare+Embarked,data=train.data,control = rpart.control(minbucket = 5,xval = 15,cp=0),method='class')
plotcp(titanic.model)
printcp(titanic.model)

#pruning the tree using cp=0.02
new_titanic.model<-prune(titanic.model,cp=0.0017)
plot(new_titanic.model,uniform = TRUE)
text(new_titanic.model,cex=0.75,pretty=0)

#prediction of titanic test data using the pruned model
predict.survived<- predict(new_titanic.model,test.data,type='class')
table<-table(predict.survived)
survival<-table[2]/(table[1]+table[2])*100
#str(predict.survived)

