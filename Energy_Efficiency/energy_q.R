#install.packages("xlsx")
#install.packages("rpart")
#library(rpart)
#library(xlsx)

file<-file.choose()
set.seed(619)
new_data<-read.xlsx(file = file.path(file),header = TRUE,sheetIndex = 1)
#data1<-lapply(new_data, na.omit)
#data1<-data1[ , ! apply( data1,c(1,2,3,4,5,6,7,8,9,10),FUN=function(x) all(is.na(x)) ) ]
data1<-new_data[1:10]
data<-na.omit(data1)
#using 80/20 split for training and test data
train<-sample(1:nrow(data),0.8*nrow(data))
#taking y1 as the response variable
rpart.data<-rpart(Y1~X1+X2+X3+X4+X5+X6+X7+X8,data = data,subset=train)
#summary(rpart.data)
plotcp(rpart.data)
printcp(rpart.data)
text(rpart.data,pretty = 0,cex=0.75)
#tree pruning using cp=min.xerror which is calculated programatically
min.xerror <- rpart.data$cptable[which.min(rpart.data$cptable[,"xerror"]),"CP"]
prune.data<- prune(rpart.data,cp=min.xerror)
plot(prune.data,uniform=TRUE)
text(prune.data,pretty = 0,cex=0.75)

#prediction using the test data as the remaning 30% of the energy dataset
predict.data<-predict(prune.data,data[-train,])
test.data<-data[-train,"Y1"]
plot(predict.data,test.data)

#calculating the mean squared error
MSE<-mean((predict.data-test.data)^2)
RMSE<-sqrt(MSE)
#RMSE gives the value of how close the predicted value is similar to the observed value. Lower score indicates a better model.
