#Install and load the kknn package
#install.packages("kknn")
library(kknn)

# assign data to df called "ccdata"
ccdata <- data

# split dataset into training, validation, and testing sets comprising 70%, 15%, and 15% of ccdata respectively
splitSample <- sample(1:3, size=nrow(ccdata), prob=c(0.70,0.15,0.15), replace = TRUE)
train <- ccdata[splitSample==1,]
valid <- ccdata[splitSample==2,]
test <- ccdata[splitSample==3,]

### Set up knn
ccdata_kknn <- kknn(formula = formula(R1~.), train, test, na.action = na.omit(), k=3, distance = 12, kernel = "optimal", scale=TRUE)
ccdata_kknn
kknn.dist(train, test, k=10, distance=2)

#view predictions
pred_kknn <- predict(ccdata_kknn,ccdata[,1:10])
pred_kknn

# get model accuracy
pred_knn <- rep(0, (nrow(ccdata)))
pred_knn[i] <- as.integer(fitted(CCmodel_knn)+0.5)
sum(pred_knn == ccdata$R1)/nrow(ccdata) 

# Find best K value
accuracy=numeric()
n<-nrow(data)
for(kvalue in 1:20)
{
  predicted <- rep(0, nrow(data))
  for(i in 1:n){
    model = kknn(R1~.,data[-i,], data[i,],k=kvalue, scale=TRUE)
    # Get result
    predicted[i] <-as.integer(fitted(model)+0.5)
    print(predicted)
  }
  
  accuracy = c(accuracy,sum(predicted==data[,11])/nrow(data))
  
}                  

accuracy
bestaccuracy <- max(accuracy) 
bestaccuracy

bestk <- which.max(accuracy)
bestk

kmax <- 20
accuracy_cv <- rep(0,kmax)

#kfold validation
for (k in 1:kmax){
  kmodel <- cv.kknn(R1~.,data,kcv=10, k=k, scale=TRUE) 
  predicted <- as.integer(kmodel[[1]][,2] + 0.5) #print yhat and convert to 0 or 1
  accuracy_cv[k]= sum(predicted == data$R1)
}

#kmodel
accuracy_cv
max(accuracy_cv)
which.max(accuracy_cv) 

# split dataset into training, validation, and testing sets comprising 70%, 15%, and 15% of ccdata respectively
splitSample <- sample(1:3, size=nrow(ccdata), prob=c(0.70,0.15,0.15), replace = TRUE)
training <- ccdata[splitSample==1,]
validation <- ccdata[splitSample==2,]
testing <- ccdata[splitSample==3,]

#HW code
kknn_model <- kknn(formula = formula(training), training, testing, na.action = na.omit(), 
                   k = 22, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
                   contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))
fit = fitted(model)

#build model
kknn(formula = formula(R1~.), training, testing, na.action = na.omit(), k=5, distance = 2, kernel = "optimal", scale=TRUE)
#Build cross validation
cv.kknn(validation, testing, kcv=10, distance=2) 

#view predictions
pred_kknn <- predict(ccdata_kknn,ccdata[,1:10])
pred_kknn

# get model accuracy
pred_knn <- rep(0, (nrow(ccdata)))
pred_knn[i] <- as.integer(fitted(CCmodel_knn)+0.5)
sum(pred_knn == ccdata$R1)/nrow(ccdata) 

# cross validation
m<-nrow(data)

#set 2 model based on different kernel, kmax and 2 different destination values
kknn1<-train.kknn(R1 ~ ., data=training, kernel = c("rectangular", "epanechnikov", "triangular", "optimal"), distance=1, kmax= 100, scale=TRUE)
kknn2<-train.kknn(R1 ~ ., data=training, kernel = c("rectangular", "epanechnikov", "triangular", "optimal"), distance=2, kmax= 100, scale=TRUE)

kknn1
#kknn1
#Type of response variable: continuous
#minimal mean absolute error: 0.1834061
#Minimal mean squared error: 0.1018426
#Best kernel: triangular
#Best k: 34

kknn2
#kknn2
#Type of response variable: continuous
#minimal mean absolute error: 0.1746725
#Minimal mean squared error: 0.1011238
#Best kernel: triangular
#Best k: 48

#choose model based on kknn2 specification above
bestmodel<-kknn(R1 ~ ., validation, testing, k=48, kernel ="triangular", distance=2, scale=TRUE)

modelPred <- as.integer(bestmodel$fitted.values +0.5)
accuracy2 = sum(modelPred == d.test2[,11]) / nrow(d.test2)
accuracy2

#> accuracy2
#[1] 0.8061224
#best K=48