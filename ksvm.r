# Building a Support Vector Machine with kernlab's ksvm

library(data.table)

data = read.table("//es00cifs00/users$/egagne/WINNT/System/Desktop/credit_card_data-headers.txt", header=TRUE)

#Install and load kernlab package
# install.packages("kernlab")
library(kernlab)

#set the seed, so that results will be replicable
set.seed(3)

# call ksvm
ccmodel <-  ksvm(as.matrix(data[,1:10]) #training data is cols 1-10 of df data
                 ,as.factor(data[,11]), #test data is the 11th col of df data
                 type="C-svc", #specify C classification
                 kernel="vanilladot", #linear kernel
                 C=100, # weight of error vs margin
                 scaled=TRUE) #scale the data
ccmodel # Training error: 0.13608
attributes(ccmodel)

# calculate a1.am (coefficients)
a <- colSums(ccmodel@xmatrix[[1]] * ccmodel@coef[[1]])
a
# calculate a0 (intercept)
a0 <- -ccmodel@b
a0
# view predictions
pred <- predict(ccmodel,data[,1:10])
pred
# get model accurracy aka 1-training error
sum(pred == data[,11]) / nrow(data) # = 0.8639144

# Explore possible C values by manually plugging them in
# It looks like 100 is the best option here. Maybe I would get different results with a funtion that iterates through all the possible values of C
# call ksvm
cmodels <-  ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
# calculate a1.am
a <- colSums(cmodels@xmatrix[[1]] * cmodels@coef[[1]])
a
# calculate a0
a0 <- -cmodels@b
a0
# view predictions
pred <- predict(cmodels,data[,1:10])
pred
# get model accuracy
sum(pred == data[,11]) / nrow(data) 

#C=0.0001 --> 0.5474006
#C=0.001 --> 0.8379205
#C=0.01 --> 0.8639144
#C=1 --> 0.8639144
#C=10 --> 0.8639144
#C=50 --> 0.8639144
#C=100 --> 0.8639144
#C=150 --> 0.8639144
#C=200 --> 0.8639144
#C=500 --> 0.8639144
#C=1000 --> 0.8623853
#C=10000 --> 0.8623853
#C=100000 --> 0.8639144
#C=1000000 --> 0.6253823
#C=10000000 --> 0.5458716
#C=100000000 --> 0.6636086

# Iterate through many possible C values and return the top 5 values
allC = seq(from = 0.0001, to = 100000000, by = 0.1) #create a sequence with lots of possible C values
allC_samp = sample(x = allC, size=100) #create a random sample of the sequence
hist(allC_samp) #see that the sample is uniformly distributed
results=list()
for(i in 1:length(allC_samp)){
  c.model <-  ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=allC_samp[[i]],scaled=TRUE)
  results[[i]]=data.table(C=0.0001,accuracy=sum(pred == data[,11])/nrow(data))
}
rbindlist(results)
results_dt = rbindlist(results)
results_dt[order(-accuracy)][1:5] #return the 5 C values with the highest accuracy

#Try another way to iterate through C values
CVal <- 1
C_vector <- numeric()
error <- numeric()

while(CVal <= 10000)
{
  
  # call ksvm. Vanilladot is a simple linear kernel. 
  model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=CVal,scaled=TRUE)
  
  # see what the model predicts 
  pred <- predict(model,data[,1:10]) 
  pred
  
  #actual classification
  sum(pred == data[,11]) / nrow(data)
  error <- c(error, model@error)
  
  
  C_vector <- c(C_vector, CVal)
  
  # increment C
  CVal <- CVal * 10
}

# store C and error 
pt_data <- data.frame(C_vector, error)
pt_data

#cross validation
svm_cross <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type = "C-svc",kernel = "vanilladot",C =
                    100,scaled = TRUE,cross=10)

# Let's see if a linear kernel really is the best option
# Build a list of all available kernels in ksvm
# Iterate through them and display accuracy metric for each in a table
myKernels = c("vanilladot","polydot","besseldot", "rbfdot", "tanhdot", "laplacedot", "anovadot", "splinedot", "stringdot")
results=list()
for(i in 1:length(myKernels)){
  # call ksvm
  models <-  ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel=myKernels[[i]],C=100,scaled=TRUE)
  # calculate a1.am
  a <- colSums(models@xmatrix[[1]] * models@coef[[1]])
  a
  # calculate a0
  a0 <- -models@b
  a0
  # view predictions
  pred <- predict(models,data[,1:10])
  pred
  # view accuracy metric
  results[[i]]=data.table(kernel=myKernels[[i]],accuracy=sum(pred == data[,11]) / nrow(data))
}
rbindlist(results)
