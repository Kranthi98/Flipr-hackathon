#importing the libraries we require 
library(readxl)
library(tseries)
library(forecast)

#importing the data 
data.file = read_excel("Train_dataset.xlsx",sheet = 2)
class(data.file)
data.file = as.data.frame(data.file)

#just check how is the data
head(data.file)
data.file[1,]
dim(data.file)
#checking whether there are any missing values are present
sum(is.na(data.file))



#creating the model time series model:
train.data = data.file[,2:7]
comparison = data.file[,8]

#converting our data to time series
#making time series models using auto arima 
train.data = apply(train.data,1,function(x) ts(x))
mods = apply(train.data,2,function(x) auto.arima(x))


#predicting the values 
model.pred = list()
for(i in 1:10714)
{
  model.pred[[i]] = forecast(mods[[i]],h = 1)
}

#obtaining separate of values of forecast with different confidence intervals
val = list()
val1 = list()
val2 = list()
val.1 = list()
val.2 = list()
for(i in 1:10714)
{
  val.1[[i]] = as.numeric(model.pred[[i]]$lower[1])
  val.2[[i]]= as.numeric(model.pred[[i]]$lower[2])
  val[[i]]= as.numeric(model.pred[[i]]$mean)
  val1[[i]]= as.numeric(model.pred[[i]]$upper[1])
  val2[[i]]= as.numeric(model.pred[[i]]$upper[2])
}


val = as.vector(val)
val =as.vector(t(val))
val.1 = as.vector(t(val.1))
val.2 = as.vector(t(val.2))
val1 = as.vector(t(val1)) #Hi 80
val2 = as.vector(t(val2))
View(val2)
data.com = cbind(val.1,val.2,val,val1,val2,comparison)
head(data.com)
View(data.com)
#we can see highest value of 80percent confidence intercal is best suited 
#when made comparison with the original values
#hence we will also take those values in our original model



########################




#converting our data ito time series using apply function
#since the data we have to forecast is along the rows we apply the function row wise
data.file = apply(data.file[,2:8],1,function(x) ts(x))          
View(data.file)


#the time series is now changed along the columns
#creating arima models along the columns
models = apply(data.file,2,function(x) auto.arima(x))

#creating an empty list for storing the predicted values
pred.op = list()

#forecasting the values 
for(i in 1:10714)
{
  pred.op[[i]] = forecast(models[[i]],h = 1)
}
pred[[3]]
head(df)

#since the values with hi 80 are close to our data
#iam expecting the same in this caase too hence taking hi 80 
#hence taking the uppe values of 80 percent confident interval
predicted.output = list()
for(i in 1:10714){
predicted.output[[i]] = as.numeric(pred.op[[i]]$upper[1])
}
head(predicted.output)
predicted.output[[8]]


#converting the list to a vector
predicted.output = as.vector(predicted.output)

s.no = 1:10714
data = cbind(s.no,predicted.output)
write.csv(data,"27.diuresis.csv")
head(data)
