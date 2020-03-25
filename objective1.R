#importing the required libraries
library(readxl)
library(tidyverse)
library(mice)
library(Matrix)
library(xgboost)
library(caTools)

#importing the train dataset
dataset = read_excel("Train_dataset.xlsx",sheet = 1)
var.des = read_excel("Variable_Description.xlsx")
View(var.des)

#importing the test dataset
test.data = read_excel("Test_dataset.xlsx")
dim(test.data)
dim(dataset)
test.data$infect_prob = 0
dataset$train = 1 
test.data$train = 0
names(dataset)
#we will store the people ID
ID = test.data$people_ID

#delete the ID column since they dont have any effect on infect probability
dataset[,1] <- NULL
test.data$people_ID <- NULL



#we will rename the names of datset and make them equal

names(dataset) <- c("region","gender","designation","names","married",
                    "children","occupation","mode_transport",
                    "cases","deaths","comorbidity","age","coma_score","pulmonary_score",
                    "cardiological_press","diuresis","platelets","HBB","dimer","heart_rate",
                    "HDL.cholesterol","charlson.index","Blood.glucose","Insurance",
                    "salary","FT","infect_prob","train")


#make the names same since having an same names is preferable
names(test.data) <- names(dataset)

dim(dataset)


#getting info about the missing data 
missing <- sapply(dataset, function(x) sum(is.na(x)))
missing.per <- missing*nrow(dataset)/100
barplot(missing.per)

missing

#removing the designation and names since they dont have any effect on data
dataset[,3:4] <- NULL
test.data[,3:4] <- NULL

#deleting the the NA columns since they are only few of them
dataset <- subset(dataset,is.na(mode_transport) == FALSE)
dataset <- subset(dataset,is.na(HBB) == FALSE)

str(dataset)


#converting the factor variables
dataset$region = as.factor(dataset$region)
dataset$gender = as.factor(dataset$gender)
dataset$children = as.integer(dataset$children)
dataset$occupation = as.factor(dataset$occupation)
dataset$mode_transport = as.factor(dataset$mode_transport)
dataset$comorbidity = as.factor(dataset$comorbidity)
dataset$pulmonary_score = as.factor(dataset$pulmonary_score)
dataset$cardiological_press = as.factor(dataset$cardiological_press)
dataset$married = as.factor(dataset$married)

test.data$region = as.factor(test.data$region)
test.data$gender = as.factor(test.data$gender)
test.data$children = as.integer(test.data$children)
test.data$occupation = as.factor(test.data$occupation)
test.data$mode_transport = as.factor(test.data$mode_transport)
test.data$comorbidity = as.factor(test.data$comorbidity)
test.data$pulmonary_score = as.factor(test.data$pulmonary_score)
test.data$cardiological_press = as.factor(test.data$cardiological_press)
test.data$married = as.factor(test.data$married)

str(test.data)

nrow(dataset)


#mice package can be used to impute the missing data
#But making life easier we will impute some variables with the mean

#heart rate
head(dataset$heart_rate)
HR = dataset$heart_rate
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset$heart_rate = HR

#dimer values
head(dataset$dimer)
HR = dataset$dimer
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset$dimer = HR
head(dataset$dimer)


#insurance
head(dataset$Insurance)
HR = dataset$Insurance
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset$Insurance = HR
head(dataset$Insurance)

#platelets
head(dataset$platelets)
HR = dataset$platelets
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset$platelets = HR
head(dataset$platelets)

#diuresis
head(dataset$diuresis)
HR = dataset$diuresis
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset$diuresis = HR
head(dataset$diuresis)


#imputing the remaining the values using mice package
impute <- mice(data = dataset,m = 1,maxit = 3,method = "cart")
dataset <- complete(impute, 1 )

head(dataset$diuresis)
str(dataset)
str(test.data)

dataset$coma_score <- as.integer(dataset$coma_score)
dataset$deaths <- as.integer(dataset$deaths)

#we will bind the train and test 
sparse <- rbind(dataset,test.data)
dim(sparse)

head(sparse)


#one hot encoding using sparse.model.matrix
sparsem = sparse.model.matrix(infect_prob~.-infect_prob,data = sparse)

sparsem@Dimnames[[2]]
head(sparsem)
dim(sparsem)
x.data <- dim(dataset)
x.test <- dim(test.data)
dim(sparsem)
sparsem@Dimnames[[2]]




#watchlist to see how the rmse values are changing w.r.t test and train data
watchlist = list(train = train.matrix,test = test.matrix)
parameters = list("objective" = "reg:squarederror")


#splitting the train data into train and test to find the best iteration
split = sample.split(dataset,SplitRatio = 0.7)

training = subset(dataset,split == TRUE)
testing = subset(dataset,split == FALSE)
head(testing)

#sparsing the both training and testing data
trainingm <- sparse.model.matrix(infect_prob~.-infect_prob,data = training)
testingm <- sparse.model.matrix(infect_prob~.-infect_prob,data = testing)

#converting the data as xgb.DMatrix
tr.m <- xgb.DMatrix(data = trainingm,label = training$infect_prob)
te.m <- xgb.DMatrix(data = testingm,label = testing$infect_prob)
watchlist = list(train = tr.m,test = te.m)
parameters = list("objective" = "reg:squarederror")


#training the model
mo = xgb.train(data = tr.m,
                  params = parameters,
                  watchlist = watchlist,
                  nrounds = 500,
               eta = 0.01,
               )
#predicting the data
pred.dummy <- predict(mo,newdata = te.m)

#seeing how close the values are
head(pred.dummy)
head(testing$infect_prob)

#finding the iteration where the test rmse is low
a <- mo$evaluation_log
plot(a$iter,a$train_rmse,col ="blue")
lines(a$iter,a$test_rmse,col = "red")
min(a$test_rmse)
a[a$test_rmse == min(a$test_rmse)]


#separating the sparse matrix into actual train ad test matrices
trainm <- (sparsem[1:10695,])
head(trainm)
testm <- sparsem[10696:25193,]
head(testm)
str(trainm)

#removing the train column it is added just to make sure we are separting them correctly
trainm <- trainm[,-48]
testm <- testm[,-48]



#making the data available as xgb.Dmatrix
training.matrix = xgb.DMatrix(data = trainm,label = dataset$infect_prob)
testing.matrix = xgb.DMatrix(data = testm)


#making the model with the best parameters obtained before
model.actual = xgboost(data = training.matrix,label = dataset$infect_prob,
                       nrounds = 433,
                       eta = 0.01,
                       objective = "reg:squarederror")

#predicting the values
predictions = predict(model.actual,newdata = testing.matrix)
predictions.mar20 <- predictions
head(predictions)


#writing the predicting values as excel file

output.mar20 <- cbind(ID,predictions.mar20)
write.csv(output.mar20,"output_mar20.csv")






#reading the 27mar diuresis values from the desktop 
#which are obtained in previous time series model

mar_27 <- read.csv("27.diuresis.csv")
head(mar_27)
head(ID)
head(test.data$Diuresis)

mar_27$X <- NULL
names(mar_27) <- c("S.no","Diuresis")

#replacing the diuresis values with 27mar diuresis values 

test.data$Diuresis <- mar_27$Diuresis
dim(test.data)
dim(dataset)


data.27 <- read_excel("Train_dataset.xlsx",sheet = 1)
data.27$Diuresis <- mar_27$Diuresis
head(data.27)
str(data.27)
data.27 = as.data.frame(data.27)
dim(data.27)

#doing all the preprocessing as we done before
#converting the factor variables
data.27$region = as.factor(data.27$region)
data.27$gender = as.factor(data.27$gender)
data.27$children = as.integer(data.27$children)
data.27$occupation = as.factor(data.27$occupation)
data.27$mode_transport = as.factor(data.27$mode_transport)
data.27$comorbidity = as.factor(data.27$comorbidity)
data.27$pulmonary_score = as.factor(data.27$pulmonary_score)
data.27$cardiological_press = as.factor(data.27$cardiological_press)
data.27$married = as.factor(data.27$married)


#renaming the name sso that it will be helpful in processing 
names(data.27) <- c("ID","region","gender","designation","names","married",
                    "children","occupation","mode_transport",
                    "cases","deaths","comorbidity","age","coma_score","pulmonary_score",
                    "cardiological_press","diuresis","platelets","HBB","dimer","heart_rate",
                    "HDL.cholesterol","charlson.index","Blood.glucose","Insurance",
                    "salary","FT","infect_prob")

data.27$names <- NULL
data.27$designation <- NULL
dim(data.27)



#calculating the missing data statistics
missing1 <- sapply(data.27,function(x) sum(is.na(x)))
missing1

#replacing few varibles missing data with the mean
HR = data.27$heart_rate
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
data.27$heart_rate = HR


head(data.27$dimer)
HR = data.27$dimer
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
data.27$dimer = HR
head(data.27$dimer)

head(data.27$Insurance)
HR = data.27$Insurance
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
data.27$Insurance = HR
head(data.27$Insurance)


head(data.27$platelets)
HR = data.27$platelets
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
data.27$platelets = HR
head(data.27$platelets)

head(data.27$diuresis)
HR = data.27$diuresis
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
data.27$diuresis = HR
head(data.27$diuresis)


#imputing the data using mice
impute1 = mice(data.27,m = 1,maxit = 1,method = "cart")

#completing the data using complete function
data.27 <- complete(impute1,1)
sum(is.na(data.27))
str(data.27)
ID.27 <- data.27$ID
data.27$ID <- NULL
dim(data.27)
data.27$infect_prob <- NULL


#sparsing the data
data.27m <- sparse.model.matrix(~.,data = data.27)


#previous created model has different feature names 
#hence creating one more model
#and doing all the steps we have done in previous case
dataset.27 = read_excel("Train_Dataset.xlsx",sheet = 1)

dataset.27$people_ID <- NULL

dataset.27$Designation <- NULL
dataset.27$Name <- NULL

dataset.27 <- as.data.frame(dataset.27)
str(dataset.27)
names(dataset.27) <-  c("region","gender","married",
                        "children","occupation","mode_transport",
                         "cases","deaths","comorbidity","age","coma_score","pulmonary_score",
                       "cardiological_press","diuresis","platelets","HBB","dimer","heart_rate",
                        "HDL.cholesterol","charlson.index","Blood.glucose","Insurance",
                        "salary","FT","infect_prob")




HR = dataset.27$heart_rate
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset.27$heart_rate = HR


head(dataset.27$dimer)
HR = dataset.27$dimer
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset.27$dimer = HR
head(dataset.27$dimer)

head(dataset.27$Insurance)
HR = dataset.27$Insurance
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset.27$Insurance = HR
head(dataset.27$Insurance)


head(dataset.27$platelets)
HR = dataset.27$platelets
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset.27$platelets = HR
head(dataset.27$platelets)

head(dataset.27$diuresis)
HR = dataset.27$diuresis
x = mean(HR,na.rm = TRUE)
for(i in 1:10695){
  HR[i] = ifelse(is.na(HR[i]),x,HR[i])
}
dataset.27$diuresis = HR
head(dataset.27$diuresis)


dataset.27$region = as.factor(dataset.27$region)
dataset.27$gender = as.factor(dataset.27$gender)
dataset.27$children = as.integer(dataset.27$children)
dataset.27$occupation = as.factor(dataset.27$occupation)
dataset.27$mode_transport = as.factor(dataset.27$mode_transport)
dataset.27$comorbidity = as.factor(dataset.27$comorbidity)
dataset.27$pulmonary_score = as.factor(dataset.27$pulmonary_score)
dataset.27$cardiological_press = as.factor(dataset.27$cardiological_press)
dataset.27$married = as.factor(dataset.27$married)

missing2 <- sapply(dataset.27,function(x) sum(is.na(x)))

missing2
impute.27.model <- mice(dataset.27,m= 1,maxit = 1,method = "cart")

dataset.27 <- complete(impute.27.model,1)


#spliting the dataset to find the best iteration
#and to avoid overfitting
split <- sample.split(dataset.27,SplitRatio = 0.7)
training.27 = subset(dataset.27,split == TRUE)
testing.27 = subset(dataset.27,split == FALSE)

#making sparse model matrix (one hot encoding)
#for train data
trainm.27 <- sparse.model.matrix(infect_prob~.-infect_prob,data = training.27 )
train.27.label = training.27$infect_prob

# for test data
testm.27 = sparse.model.matrix(infect_prob~.-infect_prob,data = testing.27 )
test.27.label = testing.27$infect_prob

#as.xgb.DMatrix
trainm.27matrix = xgb.DMatrix(data = trainm.27,label = train.27.label)
testm.27matrix = xgb.DMatrix(data = testm.27,label = test.27.label)


watchlist = list(train = trainm.27matrix,test = testm.27matrix)
parameters = list("objective" = "reg:squarederror")


#making the model with and feature names will be different with previous model
model.27 = xgb.train(data = trainm.27matrix,
               params = parameters,
               watchlist = watchlist,
               nrounds = 500,
               eta = 0.01,
)


#plotting the graphs for better understanding
d <- model.27$evaluation_log
plot(d$iter,d$train_rmse,col = "blue")
lines(d$iter,d$test_rmse,col = "red")

min(d$test_rmse)
bestmodel <- d[d$test_rmse == min(d$test_rmse)]



#now converting the whole data to into sparse matrix
#since more the data more will be out come
#although it wont be same in all cases
dataset.27m <- sparse.model.matrix(infect_prob~.-infect_prob,data = dataset.27)

#creating the model
model.27.final = xgboost(nrounds = 445,eta = 0.01,data = dataset.27m,label = dataset.27$infect_prob)

bestmodel <- d[d$test_rmse == min(d$test_rmse)]

#predictions
prediction.27 <- predict(model.27.final,data.27m)

head(prediction.27)
length(prediction.27)

#combining them with ID
output.27 <- cbind(ID.27,prediction.27)
head(output.27)
output.27 <- as.data.frame(output.27)

#writing as csv files
write.csv(output.27,"27_mar_output.csv")




























