#------------------------------Prediction Using Decision Tree Algorithm---------------------------------------#

#------------------------------Author-Tania Das---------------------------------------#

#------------------------------Task---------------------------------------#

# Create the Decision tree classifier  and visualize it graphically


#------------------------------Preparing the environment for Decision Trees---------------------------------------#

list.of.packages <- c("caret","e1071","ggplot2","rpart", "rpart.plot","pROC","randomForest","caTools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(caTools)
library(e1071)
#-------------------------Setting the working directory and Reading the dataset--------------------------------------------#

Path<-"C:/Users/TANIA DAS/Downloads"
setwd(Path)
getwd()

iris=read.csv('Iris.csv')
head(iris)# Checking the first five rows of the dataset

#-----------------------Basic Exploration of the Data Set----------------------------------------------------------#

dim(iris)# Checking the dimension of the data

str(iris)# Checking the structure of the data

iris=iris[,-1] # dropping the ID column since its unique and hence no aggregation can be done on it

dim(iris) # Checking the final dimension of the data after dropping the ID column

summary(iris)# Checking the summary of the data

colSums(is.na(iris)) # Checking for missing values in the dataset

iris$Species<-as.factor(iris$Species)# Converting the character variable into factor variable

str(iris)

#------------------------------Splitting the dataset into train and test data-----------------------#

set.seed(1000)
spl<-sample.split(iris$Species,0.7)
train<-subset(iris,spl==TRUE)
dim(train)

test<-subset(iris,spl==FALSE)
dim(test)

#-------------------------------------------Building the CART model----------------------------------------------#

CART1<-rpart(Species~.,data=train, method = "class")
prp(CART1)
CART1

#Conclusion-Hence the Decision Tree Classifier is created ; you can feed any data to this classifier  and it would be able to predict the right class accordingly.

#-------------------------Checking the accuracy of the model in the test data------------------------------#
predictCART1<-predict(CART1, newdata=test, type = "class")

confusionMatrix(predictCART1,test$Species)

