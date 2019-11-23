library(corrplot)
library(caret)
library(psych)
library(readr)
library(randomForest)
library(caret)
library(tree)
library(rpart.plot)
library(dummies)
library(dplyr)

rm(list = ls())

customer<-read.csv("C:/Users/Raja Amlan/Desktop/Data Science Projects Kaggle/Telco Customer Churn/WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE)
str(customer)
View(customer)

##Convert Senior Citizen to Factor
customer$SeniorCitizen <- as.factor(customer$SeniorCitizen)

## Check Missing values in the data
sapply(customer, function(x) sum(is.na(x)))


## Now we can go with two approaches either dropping the 11 cases that have NA's, or we can compute the values of total charges using other available data
## Here we  are dropping the variables 

set.seed(131313)

## Dropped 11 values from total charges
customer <- customer[complete.cases(customer),]

## Exploratory Analysis ###

summary(customer$Churn)

## Percentage of Churn Customers is close to 26%.



prop.table(summary(customer$Churn))


## This estalishes high correlation between the two variables monthly and total charges
## Although intuitive this correlation matrix helps us establish that

library("Hmisc")
res <- rcorr(as.matrix(customer[,19:20]))
res

## Customer ID is one variable we dont require so dropping that column
## Also removing highly correlated variables

customer_clean <- customer %>%
  select(-customerID, -TotalCharges) %>%
  rename(Gender = gender, Tenure = tenure)


### Further cleaning involves, converting 'no internet service' to no and 'no phone service' to no

factorrenames <- names(customer_clean[9:14])

data <- customer_clean %>%
  mutate_at(.vars=factorrenames,
            .funs=~recode_factor(., `No internet service`="No")) %>%
  mutate_at(.vars="MultipleLines",
            .funs=~recode_factor(., `No phone service`="No"))


str(data)


set.seed(1)


### Shuffle the order of data in order to prevent bias that is pattern specific
row_index <- sample(nrow(data))

 final_data<- data[row_index,]
 
 ## Now splitting the data in test and train data
 
 n <- nrow(final_data)
 train <- sample(n, trunc(0.70*n))
 training <- final_data[train, ]
 testing <- final_data[-train, ]
 
 ### Model Building : Logistic Regression
 
 ## Hyper Parameter Tuning for Logistic Regression Model
 
 control <- trainControl(
   method = "cv",
   number = 10,
   summaryFunction = twoClassSummary,
   classProbs = TRUE,
   verboseIter = FALSE
 )
 
log_model <- train(Churn ~ ., data = training, family = binomial, method="glm", trControl = control)
log_model ## ROC value shows 
summary(log_model)

## ROC value obtained is pretty good

 #### Predict Accuracy of the model using a confusion matrix

log_model_pred <- predict(log_model, newdata = testing)

logistic_cm <- confusionMatrix(log_model_pred, testing[["Churn"]])
glmaccuracy <- logistic_cm$overall[c(1,3,4)]
logistic_cm



### Now using regularisation to the model, ElasticNet Parameter

glmnet_model <- train(Churn ~ ., data = training,
                      metric = "ROC",
                      method = "glmnet",
                      trControl = control,
                      preProcess = c("center","scale")
)


plot(glmnet_model)
glmnet_model$bestTune$alpha


glmnet_pred <- predict(glmnet_model, newdata = testing)

glmnetcm <- confusionMatrix(glmnet_pred, testing[["Churn"]])
glmnetaccuracy <- glmnetcm$overall[c(1,3,4)]
glmnetcm





