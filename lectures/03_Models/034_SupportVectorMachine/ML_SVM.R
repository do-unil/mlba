## Libraries
library(liver)
library(e1071)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other" # reduce the number of levels of variable job

## fit an SVM with radial kernel
bank_svm <- svm(deposit ~ ., data=df, 
                kernel = "radial", type = "C-classification",
                gamma = 0.01,
                cost = 1)
summary(bank_svm)

## Example when changing the cost parameter
bank_svm <- svm(deposit ~ ., data=df, 
                kernel = "radial", type = "C-classification",
                gamma = 0.01,
                cost = 1000)
summary(bank_svm)

## Predictions
predict(bank_svm, newdata=df[1:10,]) # predicted classes by default
predict(bank_svm, newdata=df[1:10,], decision.values=TRUE) # decision value ratio

## if we want the probabilities we need to ask it first in the model training
bank_svm <- svm(deposit ~ ., data=df, 
                kernel = "radial", type = "C-classification",
                cost = 1000, 
                probability=TRUE)
predict(bank_svm, newdata=df[1:10,], decision.values=TRUE, probability = TRUE) 

## Here is the effect of changing gamma (radial kernel parameter) for a fixed cost
bank_svm <- svm(deposit ~ ., data=df, 
                kernel = "radial", type = "C-classification",
                gamma = 0.01,
                cost = 1)
summary(bank_svm)
bank_svm <- svm(deposit ~ ., data=df, 
                kernel = "radial", type = "C-classification",
                gamma = 10,
                cost = 1)
summary(bank_svm)

