## Libraries
library(liver)
library(rpart)
library(rpart.plot)
library(adabag)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

set.seed(123) ## this is useful for reproducibility because there is randomness in the following

## fit a tree (use greedy algorithm) and plot it
bank_ct <- rpart(deposit ~ ., data=df)
rpart.plot(bank_ct)

## ##############################
## Discussion
## What is the most important feature?
## If duration increases, is it more or less likely to predict "yes"?

## Pruning using the 1-SE rule
## start with a large model
bank_ct <- rpart(deposit ~ ., data=df, control = list(cp=0.0001))
rpart.plot(bank_ct)

## Produce the uncertainty plot
plotcp(bank_ct, upper = "split") 
## => select leftmost mean under the line (here, 3 splits)
## Use the table below to identify the corresponding CP (should be 0.0211)
printcp(bank_ct) 
## Use the CP to prune the tree at 3 splits (CP=0.022) with the function below
bank_ct_prun <- prune(bank_ct, cp=0.022)
rpart.plot(bank_ct_prun)

## This can be done automatically using function auto.prune
## !!! very long
bank_ct_prun2 <- autoprune(deposit ~ ., data=df)
rpart.plot(bank_ct_prun2) 

## #################################
## #################################
## Example with regression
data(house) 
df <- house[,-c(4,5)] 

house_rt <- rpart(unit.price~., data=df)
rpart.plot(house_rt)

## ###############################
## Discussion
## Why does the scatterplot observations vs predictions look like this?
plot(predict(house_rt) ~ df$unit.price, pch=20)
abline(0,1)
