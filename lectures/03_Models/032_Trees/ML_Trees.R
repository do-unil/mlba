## Libraries
library(liver)
library(rpart)
library(rpart.plot)
library(adabag)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

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
set.seed(123) ## for reproducibility
plotcp(bank_ct, upper = "split") # -> select leftmost mean under the line 
printcp(bank_ct) # look for the corresponding cp
bank_ct_prun <- prune(bank_ct, cp=0.022) # 3 splits
rpart.plot(bank_ct_prun)

## Can be done automatically (!!! very long)
##bank_ct_prun2 <- autoprune(deposit ~ ., data=df)
##rpart.plot(bank_ct_prun2) 

## ###############################
## Discussion
## What are the advantage / drawbacks of the two approaches? (precision, time, energy consumption)

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
