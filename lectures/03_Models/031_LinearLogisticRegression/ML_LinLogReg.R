## Libraries
library(liver)
library(dplyr)
library(glmnet)
library(summarytools)

## ###########################
## ###########################
## ###########################
## Logistic regression

## Load the data
data(bank)
df <- bank # create a workable copy

## logistic regression on deposit
bank_logreg <- glm(deposit ~ ., data=df, family="binomial")
summary(bank_logreg)

## !!! Important: positive class (success) is the second factor level
## i.e., see levels(bank$deposit) [cf ?family and binomial]

## Some improvement are possible, let's see
## 1. Simplify jobs
table(df$job) %>% prop.table() %>% sort(decreasing = TRUE) %>% cumsum()
## 6 modalities accounts for >75% of the instances -> create a category "other"
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"
levels(df$job)

bank_logreg <- glm(deposit ~ ., data=df, family="binomial")
summary(bank_logreg)

## Example: Linear predictor & predicted probabilty & predicted class
df[1,]
predict(bank_logreg, newdata = df[1,], type = "link") # see predict.glm
exp(-1.25823)/(1+exp(-1.25823)) # sigmoid function
predict(bank_logreg, newdata = df[1,], type = "response") # directly
ifelse(predict(bank_logreg, newdata = df[1,], type = "response") > 0.5, "yes", "no") # only by hand

## also...
predict(bank_logreg, newdata = df[1,], type = "terms") # contributions to the linear predictor of each variables

## ############################
## Discussion: 
## How certain is that case? 
## If age increases, is it more or less likely to reach deposit==yes?
## How about switching from education=primary to education==secondary?

## ###########################
## Variable selection

## using AIC
step(bank_logreg) 
bank_logreg_sel <- step(bank_logreg, trace = FALSE) # silent run + store the model

## ############################
## Discussion: 
## What variables where removed from the model?
## What type of scheme is it? (backward, forward, both)

## using LASSO 

## first convert the predictor to numerical variable (dummy variables)
x <- model.matrix(deposit~., data=df)[,-1] # remove deposit and the intercept
## set alpha=1 for full LASSO
## use cv.glmnet to select lambda by cross validation
fit <- cv.glmnet(x=x, y=df$deposit, family="binomial", alpha=1)
fit
## see the coefficients for the penalty lambda.min
coef(fit, s="lambda.min") 
## for simplicity, refit the model
bank_logreg_L1 <- glmnet(x=x, y=df$deposit, family = "binomial", alpha=1, lambda=fit$lambda.min)
## Example of prediction for the first case
predict(bank_logreg_L1, newx = x[1,], type = "response")

## ############################
## Discussion:
## Compare the selection with AIC and the one with LASSO. Are they equivalent?

## ###########################
## ###########################
## ###########################
## Linear regression

data(house) ## in package liver
?house
df <- house[,-c(4,5)] # latitude and longitude not meaningful for this illustration

## quick EDA
dfSummary(df) %>% view()

## model
house_lm <- lm(unit.price~., data=df)
summary(house_lm)

## prediction
predict(house_lm, newdata = df[1,]) ## see predict.lm

## build prediction intervals 
s_est <- summary(house_lm)$sigma # estimate of s
## quick and dirty P.I. = price prediction +/- 1.96 * s
predict(house_lm, newdata = df[1,]) + c(-1,1) * 1.96 * s_est
## Correct one (includes also parameter estimation uncertainty)
predict(house_lm, newdata = df[1,], interval = "prediction")

