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

## logistic regression with outcome = deposit
bank_logreg <- glm(deposit ~ ., data=df, family="binomial")
summary(bank_logreg)

## !!! Important: the positive class is the second factor level
## i.e., see levels(bank$deposit) [cf ?family and binomial]

## Because jobs has lots of levels, we simplify it
table(df$job) %>% prop.table() %>% sort(decreasing = TRUE) %>% cumsum()
## 6 modalities accounts for >75% of the instances -> we create a category "other"
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"
levels(df$job)

## Fit again the logistic regression with the new lob variable
bank_logreg <- glm(deposit ~ ., data=df, family="binomial")
summary(bank_logreg)

## Example: Linear predictor & predicted probability & predicted class
## suppose we want to predict this instance, i.e., the first row of df
df[1,] 
## we can make it by hand. First, compute the linear predictor:
predict(bank_logreg, newdata = df[1,], type = "link") # see ?predict.glm
## Then apply the sigmoid function:
exp(-1.25823)/(1+exp(-1.25823)) 
## Alternatively, we can use predict with argument type="response" 
predict(bank_logreg, newdata = df[1,], type = "response") 
## In any case, the final prediction ("yes" or "no") must be done by thresholding by hand:
ifelse(predict(bank_logreg, newdata = df[1,], type = "response") > 0.5, "yes", "no") # only by hand

## ###########################
## Variable selection

## using AIC
step(bank_logreg) # see the result in direct
## Equivalently, we can run it silently (trace=FALSE) and store the final model in a variable
bank_logreg_sel <- step(bank_logreg, trace = FALSE) 

## Discussion: 
## - Check which variables were removed from the model.
## - Recover what selection was applied (backward, forwar or both)

## using LASSO
## Technically more complex in R
## First, we convert the predictor to numerical variables
x <- model.matrix(deposit~., data=df)[,-1] # remove deposit and the intercept
head(x)
## Discussion:
## What type of variables are poutcomeother, poutcomesuccess?  (dummy variables)

## For illustration,
## set alpha=1 for full LASSO (see course, alpha=0 => ridge)
## set lambda=0.01
bank_logreg_L1 <- glmnet(x=x, y=df$deposit, family = "binomial", alpha=1, lambda=0.01)
coef(bank_logreg_L1)
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

