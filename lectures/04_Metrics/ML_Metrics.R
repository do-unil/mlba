## Libraries
library(liver)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(ModelMetrics)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other" # simplify the "job" variable

## Split the data in training/test set (80/20)
set.seed(145) # set the seed for reproducibility
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,] # training set (cf splitting strategy for details) 
df_te <- df[-index_tr$Resample1,] # test set (cf splitting strategy for details) 

## fit the tree and prune it (see Models for details)
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.0001))
bank_ct_prune <- prune(bank_ct, cp=0.022) # prune to 3 splits
rpart.plot(bank_ct_prune)

## Compute the confusion matrix and associated metrics
## Below, the apparent metrics, i.e., on the training set (see splitting strategy for details)
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct_prune, type="class"),
                positive="yes")
## I obtained (due to randomness in different OS, you may find different results)
# Accuracy : 0.9019          
# Kappa : 0.3848 
# Sensitivity : 0.32374         
# Specificity : 0.97719         
# Pos Pred Value : 0.64904         
# Neg Pred Value : 0.91728
# Balanced Accuracy : 0.65046         


## Compute the F1 = 2 * sens * PPV /(sens + PPV)
2 * 0.32374 * 0.64904 / (0.32374 + 0.64904) # 0.432

## Discussion 
## How come the accuracy and the balanced accuracy are so different?
prop.table(table(df_tr$deposit))
## => classes are imbalanced, the model favors prediction of "no", 
## => specificity >> sensitivity => balanced accuracy << accuracy
## Rmk: predict only "no" gives an accuracy of 3200/3617 = 0.88

## Illustration of the ROC curve
## First we need the predicted probabilities for each instances (here from the training set)
bank_prob <- predict(bank_ct_prune, type="prob")
## Then we must build a data frame of the observed class vs the predicted probability of "yes"
df_pred <- data.frame(deposit=df_tr$deposit,
                      prob_yes = bank_prob[,2]) ## see df_pred
## Call function roc and plot it
ROC_ct <- roc(deposit ~ prob_yes, data=df_pred)
ROC_ct ## we see that AUC=0.71
plot(ROC_ct)

## The ROC curve is built by varying the probability threshold 
## Usually this threshold is 50% (the one that gives sens=0.324, spec=0.977)
## This point can be identified on the curve
plot.roc(ROC_ct,print.thres = 0.5)

## In the next module, the optimization of this threshold is presented as 
## a way to solve the imbalance data issue.

## #################################
## #################################
## #################################
## We now show the metrics for regression task
data(house) 
df <- house[,-c(4,5)] 

## Same as before, we split the data in training and test set (only using training set below)
set.seed(1459) 
index_tr <- createDataPartition(y=df$unit.price, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]

## Fit the model. No pruning this time for illustration
house_rt <- rpart(unit.price~., data=df_tr)
rpart.plot(house_rt)

## We will need the price predictions of the model
pred_rt <- predict(house_rt)

## Compute the apparent MAE, RMSE, and R2
mae(actual=df_tr$unit.price, predicted=pred_rt) # same as mean(abs(df_tr$unit.price-pred_rt))
rmse(actual=df_tr$unit.price, predicted=pred_rt) # same as sqrt(mean((df_tr$unit.price-pred_rt)^2))
R2(obs=df_tr$unit.price, pred=pred_rt) # same as cor(df_tr$unit.price, pred_rt)^2

## Let's compare to a linear regression
house_lm <- lm(unit.price~., data=df_tr)
summary(house_lm)
pred_lm <- predict(house_lm)
mae(actual=df_tr$unit.price, predicted=pred_lm) 
rmse(actual=df_tr$unit.price, predicted=pred_lm) 
R2(obs=df_tr$unit.price, pred=pred_lm) 
## Overall, the tree looks better than the linear regression for these data on every metrics

## Scatterplot prediction vs observation
plot(x=df_tr$unit.price, y=pred_rt, pch=20, xlab="Actual", ylab="Predicted",
     main="Regression tree"); abline(0,1)
plot(x=df_tr$unit.price, y=pred_lm, pch=20, xlab="Actual", ylab="Predicted",
     main="Linear regression"); abline(0,1)

## Discussion
## The "step shape" of the regression tree is by construction (prediction can be only one of the node values)
## Despite this weird shape, the tree provides a better fit (see MAE, R2, RMSE)
## We see for both models have difficulties to predict one case of ~120.
