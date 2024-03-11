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
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## Split into training/test set (80/20)
set.seed(145) # for reproducibility
index <- sample(1:nrow(df))
n_tr <- round(0.8 * nrow(df))
df_tr <- df[index[1:n_tr],]
df_te <- df[-index[1:n_tr],]

## fit and prune the tree (see previous case)
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.0001))
bank_ct <- prune(bank_ct, cp=0.022) # ~3 splits
rpart.plot(bank_ct)

## Compute the confusion matrix and associated metrics
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct, type="class"),
                positive="yes")
## Compute the F1
2 * 0.33415 * 0.97474 / (0.33415 + 0.97474)

## Discussion
## Is it a large or a small accuracy? 
## Compare to a naive model? (in data proportion)
prop.table(table(df_tr$deposit))
## Check sensitivity and specificity: what is the problematic class?

## Create the ROC curve 
df_pred <- data.frame(deposit=df_tr$deposit,
                      prob_yes = bank_prob[,2])
ROC_ct <- roc(deposit ~ prob_yes, data=df_pred)
ROC_ct
plot(ROC_ct, print.thres=0.5)

## Discussion
## Where is the best threshold that we could choose (i.e., best combination specificity / sensitivity)?

## #################################
## Example with regression task
data(house) 
df <- house[,-c(4,5)] 

set.seed(1459) 
index <- sample(1:nrow(df))
n_tr <- round(0.8 * nrow(df))
df_tr <- df[index[1:n_tr],]
df_te <- df[-index[1:n_tr],]

house_rt <- rpart(unit.price~., data=df_tr)
rpart.plot(house_rt)

## Predictions
pred_tr <- predict(house_rt)
pred_te <- predict(house_rt, newdata=df_te)

## Compute the apparent and test MAE
mae(actual=df_tr$unit.price, predicted=pred_tr) # same as mean(abs(df_tr$unit.price-pred_tr))
mae(actual=df_te$unit.price, predicted=pred_te) # same as mean(abs(df_te$unit.price-pred_te))

rmse(actual=df_tr$unit.price, predicted=pred_tr) # same as sqrt(mean((df_tr$unit.price-pred_tr)^2))
rmse(actual=df_te$unit.price, predicted=pred_te) # same as sqrt(mean((df_te$unit.price-pred_te)^2))

R2(obs=df_tr$unit.price, pred=pred_tr) # same as cor(df_tr$unit.price, pred_tr)^2
R2(obs=df_te$unit.price, pred=pred_te) # same as cor(df_te$unit.price, pred_te)^2

plot(x=df_tr$unit.price, y=pred_tr, pch=20, xlab="Actual", ylab="Predicted",
     main="Training set"); abline(0,1)
plot(x=df_te$unit.price, y=pred_te, pch=20, xlab="Actual", ylab="Predicted",
     main="Test set"); abline(0,1)

## Discussion
## We see that the RMSE test < RMSE training, whereas MAE test > MAE training. Why?
