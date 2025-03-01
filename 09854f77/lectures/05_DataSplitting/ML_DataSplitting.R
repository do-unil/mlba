## Libraries
library(liver)
library(rpart)
library(rpart.plot)
library(glmnet)
library(caret)
library(pROC)
library(ModelMetrics)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## split into training and test set (80/20)
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]

## fit a very large tree to the training set
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
rpart.plot(bank_ct)

## compare the apparent and the test metrics
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct, type="class"),
                       positive="yes")
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct, newdata=df_te, type="class"),
                       positive="yes")

## ###############################
## Discussion
## What is a sign of overfitting?
## How can it be solved?

## Pruning using the 1-SE rule
set.seed(123) ## for reproducibility
plotcp(bank_ct, upper = "split") 
printcp(bank_ct) 
bank_ct_prun <- prune(bank_ct, cp=0.020) # 3 splits
rpart.plot(bank_ct_prun)

## compare the apparent and the test metrics
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct_prun, type="class"),
                       positive="yes")
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## Evaluation using cross validation 10-CV
index_cv <- createFolds(y=df_tr$deposit, k = 10, list = TRUE) # check size of each fold nrow(df_tr)/5
acc_cv <- numeric(length = length(index_cv))
for (k in 1:length(index_cv)){
  tmp <- rpart(deposit ~ ., data=df_tr[-index_cv[[k]],])
  acc_cv[k] <- accuracy(actual=df_tr$deposit[index_cv[[k]]], pred=predict(tmp, newdata=df_tr[index_cv[[k]],], type="class"))
}
acc_cv
mean(acc_cv); sd(acc_cv)

## ###############################
## Discussion
## What is the validation set in the code above?

## Tuning the hyperparameter cp 
cp_list <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.005, 0.0001, 0.00005, 0.00001)
index_cv <- createFolds(y=df_tr$deposit, k = 10, list = TRUE) # check size of each fold nrow(df_tr)/5

acc_cv <- matrix(nrow = length(index_cv), ncol=length(cp_list))
for (k1 in 1:length(index_cv)){
  for (k2 in 1:length(cp_list)){
    tmp <- rpart(deposit ~ ., data=df_tr[-index_cv[[k1]],],
                 control=list(cp=cp_list[k2]))
    acc_cv[k1,k2] <- accuracy(actual=df_tr$deposit[index_cv[[k1]]], pred=predict(tmp, newdata=df_tr[index_cv[[k1]],], type="class"))
  }
}
acc_cv
data.frame(cp=cp_list, mean_acc=apply(acc_cv, 2, mean))

## ###############################
## Discussion
## What is the best cp according to the study?

## automatic using caret::train
train(deposit ~ ., data=df_tr, 
      method = "rpart",  
      trControl = trainControl(method = "cv", number = 10), 
      tuneGrid = data.frame(cp=cp_list))

## Discussion
## How stable is the procedure? 

## use repeated CV to stabilize it
train(deposit ~ ., data=df_tr, 
      method = "rpart",  
      trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5), 
      tuneGrid = data.frame(cp=cp_list))

## ###################################
## Case with regression and bootstrap

data(house) 
df <- house[,-c(4,5)] 

index_tr <- createDataPartition(y=df$unit.price, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]

dim(df_tr)

## Discussion
## What is the advantage of using bootstrap instead of CV in this case?

## Tune a linear regression with L1 penalty
lambda_values <- 10^seq(-4, 1, by = 0.1)

train(unit.price~., data = df_tr,
      method = "glmnet",
      trControl = trainControl(method = "boot632", number = 100), 
      tuneGrid = expand.grid(alpha = 1, 
                             lambda = lambda_values))

## Under the hood:
R <- 100
index_boot <- createResample(y=df_tr$unit.price, times = R)
x_tr <- model.matrix(unit.price~., data=df_tr)[,-1]

R2_mat <- matrix(nrow=length(lambda_values), ncol=R) 
for (i in 1:R){
  for (j in 1:length(lambda_values)){
    y_tr2 <- df_tr$unit.price[index_boot[[i]]]
    x_tr2 <- x_tr[index_boot[[i]],]
    y_val <- df_tr$unit.price[-index_boot[[i]]]
    x_val <- x_tr[-index_boot[[i]],]
    mod <- glmnet(y=y_tr2, 
                  x=x_tr2, 
                  family="gaussian", 
                  lambda=lambda_values[j], 
                  alpha=1)
    pred_tr <- predict(mod, newx = x_tr2)
    pred_val <- predict(mod, newx = x_val)
    R2_app <- postResample(pred=pred_tr, obs=y_tr2)[2]
    R2_oob <- postResample(pred=pred_val, obs=y_val)[2]
    R2_boot <- 0.632 * R2_oob + 0.368 * R2_app
    R2_mat[j,i] <- R2_boot
  }
}
data.frame(lambda=lambda_values, R2=apply(R2_mat, 1, mean))

## Discussion
## What happened to the case R2==NA?

## #######################################
## #######################################
## #######################################
## #######################################
## Balancing classes

## re-set the deposit data and tree
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
bank_ct_prun <- prune(bank_ct, cp=0.032) # 3 splits

## Using the pruned tree
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## Discussion
## What sign of "imbalance" can we see?
## See also
table(df_tr$deposit)

## Solution using upsampling
set.seed(123) ## for reproducibility
df_tr_up <- upSample(subset(df_tr, select=-deposit), y=df_tr$deposit, yname="deposit")
table(df_tr_up$deposit)

bank_ct <- rpart(deposit ~ ., data=df_tr_up, control = list(cp=0.001))
rpart.plot(bank_ct)

plotcp(bank_ct, upper = "split") 
printcp(bank_ct) 
bank_ct_prun <- prune(bank_ct, cp=0.00126) 
rpart.plot(bank_ct_prun)

caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## Discussion
## Must the test set be balanced? Why?

## Solution using down-sampling (provide an equivalent solution)
set.seed(123) ## for reproducibility
df_tr_down <- downSample(subset(df_tr, select=-deposit), y=df_tr$deposit, yname="deposit")
table(df_tr_down$deposit)

bank_ct <- rpart(deposit ~ ., data=df_tr_down, control = list(cp=0.001))
rpart.plot(bank_ct)

plotcp(bank_ct, upper = "split") 
printcp(bank_ct) 
bank_ct_prun <- prune(bank_ct, cp=0.012) 
rpart.plot(bank_ct_prun)

caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## #############################
## Solution by tuning the threshold (use ROC curve)

## Applied on unbalanced data !!
set.seed(145)
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
rpart.plot(bank_ct)

plotcp(bank_ct, upper = "split") 
printcp(bank_ct) 
bank_ct_prun <- prune(bank_ct, cp=0.02) 
rpart.plot(bank_ct_prun)

bank_prob <- predict(bank_ct_prun, type="prob")

df_pred <- data.frame(deposit=df_tr$deposit,
                      prob_yes = bank_prob[,2])
ROC_ct <- roc(deposit ~ prob_yes, data=df_pred)
plot(ROC_ct, print.thres="best") ## Print the best threshold (Yule's index)

which.max(ROC_ct$sensitivities + ROC_ct$specificities)
(opt_thresh <- ROC_ct$thresholds[2])

bank_prob_te <- predict(bank_ct_prun, newdata=df_te, type="prob")
bank_pred_te <- factor(ifelse(bank_prob_te[,2] > opt_thresh, "yes", "no"))
caret::confusionMatrix(reference=df_te$deposit, data=bank_pred_te,
                       positive="yes")

## Discussion
## Do we get different results from the down/up sample? 