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
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other" # simplify the "job" variable

## split into training and test set (80/20)
set.seed(900)
index_tr <- createDataPartition(y=df$deposit, p=0.8) # see index_tr$Resample1 -> it is the indices of the rows of df that are in the training set
df_tr <- df[index_tr$Resample1,] # training set (select the rows of df that are in index_tr$Resample1)
df_te <- df[-index_tr$Resample1,] # test set (select the rows of df that are NOT in index_tr$Resample1)

## fit to the training set a large tree (use a small cp for this)
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
rpart.plot(bank_ct)

## Compare the apparent and the test metrics
## For this, use confusionMatrix function in package caret
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct, type="class"),
                       positive="yes") # apparent metrics
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct, newdata=df_te, type="class"),
                       positive="yes") # test metrics 

## Discussion
## What is a sign of overfitting? Yes, apparent accuracy ~0.94 > test accuracy ~0.88

## To try to solve the overfitting, for CART, we can use pruning.
## Pruning using the 1-SE rule
set.seed(123) # set seed for reproducibility
plotcp(bank_ct, upper = "split") # The smallest tree below the 1-SE threshold has 3 splits
printcp(bank_ct) # 4 splits corresponds to a cp of 0.0240
bank_ct_prun <- prune(bank_ct, cp=0.024) # Prune to 3 splits
rpart.plot(bank_ct_prun)

## Now, let's see if the pruned tree still suffers from overfitting 
## compare the apparent and the test metrics
caret::confusionMatrix(reference=df_tr$deposit, data=predict(bank_ct_prun, type="class"),
                       positive="yes")
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")
## => Now the apparent accuracy ~0.902 = test accuracy ~0.896
## => overfitting was solved thanks to pruning

## The following lines illustrate a CV step by step
## Evaluation using cross validation 10-CV
set.seed(675) # set seed for reproducibility
index_cv <- createFolds(y=df_tr$deposit, k = 10, list = TRUE) # creates 10 lists of indices (those that will be in the validation set at each CV-step)
acc_cv <- numeric(length = length(index_cv)) # an empty vector of length 10 to store the accuracy
for (k in 1:length(index_cv)){ # loop on each list, i.e., from 1 to 10
  tmp_tr <- df_tr[-index_cv[[k]],] # current training set
  tmp_val <- df_tr[index_cv[[k]],] # current validation set
  tmp <- rpart(deposit ~ ., data=tmp_tr) # fit the tree on the current training set
  acc_cv[k] <- accuracy(actual=tmp_val$deposit, pred=predict(tmp, newdata=tmp_val, type="class")) # compute the accuracy on the current validation set
}
acc_cv # the 10 validation accuracy 
mean(acc_cv); sd(acc_cv) # mean and standard deviation

## Now we illustrate how we can use CV to tune a hyperparameter
## Below, we will tune the "cp" parameter used in rpart
## Tuning this hyperparameter cp is equivalent to looking for the appropriate 
## lentgh of the tree, i.e., like pruning the tree.
set.seed(111) # set seed for reproducibility
cp_list <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001) # list of 11 cp to compare
index_cv <- createFolds(y=df_tr$deposit, k = 10, list = TRUE) # create our CV indices

acc_cv <- matrix(nrow = length(index_cv), ncol=length(cp_list)) # a matrix of dimension 11x10 to store all the accuracy that will be computed
for (k1 in 1:length(index_cv)){ # loop on the CV (i.e., from 1 to 10)
  tmp_tr <- df_tr[-index_cv[[k1]],] # current training set
  tmp_val <- df_tr[index_cv[[k1]],] # current validation set
  for (k2 in 1:length(cp_list)){ # loop on the list of cp (i.e., from 1 to 11)
    tmp <- rpart(deposit ~ ., data=tmp_tr, control=list(cp=cp_list[k2])) # fit a tree to the current training set with current cp
    
    acc_cv[k1,k2] <- accuracy(actual=tmp_val$deposit, pred=predict(tmp, newdata=tmp_val, type="class")) # compute and store the accuracy of the current model on the current validation set
  }
}
acc_cv # all accuracy hence obtained
data.frame(cp=cp_list, mean_acc=apply(acc_cv, 2, mean)) # compute the mean accuracy (i.e., over the 10-CV steps)

## What is the best cp according to the study? The one with the largest accuracy => 0.005 for accuracy 0.897 (could vary in your case)

## This procedure, fortunately, can be automatic using caret::train
set.seed(878)
train(deposit ~ ., data=df_tr, # use the training set
      method = "rpart",  # use tree
      trControl = trainControl(method = "cv", number = 10), # use 10-CV
      tuneGrid = data.frame(cp=cp_list)) # use the cp_list defined above
## I obtained cp=0.01 for acc = 0.899

## How stable is the procedure? 
## I you rerun the procedure (without including set.seed) you will find (maybe) a different result (e.g., cp=0.005)

## To stabilize this evaluation, we can repeat it on several run => repeated CV
set.seed(009)
train(deposit ~ ., data=df_tr, 
      method = "rpart",  
      trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5), # repeat 10-CV 5 times
      tuneGrid = data.frame(cp=cp_list))

## ###################################
## ###################################
## Case with regression and bootstrap

data(house) 
df <- house[,-c(4,5)] 

set.seed(890)
index_tr <- createDataPartition(y=df$unit.price, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]
## What is the advantage of using bootstrap instead of CV in this case? df_tr is small so bootstrap is probably more appropriate than CV 

## Now we are going to analyse the data using 
## a regression with L1-penalty 
## To select the penalty value (hyperparameter lambda), we tune it using bootstrap

lambda_values <- 10^seq(-4, 1, by = 0.1) # list of trial values of lambda

set.seed(978)
train(unit.price~., data = df_tr, 
      method = "glmnet", # glmnet is the function for elastic net, including L1-regression
      trControl = trainControl(method = "boot632", number = 100), # use 100-bootstrap with 632-rule (must!) 
      tuneGrid = expand.grid(alpha = 1, # set alpha=1 in elastic net allows to have L1-regression (not L2)
                             lambda = lambda_values)) # use the list of trial lambda's
## I obtained opt lambda = 0.0316
## Note: the number of features is too small to justify an L1. This was only for illustration.

## #######################################
## #######################################
## #######################################
## Balancing classes
## Now we illustrate how to balance the data to solve the unblanaced data issue

## We use the deposit data and classification tree
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## First usual steps: create training/test sets, fit the tree, prune it.
## This is just to illustrate the problem of unbalanced data
set.seed(367)
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
plotcp(bank_ct, upper = "split") # on this run, I found the best tree to be with 7 splits
printcp(bank_ct) # prune at cp=0.0096
bank_ct_prun <- prune(bank_ct, cp=0.01) # 7 splits
rpart.plot(bank_ct_prun)

## Test metrics of the pruned tree 
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## What sign of "imbalance" can we see? e.g., sensitivity is small, specificity is large; balanced accuracy << accuracy
## See also: table(df_tr$deposit)

## Solution using upsampling 
set.seed(123) 
df_tr_up <- upSample(subset(df_tr, select=-deposit), y=df_tr$deposit, yname="deposit") # create an upsampled data set
table(df_tr_up$deposit) # now number of "no" == number of "yes"

## The training (with pruning) is exactly the same as with the original training set
bank_ct <- rpart(deposit ~ ., data=df_tr_up, control = list(cp=0.0000001))
plotcp(bank_ct, upper = "split") # on this run, I found the best tree to be with 93 splits
printcp(bank_ct) # prune at cp=0.00084
bank_ct_prun <- prune(bank_ct, cp=0.00084) # 5 splits
rpart.plot(bank_ct_prun)

## Now let's look at the test metrics
## !!!!!!!!!!!!!!!!! test set is NOT balanced
caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")

## We see accuracy ~0.86, balanced accuracy ~0.79, sensitivity ~0.71, specificity ~0.88
## The imbalance issue was reduced

## Solution using down-sampling
set.seed(123) ## for reproducibility
df_tr_down <- downSample(subset(df_tr, select=-deposit), y=df_tr$deposit, yname="deposit")
table(df_tr_down$deposit)

bank_ct <- rpart(deposit ~ ., data=df_tr_down, control = list(cp=0.001))
rpart.plot(bank_ct)
plotcp(bank_ct, upper = "split") # target 4 splits
printcp(bank_ct) # cut at cp=0.0145
bank_ct_prun <- prune(bank_ct, cp=0.0145) 
rpart.plot(bank_ct_prun)

caret::confusionMatrix(reference=df_te$deposit, data=predict(bank_ct_prun, newdata=df_te, type="class"),
                       positive="yes")
## The solution looks well balanced but the accuracy is not as good as with upsampling; this is probably due to chance.

## #############################
## #############################
## Now we illustrate the tuning of the probability threshold (usually 50%, what if change it?)

## First we fit the model (!here it is all applied on the original training set)
set.seed(367)
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]
bank_ct <- rpart(deposit ~ ., data=df_tr, control = list(cp=0.00001))
bank_ct_prun <- prune(bank_ct, cp=0.01) # 7 splits
rpart.plot(bank_ct_prun)

## Now we need the predicted probabilities (on the training set)
bank_prob <- predict(bank_ct_prun, type="prob") # see bank_prob

## a data frame for the ROC curve is built
df_pred <- data.frame(deposit=df_tr$deposit,
                      prob_yes = bank_prob[,2]) # see df_pred
## Produce the ROC object and its curve
ROC_ct <- roc(deposit ~ prob_yes, data=df_pred)
plot(ROC_ct, print.thres="best") 
## By default the best threshold maximizes sensitivity+specificity (Youden's J stat)
## Here opt. threshold = 0.116

## Let's use this threshold to make the predictions on the test set
## First produce the predicted probabilities
bank_prob_te <- predict(bank_ct_prun, newdata=df_te, type="prob")
## Then predict "yes" if prob > 0.112, "no" if prob <= 0.116
opt_thresh <- 0.116
bank_pred_te <- factor(ifelse(bank_prob_te[,2] > opt_thresh, "yes", "no"))
caret::confusionMatrix(reference=df_te$deposit, data=bank_pred_te,
                       positive="yes")

## The results happen to be less good (sens ~0.48, spec~0.95) than with resampling
