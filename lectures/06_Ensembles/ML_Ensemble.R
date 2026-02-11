## Libraries
library(liver)
library(caret)
library(dplyr)
library(ipred)
library(ranger)
library(xgboost)
library(Matrix)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## split into training and test set (80/20)
index_tr <- createDataPartition(y=df$deposit, p=0.8)
df_tr <- df[index_tr$Resample1,]
df_te <- df[-index_tr$Resample1,]

## Bagging a logistic regression
set.seed(134)
n_bag <- 201
index_bag <- createResample(y=df_tr$deposit, times=201)
bank_lg <- vector(mode = "list", length = n_bag)
for (i in 1:n_bag){
  bank_lg[[i]] <- glm(deposit~., data=df_tr[index_bag[[i]],], family = binomial())
}
bank_lg # contains a bag 201 logistic regressions 
## inference on the test set
bank_prob_bag <- sapply(bank_lg, predict, newdata=df_te, type="response") 
bank_prob_bag
bank_pred_bag <- ifelse(bank_prob_bag > 0.5, 1, 0)
bank_pred_bag
bank_pred_te <- ifelse(apply(bank_prob_bag, 1, sum)>=100.5, "yes", "no") %>% factor()
bank_pred_te
## inference on the training set
bank_prob_bag <- sapply(bank_lg, predict, newdata=df_tr, type="response") 
bank_pred_bag <- ifelse(bank_prob_bag > 0.5, 1, 0)
bank_pred_tr <- ifelse(apply(bank_prob_bag, 1, sum)>=100.5, "yes", "no") %>% factor()
## Compare apparent and test accuracy
confusionMatrix(data=bank_pred_tr, reference = df_tr$deposit, positive = "yes")
confusionMatrix(data=bank_pred_te, reference = df_te$deposit, positive = "yes")

## Note: 
## In this case, there is no overfitting despite that no measure
## against overfitting were taken (e.g., AIC-based feature selection)

## ##################################
## Random forests: use function ranger 
set.seed(897)
bank_rf <- ranger(deposit~., 
                  data=df_tr,
                  num.trees = 500,
                  mtry=5)
bank_rf
bank_pred_tr <- predict(bank_rf, data = df_tr, type="response")
bank_pred_te <- predict(bank_rf, data = df_te, type="response")

confusionMatrix(data=bank_pred_tr$predictions, reference = df_tr$deposit, positive = "yes")
confusionMatrix(data=bank_pred_te$predictions, reference = df_te$deposit, positive = "yes")

## Note: we see clearly overfitting as accuracy Tr >> accuracy Te

## Try to reduce overfitting by limiting the maximum depth of individual trees
set.seed(667)
bank_rf <- ranger(deposit~., 
                  data=df_tr,
                  num.trees = 500,
                  mtry = 5,
                  max.depth = 3)
bank_rf
bank_pred_tr <- predict(bank_rf, data = df_tr, type="response")
bank_pred_te <- predict(bank_rf, data = df_te, type="response")

confusionMatrix(data=bank_pred_tr$predictions, reference = df_tr$deposit, positive = "yes")
confusionMatrix(data=bank_pred_te$predictions, reference = df_te$deposit, positive = "yes")

## Note: the overfitting is reduced. max.depth happens to be one parameter
## that influences the most overfitting for random forests.

## ###############################
## ###############################
## ###############################
## Example of Boosting

## We use xgboost package
## First, we need to convert the data into DMatrix format
## For this we use xgb.DMatrix
## This requires a matrix form (one-hot encoding of df) 
## For this, we first use model.matrix
df_tr_mat <- model.matrix(deposit~ . -1, data=df_tr)
df_te_mat <- model.matrix(deposit~ . -1, data=df_te)
dtrain <- xgb.DMatrix(data = df_tr_mat, label = ifelse(df_tr$deposit=="yes", 1,0))
dtest <- xgb.DMatrix(data = df_te_mat, label = ifelse(df_te$deposit=="yes", 1,0))

## Set parameters for boosting 
## See ?xgboost for the long list
params <- list(
  booster="gbtree", # use tree as the booster (i.e., the base learner) 
  objective = "binary:logistic",  # binary classification
  eval_metric = "error"         # evaluation metric (here classification error rate)
)

## Run the boosting for 1000 rounds (boosting iterations)
set.seed(987)
boost_model <- xgb.train(data = dtrain, 
                       params = params, 
                       nrounds = 1000)

## Make the predictions and compare apparent and test metrics
tr_predictions <- predict(boost_model, dtrain)
te_predictions <- predict(boost_model, dtest)
boost_pred_tr <- factor(ifelse(tr_predictions > 0.5, "yes", "no"))
boost_pred_te <- factor(ifelse(te_predictions > 0.5, "yes", "no"))
caret::confusionMatrix(data=boost_pred_tr, reference = df_tr$deposit, positive = "yes")
caret::confusionMatrix(data=boost_pred_te, reference = df_te$deposit, positive = "yes")

## Overfitting... nrounds (T in the course) is much too large

## To try to solve it we want to find the optimal number of iterations
## We use 5-CV and early stopping rule: the algorithm stops if the 
## validation metric does not improve after k more iterations
## Below k=10
set.seed(510)
cv <- xgb.cv(data = dtrain, 
             nrounds = 1000,
             nfold = 5, 
             params=params,
             early_stopping_rounds = 10)
print(cv)
cv$best_iteration ## the best number of iterations...

## We fit the model with this best number of iterations and check if overfitting 
## is solved
boost_model <- xgb.train(data = dtrain, 
                       params = params, 
                       nrounds = cv$best_iteration)
tr_predictions <- predict(boost_model, dtrain)
te_predictions <- predict(boost_model, dtest)
boost_pred_tr <- factor(ifelse(tr_predictions > 0.5, "yes", "no"))
boost_pred_te <- factor(ifelse(te_predictions > 0.5, "yes", "no"))
caret::confusionMatrix(data=boost_pred_tr, reference = df_tr$deposit, positive = "yes")
caret::confusionMatrix(data=boost_pred_te, reference = df_te$deposit, positive = "yes")
## On the example I could run, it was only partially solved: 
## apparent accuracy = 93%, test accuracy=90%

## However, setting nround to such a low value (3 in my case) 
## is a little annoying. An alternative is to keep a larger nround and 
## add a learning rate < 1, and manage tree parameters.
set.seed(567)
params <- list(
  booster="gbtree", 
  objective = "binary:logistic",  
  eval_metric = "error",   
  eta = 0.1, ## learning rate,
  max.depth=3
)
cv <- xgb.cv(data = dtrain, 
             nrounds = 1000,
             nfold = 10, 
             params=params,
             early_stopping_rounds = 10)
print(cv)
cv$best_iteration ## the best number of iterations...
boost_model <- xgb.train(data = dtrain, 
                       params = params, 
                       nrounds = cv$best_iteration)
tr_predictions <- predict(boost_model, dtrain)
te_predictions <- predict(boost_model, dtest)
boost_pred_tr <- factor(ifelse(tr_predictions > 0.5, "yes", "no"))
boost_pred_te <- factor(ifelse(te_predictions > 0.5, "yes", "no"))
caret::confusionMatrix(data=boost_pred_tr, reference = df_tr$deposit, positive = "yes")
caret::confusionMatrix(data=boost_pred_te, reference = df_te$deposit, positive = "yes")

## This solved almost completely the overfitting.

## ###############################
## Another possibility is to use caret::train
## We use 10-CV
ctrl <- trainControl(method = "cv", number = 10)

# Define parameter grid for tuning
## We tune the learning rate (eta) for fixed nrounds for illustration
param_grid <- expand.grid(
  eta = c(0.001, 0.005, 0.01, 0.1, 0.2),     # candidate learning rates
  max_depth = c(3,5,7),           # Max tree depth
  nrounds = c(50, 100, 200),        # Number of boosting rounds
  gamma = c(0, 0.1),           # Minimum loss reduction required to make a further partition on a leaf node of the tree
  colsample_bytree = c(0.6, 0.8, 1),  # Feature subsampling
  min_child_weight = c(1, 3, 5),  # Regularization on node splits
  subsample = c(0.5, 0.7, 0.9)        # Subsample ratio of the training instance
)
# Train the XGBoost model 
## IT CAN BE VERY LONG
set.seed(97)
xgb_model <- train(
  deposit ~ ., 
  data = df_tr, 
  method = "xgbTree", 
  trControl = ctrl,
  tuneGrid = param_grid
)
xgb_model ## see which one is the best model
print(xgb_model$bestTune)
## On my run I found again a accuracy of 90% with eta=0.1

