## libraries
library(liver)
library(caret)
library(dplyr)
library(car)
library(vip)
library(pdp)
library(ggplot2)
library(iml)
library(lime)
library(ranger)

## load data
data(bank)
df <- bank
levels(df$job)[c(3,4,7,6,7,9,11,12)] <- "other"

## For interpretation, we skip the training/test split

## Training the model (here logistic regression)
bank_lr <- glm(deposit~., data=df, family = binomial())
bank_lr_sel <- step(bank_lr, trace = FALSE)

bank_prob <- predict(bank_lr_sel, newdata=df, type="response")
bank_pred <- factor(ifelse(bank_prob > 0.5, "yes", "no"))
  
## ######################################
## ######################################
## ######################################
## ######################################
## Variable importance: model agnostic
## First by hand - Cohen's kappa is a reference
kapp <- postResample(pred = bank_pred, obs = df$deposit)[[2]] ## reference kappa

n_var <- ncol(df) - 1
kapp_vi <- data.frame(Var=names(df)[1:n_var],
                      VImp=numeric(n_var))
for (j in 1:n_var){
  bank_shuffle <- df
  bank_shuffle[,j] <- sample(bank_shuffle[,j]) ## shuffle column j
  
  ## predictions 
  bank_prob <- predict(bank_lr_sel, newdata=bank_shuffle, type="response")
  bank_pred <- factor(ifelse(bank_prob > 0.5, "yes", "no"))
  
  ## Compute kappa after the shuffling operation
  ## and store the difference with the initial kappa
  kapp_vi[j,2] <- kapp - postResample(pred = bank_pred, obs = df$deposit)[[2]]
}
kapp_vi
## Graphical representation of the differences to the unmodified kappa
## Larger difference => the variable is important
kapp_vi %>%
  ggplot(aes(x=reorder(Var, VImp), y=VImp)) +
  geom_segment(aes(xend=Var, yend=0)) +
  geom_point(size=2) +
  coord_flip() +
  theme_bw() +
  xlab("") + ylab("VImp: kappa - kappa_shuffled")

## Then using vip package
vip::list_metrics() ## possible metrics

## we need a prediction wrapper: a function that creates our predictions
## It is simply "predict" with some predefined parameters and calculation 
## of the prediction (thresholding)
mypred <- function(object, newdata){
  factor(ifelse(predict(object, newdata, type="response")>0.5,"yes","no"))
}
## Now we call function vi
vi(bank_lr_sel, method = "permute", 
   target="deposit", metric = "accuracy", 
   pred_wrapper = mypred, new_data=df,
   train=df, nsim=10) %>% vip(geom="boxplot")

## Now we adapt the vi for AUC
## First we change the prediction function wrapper for it must
## provide the predicted probabilities (before, predictions were required)
mypred <- function(object, newdata){
  predict(object, newdata, type="response")
}
vi(bank_lr_sel, method = "permute", target="deposit", 
   metric = "roc_auc", 
   pred_wrapper = mypred, new_data=df, 
   train=df, reference_class="yes", nsim=10) %>% vip(geom = "boxplot")

## ######################################
## variable importance: model specific
## We repeat the previous exercise but using model specific VIP
## Below we use random forest and function ranger

bank_rf <- ranger(deposit~., data=df, importance="impurity") 
bank_rf$variable.importance
rf_vip <- data.frame(Var=names(bank_rf$variable.importance),
                 VImp=bank_rf$variable.importance) 
rf_vip %>%
  ggplot(aes(x=reorder(Var, VImp), y=VImp)) +
  geom_segment(aes(xend=Var, yend=0)) +
  geom_point(size=2) +
  coord_flip() +
  theme_bw() +
  xlab("")

## #####################################
## #####################################
## #####################################
## #####################################
## partial dependence plot (PDP)

##
## Example for duration & age
## We first create sequences of "duration" and of "age", from min to max
n_seq <- 100
duration_seq <- seq(from=min(df$duration), to=max(df$duration), length=n_seq)
age_seq <- seq(from=min(df$age), to=max(df$age), length=n_seq)

duration_pdp <- data.frame(duration=duration_seq, 
                           Probability=numeric(n_seq))
age_pdp <- data.frame(age=age_seq, 
                      Probability=numeric(n_seq))
bank_dur <- data.frame(df)
bank_age <- data.frame(df)
for (i in 1:n_seq){
  ## For the current duration:
  bank_dur$duration <- duration_seq[i] # Replace in the data frame all duration by the current duration of the loop
  bank_prob <- predict(bank_lr_sel, 
                       newdata=bank_dur, type="response") # Compute the probabilities for all these data (with that duration)
  duration_pdp[i,2] <- mean(bank_prob) # average all these probabilities
  
  ## Repeats for age:
  bank_age$age <- age_seq[i]
  bank_prob <- predict(bank_lr_sel, newdata=bank_age, type="response")
  age_pdp[i,2] <- mean(bank_prob)
}

duration_pdp
age_pdp
duration_pdp %>% ggplot(aes(x=duration, y=Probability)) +
  geom_line() + ylim(0,1)
age_pdp %>% ggplot(aes(x=age, y=Probability)) +
  geom_line() + ylim(0,1)

##
## Example for categorical variable: here job
bank_job <- data.frame(df)
job_seq <- unique(bank_job[,"job"]) # now the sequence is all the possible modalities

n_seq <- length(job_seq)
job_pdp <- data.frame(job=job_seq, 
                      Probability=numeric(n_seq))
for (i in 1:n_seq){
  bank_job[,"job"] <- job_seq[i]
  bank_prob <- predict(bank_lr_sel, newdata=bank_job, 
                       type="response")
  job_pdp[i,2] <- mean(bank_prob)
}
job_pdp %>% 
  ggplot(aes(x=reorder(job, Probability), y=Probability)) + 
  geom_segment(aes(xend=job, yend=0)) +
  geom_point(size=2) +
  coord_flip() + ylim(0,1) + xlab("")

## with pdp
pdp::partial(bank_lr_sel, pred.var = "duration", prob=TRUE, plot=TRUE)
pdp::partial(bank_lr_sel, pred.var = "age", prob=TRUE, plot=TRUE)
pdp::partial(bank_lr_sel, pred.var = "job", prob=TRUE, plot=TRUE, plot.engine = "ggplot") + coord_flip()

pdp::partial(bank_lr_sel, pred.var = c("duration", "age"), prob=TRUE, plot=TRUE)
pdp::partial(bank_lr_sel, pred.var = c("duration", "contact"), prob=TRUE, plot=TRUE)
pdp::partial(bank_lr_sel, pred.var = c("duration", "job"), prob=TRUE, plot=TRUE)

## ###########################
## with iml: a general package that works well when one works a lot...

##
## Variable Importance
## Defines a prediction wrapper
mypred <- function(object, newdata){
  factor(ifelse(predict(object, newdata, type="response")>0.5,"yes","no"))
}
## Set the predictor; here we use the logistic regression after AIC-based selection
predictor <- Predictor$new(bank_lr_sel, data = bank_var, y=bank_var$deposit,                           
                           predict.function = mypred)
## Compute the variable importance based on Classification Error (!!! not Cross-Entropy)
imp <- FeatureImp$new(predictor, loss="ce")
plot(imp) ## equiv. imp$plot()

## Same as above with Random Forest (ranger); here, no wrapper is needed
bank_rf <- ranger(deposit~., data=df)
predictor <- Predictor$new(bank_rf, data = df[,-17], y=df$deposit, type="response")
imp <- FeatureImp$new(predictor, loss="ce")
plot(imp) ## equiv. imp$plot()

##
## PDP
bank_rf <- ranger(deposit~., data=df, probability = TRUE) # Here probability must be set to TRUE
mypred <- function(object, newdata){
  predict(object, data=newdata)$predictions[,"yes"]
}
predictor <- Predictor$new(bank_rf, data = df[,-17], y=df$deposit,
                           predict.function = mypred)
bank_pdp <- FeatureEffect$new(predictor, feature = "duration", 
                              method="pdp", grid.size = 10)
bank_pdp$plot(rug=FALSE, ylim=c(0,1)) ## equiv. plot(bank_pdp, rug=FALSE, ylim=c(0,1), yaxlabel="VV")
bank_pdp$print()

## change feature
bank_pdp$set.feature("age")
bank_pdp$plot(rug=FALSE, ylim=c(0,1))
bank_pdp$set.feature("job")
bank_pdp$plot(rug=FALSE, ylim=c(0,1))

## ICE curves: like PDP but one curve for each data row -> PDP is the point-wise mean
bank_ice <- FeatureEffect$new(predictor, feature = "duration", 
                              method="ice", grid.size = 10)
bank_ice$plot(rug=FALSE, ylim=c(0,1)) 

## ################################
## ################################
## LIME
## search the instances with predicted probabilities close to 1, 0, and 0.5 
pred_bank_rf <- predict(bank_rf, data=df)$predictions
which.max(pred_bank_rf[,"yes"]) # Case with largest P(=Yes)
which.min(pred_bank_rf[,"yes"]) # Case with largest P(=No)
which.min((pred_bank_rf[,"yes"]-0.5)^2) # Case P(=Yes) closest to 0.5

bank_lime <- lime(df[,-17], bank_rf, bin_continuous = TRUE, quantile_bins = FALSE)
bank_expl <- lime::explain(df[c(2450, 189, 1110),-17], bank_lime, n_labels=1, n_features = 10)
bank_expl
plot_features(bank_expl, ncol = 2)

