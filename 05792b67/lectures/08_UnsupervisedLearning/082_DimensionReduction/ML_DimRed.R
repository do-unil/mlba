## libraries
library(liver)
library(dplyr)
library(ggplot2)
library(FactoMineR) 
library(factoextra) 

library(keras) ## for AE
library(vip) ## for the variable importance (ae interpretation)

## load data
data(house) 
df <- house[,-c(4,5)] 

## ##################################
## PCA: factor analysis on numerical variables
house.pca <- PCA(df, graph=FALSE) ## produce PCA (without the column Species)
summary(house.pca)

fviz_eig(house.pca, addlabels = TRUE) ## screeplot to inspect the number of components

fviz_pca_var(house.pca) ## circle of correlations
fviz_cos2(house.pca, choice="var") ## cos2 plot; dimension 1

fviz_pca_ind(house.pca) ## map of individuals

fviz_pca_biplot(house.pca) ## biplot

## ##################################
## ##################################
## ##################################
## Autoencoder with house data
data(house)
df <- house[,-c(4,5)] 

## Prepare the data
set.seed(324) 
x_train <- as.matrix(df) 

## Define model
model <- keras_model_sequential(input_shape=ncol(x_train)) ## input layer
model %>% 
  layer_dense(units=2, activation = "linear",
              name="intermediate") %>% ## intermediate layer
  layer_dense(units=ncol(x_train), activation = "linear") ## output layer

## show the model structure
summary(model)

## Compile the model
model %>% compile(optimizer=optimizer_adam(0.1),
                  loss="mean_absolute_error")
## fit the model
model %>% fit(x_train, x_train, epoch=100, batch_size=10)

## Extract the coding
xx <- get_layer(model, name = "intermediate")
inter_output_model <- keras_model(input = model$input, output = 
                                    xx$output)
embed <- inter_output_model %>% predict(x_train)
## plot of the intermediate layers
plot(embed, pch=20, xlab="Node 1", ylab="Node 2") 

## Another model (deeper encoding)
set.seed(718)
model <- keras_model_sequential(input_shape=ncol(x_train))
model %>% 
  layer_dense(units=6, activation = "linear",
              name="intermediate1") %>% 
  layer_dense(units=3, activation = "linear",
              name="intermediate2") %>%
  layer_dense(units=4, activation = "linear")
summary(model)
model %>% compile(optimizer=optimizer_adam(0.1),
                  loss="mean_absolute_error")

model %>% fit(x_train, x_train, epoch=50, batch_size=30)
ae_pred <- model %>% predict(x=x_train)

## Intermediate layer is intermediate2. It has 3 nodes
xx <- get_layer(model, name = "intermediate2")
inter_output_model <- keras_model(input = model$input, output = 
                                    xx$output)
embed <- inter_output_model %>% predict(x_train)
plot(embed, pch=20) 

## ##############################
## Analysis of the encoding link to the data
mypred <- function(object, newdata){
  xx <- newdata
  predict(object, xx)[,3]
}
newdata <- cbind(x_train, Dim=embed[,1])
vi(inter_output_model, method = "permute", target="Dim", 
   metric = "rmse", pred_wrapper = mypred, train=newdata) %>% vip()
newdata <- cbind(x_train, Dim=embed[,2])
vi(inter_output_model, method = "permute", target="Dim", 
   metric = "rmse", pred_wrapper = mypred, train=newdata) %>% vip()
newdata <- cbind(x_train, Dim=embed[,3])
vi(inter_output_model, method = "permute", target="Dim", 
   metric = "rmse", pred_wrapper = mypred, train=newdata) %>% vip()

