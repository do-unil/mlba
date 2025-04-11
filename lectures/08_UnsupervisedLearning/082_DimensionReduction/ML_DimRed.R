## libraries
library(liver)
library(dplyr)
library(ggplot2)
library(FactoMineR) 
library(factoextra) 
library(keras) ## for Auto-Encoder
library(vip) ## for the variable importance (ae interpretation)

install.packages("remotes")
library(remotes)
remotes::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")

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

