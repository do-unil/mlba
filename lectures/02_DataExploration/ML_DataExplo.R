## Data
library(liver) 
data(bank)

## Meta data
?bank

## Since we are going to modify the data set, we make a workable copy
df <- bank

## Global data structure and view the data
str(df)
View(df)

## ########################################
## Prepare the data: 
## Reorder the factor modalities per decreasing frequency 
for (i in 1:ncol(df)){
  if (class(df[,i])=="factor"){
    df[,i] <- reorder(df[,i], df[,i], decreasing = TRUE, FUN=length)
  }
}

## ########################################
## Univariate: Exploration of each variables
library(summarytools)
library(dplyr)
df_sum <- dfSummary(df, max.distinct.values = 5)
df_sum %>% view()

## More statistics on numerical variables
library(psych)
describe(df, omit=TRUE, skew=FALSE, IQR = TRUE)

## More summary using the R base function
summary(df)

## ##########################################
## Bivariate: Exploration of dependence with the outcome (deposit)

## Global summary per deposit
df_sum_depo <- df %>% group_by(deposit) %>% dfSummary(max.distinct.values = 5)
df_sum_depo %>% view()

## Numbers
## Summary statistics per deposit
## Caution: cat are also included
describe(df~deposit, skew=FALSE, IQR = TRUE)

## Graphs
library(ggplot2)
## num*cat: several histograms
ggplot(df, aes(x = age)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(deposit ~ .)
## num*cat: boxplots
ggplot(df, aes(x = age)) +
  geom_boxplot(fill = "white", colour = "black") +
  facet_grid(deposit ~ .)
## Discuss outliers? Should they be removed?

## cat*cat: bar plots
ggplot(df, aes(x=job, fill=deposit)) + 
  geom_bar(position="dodge") + coord_flip()
## cat*cat: mosaic plot
library(ggmosaic)
ggplot(df) +
  geom_mosaic(aes(x = product(job), fill=deposit), show.legend = FALSE) +
  coord_flip()

## num*num (not deposit here) : scatterplots 
library(GGally)
ggpairs(bank[,c(1,6,10,12,13)])

## Try also library(scatterPlotMatrix); scatterPlotMatrix(df[,c(1,6,10,12,13)])

## #########################################
## Some multivariate exploration

## num*num*cat: scatterplot with deposit
ggpairs(df[,c(1,6,10,12,13,17)], aes(colour = deposit, alpha = 0.4))

## cat*cat*cat: sankey diagram
library(ggsankey)
df_skey <- df %>% make_long(housing, deposit, job)
ggplot(df_skey, aes(x = x, next_x = next_x, 
                    node = node,
                    next_node = next_node,
                    fill = node,
                    label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black",
              show.legend = FALSE) + 
  geom_sankey_label(show.legend = FALSE)

## To see better, we diminish the number of modalities in job
df$job_short <- ifelse(df$job %in% c("management","blue-collar","technician","admin.","services","retired"),
         as.character(df$job), "other") %>% factor()
df_skey <- df %>% make_long(housing, deposit, job_short)
ggplot(df_skey, aes(x = x, next_x = next_x, 
                    node = node,
                    next_node = next_node,
                    fill = node,
                    label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = "black",
              show.legend = FALSE) + 
  geom_sankey_label(show.legend = FALSE) +   theme_sankey(base_size = 16)

## only num: parallel coordinates
ggparcoord(df,
           columns = c(1,6,10,12,13), groupColumn = 17, 
           scale="center",
           showPoints = TRUE, 
           order="anyClass")
