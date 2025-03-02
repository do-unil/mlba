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
ggplot(df, aes(x=job, fill=education)) + 
  geom_bar(position="stack") + coord_flip()
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

# Load necessary libraries
library(ggplot2)
library(ggalluvial)

# Plot Sankey diagram using ggalluvial
df %>%
  count(housing, deposit, job, name = "count") %>%
ggplot(aes(axis1 = housing, axis2 = deposit, axis3 = job, y = count)) +
  geom_alluvium(aes(fill = housing)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Housing", "Deposit", "Job")) +
  theme_minimal() +
  labs(title = "Sankey Diagram using ggalluvial")

## Rename job label with category "other"
job_counts <- df %>%
  count(job, name = "count") %>%
  arrange(desc(count))
total_count <- sum(job_counts$count)
cumulative_sum <- cumsum(job_counts$count)
job_counts <- job_counts %>%
  mutate(cumulative_prop = cumulative_sum / total_count)
top_jobs <- job_counts$job[job_counts$cumulative_prop <= 0.80]
levels(df$job) <- ifelse(levels(df$job) %in% top_jobs, levels(df$job), "other")

df %>%
  count(housing, deposit, job, name = "count") %>% 
  ggplot(aes(axis1 = housing, axis2 = deposit, axis3 = job, y = count)) +
  geom_alluvium(aes(fill = housing)) +
  geom_stratum(reverse=FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Housing", "Deposit", "Job")) +
  theme_minimal() +
  labs(title = "Sankey Diagram")


