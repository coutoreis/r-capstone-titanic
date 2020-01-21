# EDX HarvardX Capstone Project - Titanic Survival Prediction

---
title: "EDX Capstone Project - Titanic Survival Prediction"
author: "Thiago do Couto"
date: "13/06/2019"
output: html_document
---
# R Capstone - Standalone Project
## Titanic Survival Prediction
This project aims to predict if a certain passenger would survive the Titanic disaster and show the importance of each variable.


## 1 - Load required libraries
We'll use Random Forest algorithm in this prediction project, the Grammar of Graphics to the Importance Plot and Dplyr to use the Glimpse function.
```{r}
# Load required libraries
library(randomForest)
library(ggplot2)
library(dplyr)
```

## 2 - Load datasets
```{r}
# Load datasets
train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv",  stringsAsFactors = TRUE)
```

## 3 - Exploratory Data Analysis
Verify NAs in both sets.
```{r}
# Verify NAs
colSums(is.na(train)) 
colSums(is.na(test))
```

Create the Target variable (Survival) in Test Set.
```{r}
# Create target variable in Test set
test$Survived <- NA
```

Create variable 'IsTrainSet' to track if the observation is from Test or Train set.
```{r}
# Create variable to track if the observation is from Test or Train set
train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE
```

Group datasets so that we can work with it.
```{r}
# Group datasets
full_df <- rbind(train, test)
```

Let's take a macro view of the set.
```{r}
glimpse(full_df)
```
As we cah see, there are variables that have data types that hold us to work with them. We'll treat them soon.

So, lets make a summary analysis.
```{r}
# Dataframe summary
summary(full_df)
```
As we can see, there is 1 NA in Fare, 418 NAs in Survived (because of the Test set), 263 NAs in Age and 2 NAs in Embarked. We'll deal with them later. 

Let's analyse specifically the NAs.
```{r}
# Check for invalid data
colSums(is.na(full_df)) 
```
So, let's treat them accordingly. 

## 4 - Data Transformations

Lets initially treat the NAs observations.

As there are some occurences for ordinal data, we'll use the MEDIAN value to fill the fields.
```{r}
# As there are some occurences for ordinal data, we'll use the MEDIAN value to fill the fields.
full_df$Age[is.na(full_df$Age)] <- median(full_df$Age, na.rm = TRUE)
full_df$Fare[is.na(full_df$Fare)] <- median(full_df$Fare, na.rm = TRUE)
```

As there are 2 occurrences of NAs in Embarked, we'll use the most common value to fill the fields.
```{r}
# As there are 2 occurrences of NAs in Embarked, we'll use the most common value to fill the fields.
full_df$Embarked[full_df$Embarked==""] <-"S"
```

As foretold, there are some classes that can avoid us to work accordingly with the data.

Coerce data types to factor (when categorical) and to numeric (when ordinal).
```{r}
# Coerce data types to factor (when categorical) and to numeric (when ordinal).
full_df$Survived <- as.factor(full_df$Survived)
full_df$Pclass <- as.factor(full_df$Pclass)
full_df$SibSp <- as.numeric(full_df$SibSp)
full_df$Parch <- as.numeric(full_df$Parch)
full_df$Embarked <- as.factor(as.character(full_df$Embarked))
```

## 5 - The Random Forest Model

Now that we have the dataset treated, let's build the model.
```{r}
# Building the model
train_set <- full_df[full_df$IsTrainSet == TRUE, ]
test_set <- full_df[full_df$IsTrainSet == FALSE, ]
rf_model <- randomForest(formula = as.formula("Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked"), data = train_set, ntree = 50, importance = TRUE)
```

Let's visualize the model results.
```{r}
# Visualizing the model
rf_model
plot(rf_model)
```

There we can sse the model error and accuracy.


Let's generate the importance Matrix of the variables.
```{r}
# Gerenating importance matrix
importance_var <- importance(rf_model, type = 1)
importance_var
```


Let's plot the graph of the Importance attributes. The higher the Importance the most it impacts the possibility of Survivability .
```{r}
# Generating importance graph
importance_df <- data.frame(variables = row.names(importance_var), relevancy = importance_var[,1]);importance_df
importance_graph <- ggplot(importance_df, aes(x=reorder(variables, relevancy), y = importance_var)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Model - Variable Importance") +
  theme(plot.title = element_text(size = 18))
importance_graph
```

We'll then generate the model versus data in the test set, removing the previous sent NAs with the correct prediction values.
```{r}
# Create a Data Frame with PassengerID
final_df <- data.frame(PassengerId = test$PassengerId,
                         Survived = predict(rf_model, newdata = test_set))
View(final_df)
```
