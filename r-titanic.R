# Load required libraries
library(randomForest)
library(ggplot2)

# Load datasets
train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv",  stringsAsFactors = TRUE)

# Verify NAs
colSums(is.na(train)) 
colSums(is.na(test))

# Create target variable in Test set
test$Survived <- NA

# Create variable to track if the observation is from Test or Train set
train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE

# Group datasets
full_df <- rbind(train, test)

# Dataframe summary
summary(full_df)

# Check for invalid data
colSums(is.na(full_df)) 

# As there are some occurences for ordinal data, we'll use the MEDIAN value to fill the fields.
full_df$Age[is.na(full_df$Age)]      <- median(full_df$Age, na.rm = TRUE)
full_df$Fare[is.na(full_df$Fare)]    <- median(full_df$Fare, na.rm = TRUE)

# As there are 2 occurrences of NAs in Embarked, we'll use the most common value to fill the fields.
full_df$Embarked[full_df$Embarked==""] <-"S"

# Coerce data types to factor (when categorical) and to numeric (when ordinal).
full_df$Survived <- as.factor(full_df$Survived)
full_df$Pclass <- as.factor(full_df$Pclass)
full_df$SibSp <- as.numeric(full_df$SibSp)
full_df$Parch <- as.numeric(full_df$Parch)
full_df$Embarked <- as.factor(as.character(full_df$Embarked))

# Building the model
train_set <- full_df[full_df$IsTrainSet == TRUE, ]
test_set <- full_df[full_df$IsTrainSet == FALSE, ]
rf_model <- randomForest(formula = as.formula("Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked"), data = train_set, ntree = 50, importance = TRUE)

# Visualizing the model
rf_model
plot(rf_model)

# Gerenating importance matrix
importance_var <- importance(rf_model, type = 1)
importance_var

# Converting as dataframe to use ggplot
importance_df <- data.frame(variables = row.names(importance_var), relevancy = importance_var[,1]);importance_df

# Generating importance graph
importance_graph <- ggplot(importance_df, aes(x=reorder(variables, relevancy), y = importance_var)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Model - Variable Importance") +
  theme(plot.title = element_text(size = 18))
importance_graph

# Create a Data Frame with PassengerID
final_df <- data.frame(PassengerId = test$PassengerId,
                         Survived = predict(rf_model, newdata = test_set))
View(final_df)