
library(ggplot2)
library(dplyr)

library(psych)



#import the data sets
df <- read.csv("data.csv")

# head of the dataset
head(df)
#summary of the dataset
summary(df)


# Data preprocessing

#Find the missing values 
print(colSums(is.na(df)))


# duplication of the dataset by showing the duplicated rows
duplicates <- duplicated(df)
df[duplicates, ]


#assign a new variable to dataset
df1<-df
# exploratory data analysis

# traget class distirbution

hist(df$MMSE_class_2, main = 'Target class distribution')


# Histogram for all the dataset

multi.hist(df)

# replace variables
df$Hyperlipidaemia <- factor(df$Endocrine.Disease_Hyperlipidaemia, levels=c(0, 1), labels=c("No", "Yes"))

# Barplot
ggplot(df, aes(x=Hyperlipidaemia)) + 
  geom_bar(fill="green") +
  labs(title="count for Individuals Based on Hyperlipidaemia Status", x="Hyperlipidaemia", y="Count") +
  theme_minimal()

#plot the Mobility
# labels
df$mobility <- factor(df$Mobility, levels=c(1, 2, 3, 4), labels=c("Wheelchair", "Walker", "Cane", "Self-mobility or self-activity"))

# Bar chart
ggplot(df, aes(x=mobility)) + 
  geom_bar(fill="skyblue") +
  labs(title="count for Individuals Based on Mobility", x="Mobility", y="Count") +
  theme_minimal()





#Model Prediciton

# Normalize the dataset


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
scaled_df1 <- as.data.frame(lapply(df1, normalize))
head(scaled_df1)


# factor the target class
df1$MMSE_class_2<-as.factor(df1$MMSE_class_2)

# split the data fro train and test
library(caret)
set.seed(400)
index <- createDataPartition(scaled_df1$MMSE_class_2, p = 0.7, list = FALSE)
train <- scaled_df1[index, ]
test  <- scaled_df1[-index, ]

dim(train)
dim(test)

# Apply the logitic regresison

# apply logistic regression
Logistice_regression_model <- glm(MMSE_class_2~., data=train, family='binomial')

# Predict the values using the test dataset
Logistice_regression_model_prediciton <- predict(Logistice_regression_model, test, type = 'response')
Logistic_predict <- ifelse(Logistice_regression_model_prediciton > 0.5, 1, 0)

# calculte the confusion metrics
regression_confu_matrixs <- confusionMatrix(as.factor(Logistic_predict), as.factor(test$MMSE_class_2))

# Calculate accuracy
accuracy <- sum(diag(regression_confu_matrixs$table)) / sum(regression_confu_matrixs$table)

# Calculate precision
precision <- regression_confu_matrixs$byClass['Pos Pred Value']


# Calculate recall
recall <- regression_confu_matrixs$byClass['Sensitivity']


# Calculate F1-score
f1 <- 2 * ((precision * recall) / (precision + recall))

regression_confu_matrixs$table
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1, "\n")

# Apply the  tree model


library(rpart)
library(rpart.plot)

classification_tree_model <- rpart(MMSE_class_2 ~ ., data=train, method="class")

# Predict the values using the test dataset
classification_tree_predictions <- predict(classification_tree_model, test, type="class")




# calculte the confusion metrics
tree_confu_matrixs <- confusionMatrix(as.factor(classification_tree_predictions), as.factor(test$MMSE_class_2))

# Calculate accuracy
accuracy <- sum(diag(tree_confu_matrixs$table)) / sum(tree_confu_matrixs$table)

# Calculate precision
precision <- tree_confu_matrixs$byClass['Pos Pred Value']


# Calculate recall
recall <- tree_confu_matrixs$byClass['Sensitivity']


# Calculate F1-score
f1 <- 2 * ((precision * recall) / (precision + recall))

tree_confu_matrixs$table
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1, "\n")

