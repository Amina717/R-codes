

# Load necessary libraries
library(readxl)
library(tidyverse)

# Load the data
data <- read_excel("ERIMData.xlsx")

# View first few rows
str(data)

#*******************Data Wrangling*************888

# Check for missing values
colSums(is.na(data))

# Remove rows with missing values
data_clean <- na.omit(data)


# Convert to factor
data_clean$HHInc <- as.factor(data_clean$HHInc)
data_clean$ResType <- as.factor(data_clean$ResType)
data_clean$ResStatus <- as.factor(data_clean$ResStatus)
data_clean$Cable <- as.factor(data_clean$Cable)
#********************Data understanding****************
# Summary statistics
summary(data)

# Load ggplot2 for plotting
library(ggplot2)

# Plotting the distribution of HHInc
ggplot(data, aes(x = HHInc)) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.7) +
  ggtitle("Distribution of Household Income Categories") +
  xlab("Household Income Category") +
  ylab("Frequency")


# Calculating average work hours for male and female adults
avg_work_hours <- data %>% 
  summarise(Avg_MWrkHrs = mean(MWrkHrs, na.rm = TRUE), 
            Avg_FWrkHrs = mean(FWrkHrs, na.rm = TRUE))

# Creating the bar graph
barplot(height = c(avg_work_hours$Avg_MWrkHrs, avg_work_hours$Avg_FWrkHrs),
        names.arg = c("Male", "Female"),
        main = "Average Work Hours by Gender",
        xlab = "Gender",
        ylab = "Average Work Hours",
        col = "blue")


# Barplot for Households with Cable TV by Income Level
library(ggplot2)
# Convert Cable to a factor
data_clean$Cable <- as.factor(data_clean$Cable)

# Assuming data_clean is your cleaned data frame
ggplot(data_clean, aes(x=HHInc, fill=Cable)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("red", "blue"), labels=c("No Cable", "Has Cable")) +
  labs(title="Households with Cable TV by Income Level",
       x="Income Category",
       y="Count",
       fill="Cable TV Status")

# Bar Graph for Types of Residences
ggplot(data_clean, aes(x=ResType)) +
  geom_bar(aes(fill=ResType), alpha=0.7) +
  labs(title="Most Common Types of Residences",
       x="Residence Type",
       y="Count")

#***********************Modeling 1******************


# Load the MASS package for ordinal logistic regression
#install.packages("MASS") # Install if not already installed
data_clean$HHInc <- ordered(data_clean$HHInc)
library(MASS)

# Assuming 'data_clean' is your cleaned and preprocessed dataframe
# Ordinal Logistic Regression Model to predict HHInc based on MWrkHrs
model1 <- polr(HHInc ~ MWrkHrs, data=data_clean, Hess=TRUE)

# Summary of the model to check various metrics like p-value etc.
summary(model1)


#***********************Modeling 2*******************
# Make sure 'data_clean' is your cleaned and preprocessed dataframe and that HHInc is an ordered factor
# Ordinal Logistic Regression Model to predict HHInc based on MEdu and FEdu
model2 <- polr(HHInc ~ MEdu + FEdu, data=data_clean, Hess=TRUE)

# Summary of the model to check various metrics like p-value etc.
summary(model2)



