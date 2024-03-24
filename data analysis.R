

# Import the libraries these libraries

library(dplyr)
library(corrplot)
library(stats)
library(psych)
library(ggfortify)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(PerformanceAnalytics)
library(caret)
library(rpart)
library(rpart.plot)

#Dataset Read
df<-read.csv('Credit_Risk_25_final-2.csv',stringsAsFactors = TRUE)

# show 3 last rows in data
tail(df,3)

# dataset details
str(df)

# summary of the dataset
summary(df)


# Dataset pre-processing is being conducted here

# Discover the missing values
colSums(is.na(df))

# Remove the ID variables
df$ID<-NULL



#########........ a) Exploratory data analysis.........########


# Plot the loan reason using the month since accout is opened

ggplot(df, aes(x =  Loan.Reason, y = Months.since.Checking.Acct.opened)) +
  ylab("Months since Checking Acctount opened")+
  xlab("Loan Reason")+
  geom_col(position = "dodge", fill = "red")



# # Plot the credit history and resident time in districr using the credit standings

ggplot(df, aes(x = Credit.History, y = Age, fill = Credit.Standing)) +
  ylab("Residence Time In current district")+
  xlab("Credit History")+
  geom_col(position = "dodge")


# THree way relatioship between the months , residence using the saving accoutn\

qplot(x= Months.since.Checking.Acct.opened,
      y=Residence.Time.In.current.district,
      data = df,
      color=Savings.Acct,
      main="Three-way Relationship")


# Boxplot for age, month and residence

opar <- par(no.readonly = T) # Save the original graphical settings
par(mfrow = c(1,3)) # This will create a plot "matrix" with one row and three columns
boxplot(df$Age, main='Age')
boxplot(df$Months.since.Checking.Acct.opened, main='Months.since.Checking.Acct.opened')
boxplot(df$Residence.Time.In.current.district, main='Residence.Time.In.current.district')
par(opar) # Restore the original graphical settings


#plot for the  Ipersonal status count for the dataset
ggplot(df, aes(x =  Personal.Status)) +
  geom_bar(fill = "blue")+
  theme_gray()





#######........ b) Split the dataset into 75% training and 25% test set...........######


# make the partition of the data set in training and testing

set.seed(367)
#library(caret)

partition <- createDataPartition(df$Credit.Standing, p = 0.75, list = FALSE)
train <- df[partition, ]
test  <- df[-partition, ]

# Dimensions of the training and testing dataset
dim(train)
dim(test)


#######........ c) Split the dataset into 75% training and 25% test set...........######

# create a compy of the dataset

c2<-train

# probabiliteis for the target variable
table(c2$Credit.Standing)
(Churnpt <- prop.table(table(c2$Credit.Standing)))

z <- 1
tabfun <- function(x) {prop.table(table(c2[,x],c2[,14]), margin = z)}  
print(tabfun(5))  # checking it agrees with Exce





# Now as before create a function so that we can create any 2-D prop.table

entropy_tab <- function(x) { tabfun <- prop.table(table(c2[,x],c2[,13]) + 1e-6, margin = 1)
sum(prop.table(table(c2[,x]))*rowSums(-tabfun(x)*log2(tabfun(x))))}



#######Now create a for loop for the categorical variable to make the entropy##########

# Now create a for loop for the 9 categorical variables of interest

r <- c(1,2,3,4,5,6,7,8,9)

r3 <- NULL # Initialise variable
for (x in r) { r2 <- entropy_tab(x)
r3 <- c(r3,r2)
if (x == 9) print(r3)
}

# Find the info gain entropy  for all categorical variables

Churnpt <- prop.table(table(c2$Credit.Standing))
(entropy_total <-sum(-Churnpt*log2(Churnpt)))
r <- c(1,2,3,4,5,6,7,8,9)

r3 <- NULL
info_gain <- NULL
for (x in r) { r2 <- entropy_tab(x)

r3 <- c(r3,r2)
if (x == 9) 
{print(c("Entropy minimum",colnames(c2[r[which.min(r3)]]), round(min(r3),5)))
  print(c("Info Gain maximum", round((entropy_total-min(r3)),5)))}  
}


## d

library(partykit)


## binary split in numeric variable `Months.since.Checking.Acct.opened'

sl5 <- partysplit(which(names(train) == "Months.since.Checking.Acct.opened"),
                  breaks = 5)
character_split(sl5, data = train)
table(kidids_split(sl5, data = train), train$Months.since.Checking.Acct.opened <= 100)


## e

##f



# Yes it gives answer - but what is it doing?
(tabfun <- prop.table(table(c2[,2],c2[,13]), margin = 1))
# or
(table(c2[,2],c2[,13]))


#######........ Use the tree function from the package tree, or equivalent, to build a decision tree 
#and compare the
#results to those in g) and comment. If you use pruning here you should explain all the methodology
#you uset...........######

library(tree)
library(rpart)
library(rpart.plot)

tree_model <- tree( Credit.Standing~.,data = train)

summary(tree_model)

plot(tree_model)
text(tree_model,pretty=0)
tree_model

tree_pred <- predict(tree_model,test,type="class")
table(tree_pred,test$Credit.Standing)

#accuracy= 45+114/201 ===> 79.10%

# Now prunned the tree for more optimal results


tree_cv <- cv.tree(tree_model,K=10, FUN=prune.misclass)
names(tree_cv)
tree_cv

 tree_cv$k
# plot the results

par(mfrow=c(1,2))
plot(tree_cv$size,tree_cv$dev,type="b")
plot(tree_cv$k,tree_cv$dev,type="b")


#prune.misclass
prune_tree<-prune.misclass(tree_model,best=14)
plot(prune_tree)
text(prune_tree,pretty=0)
tree_pred<-predict(prune_tree,test,type="class")
table(tree_pred,test$Credit.Standing)

# after pruning acuuracy is same

#accuracy= 45+114/201 ===> 79.10%


# i) Now see if you can improve your results by using a random forest model. Give youre results (5 marks) and explain and comment (5 marks).

# apply the random forest 
library(randomForest)

set.seed(2)
rf_model <- randomForest(Credit.Standing~.,data = train, importance=TRUE)
print(rf_model)

pred <- predict(rf_model, test, type = "class")

# Get the confusion matrix here and the accuracy rate.
table(pred, test$Credit.Standing)

#accuracy= 63+104/201 ===> 83.0%


varImp(rf_model)

# then agian train the model 
set.seed(36)
high.rf <- randomForest(Credit.Standing~.,data = train, 
                        ntree = 1000, importance=TRUE, do.trace=TRUE)
print(high.rf)


pred <- predict(high.rf, test, type = "class")

varImp(high.rf)

# Get the confusion matrix here and the accuracy rate.
table(pred, test$Credit.Standing)

#accuracy= 66+105/201 ===> 85.07%

#The accuracy is bit increased

#j) Due to GDPR you are no longer allowed use the following variables to buld your model Age,
#Personal.Status and Foreign.National. Now redo your working for parts h) and i). Give your results
#and comment.

tree_model1 <- tree( Credit.Standing~.-Age -Personal.Status -Foreign.National,data = train)

summary(tree_model1)

plot(tree_model1)
text(tree_model1,pretty=0)
tree_model1

tree_pred1 <- predict(tree_model1,test,type="class")
table(tree_pred1,test$Credit.Standing)

#accuracy= 50+109/201 ===> 79.64%

# Model performance is same

# Now prunned the tree for more optimal results


tree_cv1 <- cv.tree(tree_model1,K=10, FUN=prune.misclass)
names(tree_cv1)
tree_cv1

tree_cv1$k
# plot the results

par(mfrow=c(1,2))
plot(tree_cv1$size,tree_cv1$dev,type="b")
plot(tree_cv1$k,tree_cv1$dev,type="b")


#prune.misclass
prune_tree<-prune.misclass(tree_model,best=9)
plot(prune_tree)
text(prune_tree,pretty=0)
tree_pred<-predict(prune_tree,test,type="class")
table(tree_pred,test$Credit.Standing)

# after pruning acuuracy is same

#accuracy= 45+114/201 ===> 79.10%

# Now apply the random forest on the dataset

set.seed(2)
rf_model <- randomForest(Credit.Standing~.-Age -Personal.Status -Foreign.National,data = train, importance=TRUE)
print(rf_model)

pred <- predict(rf_model, test, type = "class")

# Get the confusion matrix here and the accuracy rate.
table(pred, test$Credit.Standing)

#accuracy= 57+99/201 ===> 77.61%

# Accuracy is bit decreased


varImp(rf_model)


# then agian train the model 

set.seed(36)
high.rf <- randomForest(Credit.Standing~.-Age -Personal.Status -Foreign.National,data = train, 
                        ntree = 1000, importance=TRUE, do.trace=TRUE)
print(high.rf)


pred <- predict(high.rf, test, type = "class")



# Get the confusion matrix here and the accuracy rate.
table(pred, test$Credit.Standing)

#accuracy= 57+98/201 ===> 77.11%

#decreaesd

varImp(high.rf)
