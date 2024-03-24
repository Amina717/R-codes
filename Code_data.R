
# Import the libraries
library(ggplot2)
library(dplyr)
library(stats)
library(tidyverse)

#import the data sets

df <- read.csv("ds_salaries.csv")

# head of the dataset
head(df)

# Structure of data
str(df)


# Dataset pre-processing is being conducted here

# Discover the missing values
colSums(is.na(df))



# Data Visulization



# Top 10 Jobs with Avg Salary in USD by counting number of jobs

df1 <- df %>%
  group_by(job_title) %>%
  summarise(Average_Salary_in_USD = mean(salary_in_usd), Number_Jobs=n()) %>%
  arrange(desc(Number_Jobs), .by_group = FALSE) 

df2<-head(df1,10)

df2


gg<-ggplot(df2, aes(x = job_title, y = Average_Salary_in_USD,fill=Number_Jobs, label = Number_Jobs)) +
  ylab("Average Salary in USD")+
  xlab("Job Title")+
  ggtitle("Top 10 Jobs with Avg Salary in USD by counting number of jobs")+
  geom_col(position = "dodge")+
  geom_text(aes(label = Number_Jobs), vjust = 0.5,colour = "Red")
gg+coord_flip()




# Number of workers in companies by size and worker experience

p4 <- df%>% 
  group_by(experience_level, company_size)%>% 
  count()%>% 
  ggplot(mapping = aes(y = experience_level, x = n, fill = experience_level))+ 
  geom_bar(stat = 'Identity')+ 
  facet_wrap(~company_size)+ 
  theme_bw()+ labs(y = "Experience Level", x = "Number of positions", title = "Number of positions by experience level and company size")  

p4  + scale_fill_brewer(palette = "Pastel1")

