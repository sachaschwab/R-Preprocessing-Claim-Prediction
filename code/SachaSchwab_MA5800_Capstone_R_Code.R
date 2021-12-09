# Student: Sacha Schwab
# R Code to Capstone Assessment MA5800 Foundations for Data Science

#~~~~~~~~~~~ 
# Libraries 
#~~~~~~~~~~
library(dplyr) 
library(ggplot2) 
library(tidyr)

#~~~~~~~~~~~~~~~
# Preparations
#~~~~~~~~~~~~~~~

# Tidy up workspace
rm(list=ls())
# Set work environment
setwd('/Users/sachaschwab/Dropbox/JCU/03 Foundations/Assessments/Capstone')
df=read.csv('travel insurance.csv')

View(df)
# Variables
str(df)
# Number of rows
nrow(df)
# Number of variables
ncol(df)

# Get R version for report
RStudio.Version()
# Remove unnecessary records and variables
# Summary
summary(df)
colnames(df)

# Data cleaning: Identify unnecessary records
nrow(df[df$Duration <= 0,])
nrow(df[df$Age > 99,])
nrow(df[df$Gender == "",])

# Data cleaning: Eliminate unnecessary records
df <- df %>%
  filter(Duration > 0) %>%
  filter(Age < 100) %>%
  filter(Gender != "")

# Type conversion: Convert 2 categorical binomial variable values into integer 1/2 for more efficient processing
df$Distribution.Channel <- as.integer(df$Distribution.Channel)
df$Agency.Type <- as.integer(df$Agency.Type)

# Investigate whether the data is balanced or imbalanced
df$Claim <- as.factor(df$Claim)
ggplot(data.frame(df$Claim), aes(x=df$Claim)) +
  geom_bar(fill=c('goldenrod1', 'green')) + ggtitle("Distribution of Claim") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Claim") + ylab("Count")

# Data representation:
# Investigate proximity measures
library(cluster)
Dist <- daisy(df_sampled, metric = "gower")
Dist <- as.matrix(Dist)
dim <- ncol(Dist)  # used to define axis in image
image(1:dim, 1:dim, Dist, axes = FALSE, xlab="", ylab="", col = rainbow(100))

# Variable selection
df <- select(df, Agency, Agency.Type, Distribution.Channel, Product.Name, Claim, Duration, Destination, Age)
View(df)
# Variable visualisation and transformation
# Investigate visual spread of variables according to Claim
# Claim per Agency
agency_summary <- df %>%
  group_by(Agency, Claim) %>%
  summarise(n = n()) %>%
  mutate(percClaim = 100 / sum(n) * n) %>%
  arrange(desc(Agency))
#View(agency_summary)
agency_summary <- agency_summary[agency_summary$Claim == "Yes", ]
agency_summary <- arrange(agency_summary, desc(percClaim))
ggplot(agency_summary, aes(x=reorder(Agency, -percClaim), y=percClaim)) + geom_col(fill="darkorchid4") +
  xlab("Agency") + ylab("% (Claim = 'Yes')") +
  ggtitle("Percentage Claim(Yes) per Agency") + theme(plot.title = element_text(hjust = 0.5))
# Calculate standard deviation
sd(agency_summary$percClaim)

# Claim per Agency Type
agency_type_summary <- df %>%
  group_by(Agency.Type, Claim) %>%
  summarise(n = n()) %>%
  mutate(percClaim = 100 / sum(n) * n) %>%
  arrange(desc(Agency.Type))
View(agency_type_summary)
agency_type_summary <- agency_type_summary[agency_type_summary$Claim == "Yes", ]
agency_type_summary <- arrange(agency_type_summary, desc(percClaim))
ggplot(agency_type_summary, aes(x=reorder(Agency.Type, -percClaim), y=percClaim)) + geom_col(fill="blue4") +
  xlab("Agency Type") + ylab("% (Claim = 'Yes')") +
  ggtitle("Percentage Claim(Yes) per Agency Type") + theme(plot.title = element_text(hjust = 0.5))
# Calculate standard deviation
sd(agency_type_summary$percClaim)

# Claim per Distribution Platform
distr_summary <- df %>%
  group_by(Distribution.Channel, Claim) %>%
  summarise(n = n()) %>%
  mutate(percClaim = 100 / sum(n) * n) %>%
  arrange(desc(Distribution.Channel))
View(distr_summary)
distr_summary <- distr_summary[distr_summary$Claim == "Yes", ]
distr_summary <- arrange(distr_summary, desc(percClaim))
ggplot(distr_summary, aes(x=reorder(Distribution.Channel, -percClaim), y=percClaim)) + geom_col(fill="blue3") +
  xlab("Distribution Platform") + ylab("% (Claim = 'Yes')") +
  ggtitle("Percentage Claim(Yes) per \n Distribution Platform") + theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x=Agency, fill=Claim)) + ggtitle("Distribution of Claim \n among Agencies") +
  geom_bar(position="Fill") + theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Ratio")
# Calculate standard deviation
sd(distr_summary$percClaim)

# Claim per Product
prod_summary <- df %>%
  group_by(Product.Name, Claim) %>%
  summarise(n = n()) %>%
  mutate(percClaim = 100 / sum(n) * n) %>%
  arrange(desc(Product.Name))
View(prod_summary)
prod_summary <- prod_summary[prod_summary$Claim == "Yes", ]
prod_summary <- arrange(prod_summary, desc(percClaim))
ggplot(prod_summary, aes(x=reorder(Product.Name, -percClaim), y=percClaim)) + geom_col(fill="blue3") +
  xlab("Product Name") + ylab("% (Claim = 'Yes')") +
  ggtitle("Percentage Claim(Yes) per Product") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Calculate standard deviation
sd(prod_summary$percClaim)

# Skewness of duration
ggplot(data=df, aes(df$Duration)) + geom_histogram(fill="blue4") +
  xlab("Duration") + ylab("Count") +
  ggtitle("Spread of Duration") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Normalise with log transform
df$Duration <- log(df$Duration)
# Re-do the histogram
ggplot(data=df, aes(df$Duration)) + geom_histogram(fill="blue2") +
  xlab("Duration") + ylab("Count") +
  ggtitle("Spread of Duration") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate percentage of Yes (Claims)
100 / nrow(df) * nrow(df[df$Claim == "Yes",])
# Calculate number of Yes (Claims)
total_yes <- nrow(df[df$Claim == "Yes",])
total_yes

# Subsampling with Total Claims=Yes * 10
yes_data <- df[df$Claim == "Yes",]
no_data <- df[df$Claim == "No",]
no_data_sample <- sample_n(no_data, total_yes / 3 * 7)
df_sampled <- rbind(yes_data, no_data_sample)
View(df_sampled)
