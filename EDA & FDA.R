library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)
library(patchwork)
library(moderndive)

## Data Cleaning
# Read the data set
Data_FIES <- read.csv("dataset03.csv")
# Tidy the data
FIES <- Data_FIES %>%
  # Place the dependent variable in the first column and delete the unique value
  select(Total.Number.of.Family.members, everything(), -Region) %>%
  # Convert categorical variables into factors
  mutate(
    Household.Head.Sex = as.factor(Household.Head.Sex),
    Type.of.Household = as.factor(Type.of.Household),
    Electricity = as.factor(Electricity)) %>%
  # Remove Missing Values
  drop_na()

## Exploratory Data Analysis(EDA)
str(FIES) # Check data structure 
summary(FIES) # Get summary statistics of all variables  
dim(FIES) # Check dataset dimensions (number of rows and columns)

# Visualize dependent variable distribution
ggplot(FIES, aes(x = Total.Number.of.Family.members)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Family Size", x = "Number of Family Members", y = "Frequency")

# Compute mean and variance of the dependent variable
mean <- mean(FIES$Total.Number.of.Family.members)
var <- var(FIES$Total.Number.of.Family.members)
var_ratio <- var/mean
# var/mean=5.28/4.68=1.13≈1 indicates that there is no overdispersion problem, so Poisson regression model may be appropriate.

# Histograms for Continuous Variables
p1 <- ggplot(FIES, aes(x = Total.Household.Income)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Household Income Distribution", x = "Household Income", y = "Count")
p2 <- ggplot(FIES, aes(x = Total.Food.Expenditure)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Household Expenditure Distribution", x = "Household Expenditure", y = "Count")
p3 <- ggplot(FIES, aes(x = Household.Head.Age)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Household Head Age Distribution", x = "Household Head Age", y = "Count")
p4 <- ggplot(FIES, aes(x = House.Floor.Area)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "House Floor Area Distribution", x = "House Floor Area", y = "Count")
p5 <- ggplot(FIES, aes(x = House.Age)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "House Age Distribution", x = "House Age", y = "Count")
p6 <- ggplot(FIES, aes(x = Number.of.bedrooms)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Number of bedrooms Distribution", x = "Number of bedrooms", y = "Count")
(p1 | p2 | p3) / (p4 | p5 | p6)

# Boxplots for Numerical explanatory variables
p1 <- ggplot(FIES, aes(y = Total.Household.Income)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Household Income", y = "Household Income") 
p2 <- ggplot(FIES, aes(y = Total.Food.Expenditure)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Household Expenditure", y = "Household Expenditure") 
p3 <- ggplot(FIES, aes(y = Household.Head.Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Household Head Age", y = "Household Head Age") 
p4 <- ggplot(FIES, aes(y = House.Floor.Area)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "House Floor Area", y = "House Floor Area") 
p5 <- ggplot(FIES, aes(y = House.Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "House Age", y = "House Age") 
p6 <- ggplot(FIES, aes(y = Number.of.bedrooms)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Number of Bedrooms", y = "Number of Bedrooms") 
(p1 | p2 | p3) / (p4 | p5 | p6)

# Boxplots for Categorical explanatory variables
ggplot(FIES, aes(x = Household.Head.Sex, y = Total.Number.of.Family.members, fill = Household.Head.Sex)) +
  geom_boxplot() +
  labs(title = "Family Size by Household Head's Gender", x = "Household Head's Gender", y = "Family Size") 
ggplot(FIES, aes(x = Type.of.Household, y = Total.Number.of.Family.members, fill = Type.of.Household)) +
  geom_boxplot() +
  labs(title = "Family Size by Household Type", x = "Household Type", y = "Family Size") 
ggplot(FIES, aes(x = Electricity, y = Total.Number.of.Family.members, fill = Electricity)) +
  geom_boxplot() +
  labs(title = "Family Size by Electricity Access", x = "Electricity (0 = No, 1 = Yes)", y = "Family Size") 

## Formal Data Analysis
# Compute Correlation Matrix and Scatterplot Matrix
numeric_vars <- select(FIES,where(is.numeric))
cor_matrix <- cor(numeric_vars)
print(cor_matrix)
ggpairs(numeric_vars)

# Fit a Poisson regression model
poisson_model <- glm(Total.Number.of.Family.members ~ 
                       Total.Household.Income + 
                       Total.Food.Expenditure + 
                       Household.Head.Age + 
                       House.Floor.Area +
                       House.Age +
                       Number.of.bedrooms +
                       Household.Head.Sex +
                       Type.of.Household +
                       Electricity, 
                     family = poisson(link = "log"), 
                     data = FIES)
summary(poisson_model)

# Check the Relative Risk(RR)
exp(coef(poisson_model)) # For Poission Regression, coefficients represent log-relative risk, so convert them to RR

# plot the Pearson and deviance residuals against the linear predictor
resp <- resid(poisson_model, type = "pearson")
resd <- resid(poisson_model, type = "deviance")
p1 <- ggplot(poisson_model, aes(sample = resp)) + geom_point(stat = "qq", color = "steelblue") +
  ylab("Pearson residuals")
p2 <- ggplot(poisson_model, aes(sample = resd)) + geom_point(stat = "qq", color = "steelblue") +
  ylab("Deviance residuals")
p3 <- ggplot(poisson_model, aes(x = predict(poisson_model, type="link"), y =resp))+
  geom_point(col = "steelblue") +
  ylab("Pearson residuals") + xlab("Linear predictor")
p4 <- ggplot(poisson_model, aes(x = predict(poisson_model, type="link"), y =resd))+
  geom_point(col = "steelblue") +
  ylab("Deviance residuals") + xlab("Linear predictor")
grid.arrange(p1, p2, p3, p4, nrow = 2)
