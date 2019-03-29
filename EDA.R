#install.packages('corrplot')
library(tidyverse)
library(readr)
library(corrplot)

## Data import
train <- read_csv("Data/train.csv") 

# delete rows containing the missing data 
train1 <- na.omit(train)
#this went from 2082 obs to 709

#******Bar graphs for categorical variables**********
#bar graph by metro
ggplot(data = train) +
  geom_bar(mapping = aes(x = metro))

#bar graph by population
ggplot(data = train) +
  geom_bar(mapping = aes(x = population))

#bar graph by area urban influence
ggplot(data = train) +
  geom_bar(mapping = aes(x = area__urban_influence))

#bar graph by econ typology
ggplot(data = train) +
  geom_bar(mapping = aes(x = econ__economic_typology))


#******Bar graphs for continuous variables**********
#outcome
ggplot(data = train) +
  geom_histogram(mapping = aes(x = heart_disease_mortality_per_100k), binwidth = 10)

ggplot(data = train) +
  geom_histogram(mapping = aes(x = econ__pct_unemployment), binwidth = 0.005)

ggplot(data = train) +
  geom_histogram(mapping = aes(x = econ__pct_uninsured_adults), binwidth = 0.01)

#gender skewed
ggplot(data = train) +
  geom_histogram(mapping = aes(x = demo__pct_female), binwidth = 0.01)



#******2 variables (outcome and categorical predictor)**********
#metro vs non-metro (more heart disease in nonmetro)
ggplot(data = train, mapping = aes(x = heart_disease_mortality_per_100k, colour = metro)) +
  geom_freqpoly(binwidth = 20)

#economic typology
ggplot(data = train, mapping = aes(x = heart_disease_mortality_per_100k, colour = econ__economic_typology)) +
  geom_freqpoly(binwidth = 20)


#******scatter plots**********
#plotting heart disease mortality against pct unemployment
ggplot(data = train, mapping = aes(x = econ__pct_unemployment, y = heart_disease_mortality_per_100k)) + 
  geom_point()

#plotting heart disease mortality against pct pct uninsured adults
ggplot(data = train, mapping = aes(x = econ__pct_uninsured_adults, y = heart_disease_mortality_per_100k)) + 
  geom_point()

#65 years and older: would've assumed this would be more linear
ggplot(data = train, mapping = aes(x = demo__pct_aged_65_years_and_older, y = heart_disease_mortality_per_100k)) + 
  geom_point()

#looking at education
ggplot(data = train, mapping = aes(x = demo__pct_adults_less_than_a_high_school_diploma, y = heart_disease_mortality_per_100k)) + 
  geom_point()
ggplot(data = train, mapping = aes(x = demo__pct_adults_bachelors_or_higher, y = heart_disease_mortality_per_100k)) + 
  geom_point()

#obesity and diabetes: too colinear?
ggplot(data = train, mapping = aes(x = health__pct_adult_obesity, y = health__pct_diabetes)) + 
  geom_point()




#boxplots
ggplot(data = train, mapping = aes(x = econ__economic_typology, y = heart_disease_mortality_per_100k)) +
  geom_boxplot()

ggplot(data = train, mapping = aes(x = econ__economic_typology, y = econ__pct_unemployment)) +
  geom_boxplot()



# matrix of predictors (glmnet uses input matrix) 
x <- model.matrix(heart_disease_mortality_per_100k~.,train)[,-1]
# vector of response
y <- train$heart_disease_mortality_per_100k

corrplot(cor(x))

cor(econ__pct_unemployment,heart_disease_mortality_per_100k)
