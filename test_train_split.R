library(tidyverse)
library(readr)

## Data import
predictors <- read_csv("data/Training_values.csv") 

response <- read_csv("data/Training_labels.csv") 

data <- response %>% 
  full_join(predictors, by = "row_id")

## Manipulation
data <- data %>%
  separate(col = area__rucc, into = c('metro', 'population'), sep = ' - ')


## training/test data
set.seed(1)
train_ind <- sample(seq_len(nrow(data)), size = 2/3*nrow(data))

train <- data[train_ind, ]
test <- data[-train_ind, ]

write.csv(train, file = './data/train.csv', row.names = F)
write.csv(test, file = './data/test.csv', row.names = F)
