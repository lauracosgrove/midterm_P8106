library(tidyverse)
library(readr)

## Data import
predictors <- read_csv("./data/Training_values.csv") 

response <- read_csv("./data/Training_labels.csv") 

data <- response %>% 
  full_join(predictors, by = "row_id")

## Manipulation
data <- data %>%
  separate(col = area__rucc, into = c('metro', 'population'), sep = ' - ')

###Factor Manipulation
data <- data %>% 
  mutate(population = factor(population, levels = c(
    "Counties in metro areas of 1 million population or more", 
    "Counties in metro areas of 250,000 to 1 million population", 
    "Counties in metro areas of fewer than 250,000 population", 
    "Urban population of 20,000 or more, adjacent to a metro area", 
    "Urban population of 20,000 or more, not adjacent to a metro area", 
    "Urban population of 2,500 to 19,999, adjacent to a metro area", 
    "Urban population of 2,500 to 19,999, not adjacent to a metro area", 
    "Completely rural or less than 2,500 urban population, adjacent to a metro area", 
    "Completely rural or less than 2,500 urban population, not adjacent to a metro area"),
    labels = c("Metro > 1 million", "Metro 250k - 1 million", "Metro <250k", "Urban >20,000 metro-adjacent", "Urban >20,000 metro non-adjacent","Urban 2,500-19,999 metro-adjacent", "Urban 2,500-19,999 metro non-adjacent", "Rural metro-adjacent", "Rural metro non-adjacent")),
    metro = factor(metro, levels = c("Metro", "Nonmetro")),
    area__urban_influence = factor(area__urban_influence, levels = c(
      "Large-in a metro area with at least 1 million residents or more", 
      "Small-in a metro area with fewer than 1 million residents",
      "Micropolitan adjacent to a large metro area",
      "Noncore adjacent to a large metro area",
      "Micropolitan adjacent to a small metro area",
      "Noncore adjacent to a small metro with town of at least 2,500 residents",
      "Noncore adjacent to a small metro and does not contain a town of at least 2,500 residents",
      "Micropolitan not adjacent to a metro area",
      "Noncore adjacent to micro area and contains a town of 2,500-19,999 residents",
      "Noncore adjacent to micro area and does not contain a town of at least 2,500 residents",
      "Noncore not adjacent to a metro/micro area and contains a town of 2,500  or more residents",
      "Noncore not adjacent to a metro/micro area and does not contain a town of at least 2,500 residents"),
      labels = 1:12)) %>% 
  mutate(area__urban_influence = fct_rev(area__urban_influence)) %>% 
  rename(metro_ruccs = metro, 
         population_ruccs = population,
         urban_influence = area__urban_influence)

data <- data %>% 
  rename(economic_typology = econ__economic_typology) %>% 
  mutate(economic_typology = factor(economic_typology),
         economic_typology = fct_inorder(economic_typology)) 

data <- data %>% 
  mutate(pure_population = fct_collapse(population_ruccs, "> 1,000,000" = "Metro > 1 million",
                                        "250,000 - 1,000,000" = "Metro 250k - 1 million",
                                        "< 250,000" = "Metro <250k",
                                        ">20,000" = c("Urban >20,000 metro-adjacent", "Urban >20,000 metro non-adjacent"),
                                        "2,500 - <20,000" = c("Urban 2,500-19,999 metro-adjacent", "Urban 2,500-19,999 metro non-adjacent"),
                                        "< 2,500" = c("Rural metro-adjacent", "Rural metro non-adjacent")),
         metro_adjacency = fct_collapse(population_ruccs, metro = c("Metro > 1 million", "Metro 250k - 1 million", "Metro <250k"),
                                        adjacent = c("Urban >20,000 metro-adjacent", "Urban 2,500-19,999 metro-adjacent", "Rural metro-adjacent"),
                                        nonadjacent = c("Urban >20,000 metro non-adjacent", "Urban 2,500-19,999 metro non-adjacent", "Rural metro non-adjacent")))

### Removing variables with NA Values
data <- data %>% 
  select(-health__homicides_per_100k, 
         -health__pct_excessive_drinking)
#two missingest variables


## training/test data
set.seed(1)
train_ind <- sample(seq_len(nrow(data)), size = 2/3*nrow(data))

train <- data[train_ind, ]
test <- data[-train_ind, ]

##Drop NA columns for train
train <- train %>% 
  drop_na()
# 2132 --> 1606

write.csv(train, file = './data/train.csv', row.names = F)
write.csv(test, file = './data/test.csv', row.names = F)
