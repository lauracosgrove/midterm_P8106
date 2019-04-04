library(tidyverse)
library(readr)

## Data import
predictors <- read_csv("./data/Training_values.csv") 
response <- read_csv("./data/Training_labels.csv") 


## Manipulation
data <- response %>% 
  full_join(predictors, by = "row_id") %>%
  separate(col = area__rucc, into = c('metro', 'population'), sep = ' - ') %>%
  rename(urban_influence = area__urban_influence,
         economic_typology = econ__economic_typology) %>%
  mutate(pure_population = fct_collapse(as.factor(population), 
                                        "more than 1,000,000" = "Counties in metro areas of 1 million population or more",
                                        "250,000 - 1,000,000" = "Counties in metro areas of 250,000 to 1 million population",
                                        "less than 250,000" = "Counties in metro areas of fewer than 250,000 population",
                                        "more than 20,000" = c("Urban population of 20,000 or more, adjacent to a metro area", 
                                                               "Urban population of 20,000 or more, not adjacent to a metro area"),
                                        "2,500 - 20,000" = c("Urban population of 2,500 to 19,999, adjacent to a metro area", 
                                                             "Urban population of 2,500 to 19,999, not adjacent to a metro area"),
                                        "0 - 2,500" = c("Completely rural or less than 2,500 urban population, adjacent to a metro area", 
                                                        "Completely rural or less than 2,500 urban population, not adjacent to a metro area")),
         economic_typology = as.factor(recode(economic_typology,
                                              "Nonspecialized" = "Nonspecialized",
                                              "Manufacturing-dependent" = "Manufacturing",
                                              "Farm-dependent" = "Farming",
                                              "Federal/State government-dependent" = "Government",
                                              "Mining-dependent" = "Mining",
                                              "Recreation" = "Recreation")),
        metro = factor(metro, 
                       levels = c("Metro", "Nonmetro")),
        urban_influence = as.factor(urban_influence),
        demo__pct_nonwhite = demo__pct_hispanic + demo__pct_asian + demo__pct_american_indian_or_alaskan_native + demo__pct_non_hispanic_african_american,
        urban_influence = fct_rev(urban_influence),
        metro_adjacency = fct_collapse(population, 
                                       metro = c("Counties in metro areas of 1 million population or more", 
                                                 "Counties in metro areas of 250,000 to 1 million population", 
                                                 "Counties in metro areas of fewer than 250,000 population"),
                                       adjacent = c("Urban population of 20,000 or more, adjacent to a metro area", 
                                                    "Urban population of 2,500 to 19,999, adjacent to a metro area", 
                                                    "Completely rural or less than 2,500 urban population, adjacent to a metro area"),
                                       nonadjacent = c("Urban population of 20,000 or more, not adjacent to a metro area", 
                                                       "Urban population of 2,500 to 19,999, not adjacent to a metro area", 
                                                       "Completely rural or less than 2,500 urban population, not adjacent to a metro area"))) %>% 
  dplyr::select(-demo__pct_hispanic, 
                -demo__pct_asian,
                -demo__pct_american_indian_or_alaskan_native, 
                -demo__pct_non_hispanic_african_american,
                -demo__pct_non_hispanic_white,
                -health__homicides_per_100k, # >90% missing
                -health__pct_excessive_drinking, # >90% missing
                -yr,
                -population)


## training/test data
set.seed(1)
train_ind <- sample(seq_len(nrow(data)), size = 2/3*nrow(data)) # select rows in 2:1 ratio 

train <- data[train_ind, ] # training dataset
test <- data[-train_ind, ] # testing dataset

# Imputation for missing values with caret, based on training data
training_preproc = caret::preProcess(train, 
                                     method = "knnImpute", # automatically centers and scales data
                                     pcaComp = 10,
                                     na.remove = TRUE,
                                     k = 5,
                                     knnSummary = mean,
                                     outcome = NULL,
                                     fudge = .2,
                                     numUnique = 3,
                                     verbose = TRUE)

# Impute training imputation on both training and testing datasets
train_imputed = predict(training_preproc, train)
test_imputed = predict(training_preproc, test)


# save files to data folder
write.csv(train, file = './data/train_before_imputed.csv', row.names = F)
write.csv(test, file = './data/test_before_imputed.csv', row.names = F)
write.csv(train_imputed, file = './data/train_imputed.csv', row.names = F)
write.csv(test_imputed, file = './data/test_imputed.csv', row.names = F)
write.csv(data, file = './data/full_processed_data.csv', row.names = F)
saveRDS(training_preproc, 'training_preproc.rds') # save imputation call in case we need it when we resample later on
