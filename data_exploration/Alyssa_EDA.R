library(tidyverse)
library(caret) # only for plot
library(splines)
library(mgcv)
library(patchwork)

train <- readr::read_csv("data/train.csv")

table(train$econ__economic_typology)

# x = train %>% select(-heart_disease_mortality_per_100k)
# y = train$heart_disease_mortality_per_100k

train %>% skimr::skim()

# Looking at the distributions of some variables, I see that the demographic variables for percent of hispanic and other smaller values have a lot of values close to 0. I am going to combine vars to create "percent white" and "percent non-white" categories.
# Additionally, 61% of rows have missing information about homicides per 100k

train <- train %>%
  mutate(demo__pct_nonwhite = demo__pct_hispanic + 
           demo__pct_asian + 
           demo__pct_american_indian_or_alaskan_native + 
           demo__pct_non_hispanic_african_american) %>%
  select(-demo__pct_hispanic, 
           -demo__pct_asian,
           -demo__pct_american_indian_or_alaskan_native, 
           -demo__pct_non_hispanic_african_american)


hist(train$pct_nonwhite) # still skewed but no 0 percentages - does this matter?

cor(train[, c(8:31, 33)], use = "complete.obs") # correlation of econ vars
train %>% select(starts_with("demo__pct")) %>% pairs
train %>% select(starts_with("econ__pct")) %>% pairs
train %>% select(starts_with("health__pct")) %>% pairs

pairs(train[, c(8:31, 33)], use = "complete.obs")
# Health outcomes appear to be notably correlated with education, employment/insuarance, and race
# high corr between bachelors and other education levels
# pct uninsured children & adults highly correlated
# almost perfect correlation with pct_non_hispanic_white - only need one in model
