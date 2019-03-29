Laura\_modeling
================
Laura Cosgrove
3/28/2019

``` r
library(tidyverse)
library(caret)
library(modelr)
library(glmnet)
library(pls)
```

``` r
heart <- read_csv("./data/train_noNA.csv") %>% 
  select(-row_id)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   row_id = col_integer(),
    ##   heart_disease_mortality_per_100k = col_integer(),
    ##   metro_ruccs = col_character(),
    ##   population_ruccs = col_character(),
    ##   urban_influence = col_integer(),
    ##   economic_typology = col_character(),
    ##   demo__birth_rate_per_1k = col_integer(),
    ##   demo__death_rate_per_1k = col_integer(),
    ##   health__air_pollution_particulate_matter = col_integer(),
    ##   health__pop_per_dentist = col_integer(),
    ##   health__pop_per_primary_care_physician = col_integer(),
    ##   yr = col_character(),
    ##   pure_population = col_character(),
    ##   metro_adjacency = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
x <- model.matrix(heart_disease_mortality_per_100k ~ ., data = heart)[,-1]
y <- heart$heart_disease_mortality_per_100k
```

Set up `caret` training control. We will use this for all models.

``` r
set.seed(100)
ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
```

### Ridge

Cross validate to find the optimal lambda value.

``` r
set.seed(100)

ridge_fit <- train(x, y,
                     method = "glmnet",
                     tuneGrid = expand.grid(alpha = 0, 
                                            lambda = exp(seq(-1, 12, length = 200))),
                    #preProc = c("center", "scale"),
                     trControl = ctrl1)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
plot(ridge_fit, xTrans = function(x) log(x)) #in correct range
```

![](laura_model_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
best_lambda_ridge = ridge_fit$bestTune$lambda # lower lambda bc adjusted bounds


#ridge_pred <- predict(ridge_fit$finalModel, s = best_lambda_ridge, newx = x_test)

#ridge_MSE = mean((ridge_pred - y_test)^2) #slightly lower mse

#Glmnet for vizualization #i think this is bad
ridge_cv_glmnet <- cv.glmnet(x, y, 
                      alpha = 0, 
                      lambda = exp(seq(-1, 12, length = 200)), 
                      type.measure = "mse")

plot(ridge_cv_glmnet, xvar = "lambda", label = TRUE)
```

    ## Warning in plot.window(...): "xvar" is not a graphical parameter

    ## Warning in plot.window(...): "label" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "xvar" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "label" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "xvar" is not
    ## a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "label" is not
    ## a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "xvar" is not
    ## a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "label" is not
    ## a graphical parameter

    ## Warning in box(...): "xvar" is not a graphical parameter

    ## Warning in box(...): "label" is not a graphical parameter

    ## Warning in title(...): "xvar" is not a graphical parameter

    ## Warning in title(...): "label" is not a graphical parameter

![](laura_model_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
plotmo::plot_glmnet(ridge_cv_glmnet$glmnet.fit, xvar = "lambda")
```

![](laura_model_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
ridge_cv_table <- tibble(lambda_values = ridge_cv_glmnet$lambda, mse = ridge_cv_glmnet$cvm, mse_upper = ridge_cv_glmnet$cvup, mse_lower = ridge_cv_glmnet$cvlo)

##MSE
ridge_cv_table_best <- ridge_cv_table %>% 
  arrange(mse) %>% 
  filter(lambda_values == best_lambda_ridge)

ridge_mse_plot <- ridge_cv_table %>% 
  filter(lambda_values %in% exp(seq(-1, 4, length = 200))) %>% 
  ggplot(aes(x = log(lambda_values), y = mse)) +
  geom_point() +
  geom_point(aes(x = log(best_lambda_ridge), y = ridge_cv_table_best$mse), color = "red") +
  geom_ribbon(aes(ymin = mse_lower, ymax = mse_upper), alpha = 0.1)
  
ridge_mse_plot + annotate("text", x = 0.5 + log(best_lambda_ridge), y = 985, label = "Lambda chosen by caret", color = "red")
```

![](laura_model_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
#for fun


## Fit a glmnet with besttune from caret

ridge_glmnet <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)

broom::tidy(ridge_glmnet) %>% 
  arrange(abs(estimate)) #see shrunk coefficients with lambda chosen by caret
```

    ## # A tibble: 47 x 5
    ##    term                                     step estimate lambda dev.ratio
    ##    <chr>                                   <dbl>    <dbl>  <dbl>     <dbl>
    ##  1 health__pop_per_primary_care_physician      1 -8.22e-4   4.13     0.715
    ##  2 health__pop_per_dentist                     1  1.03e-3   4.13     0.715
    ##  3 health__motor_vehicle_crash_deaths_per…     1  4.32e-1   4.13     0.715
    ##  4 population_ruccsUrban 2,500-19,999 met…     1 -5.61e-1   4.13     0.715
    ##  5 population_ruccsRural metro non-adjace…     1  9.87e-1   4.13     0.715
    ##  6 pure_population>20,000                      1  1.18e+0   4.13     0.715
    ##  7 demo__birth_rate_per_1k                     1  1.31e+0   4.13     0.715
    ##  8 economic_typologyManufacturing-depende…     1  1.47e+0   4.13     0.715
    ##  9 pure_population2,500 - <20,000              1  1.51e+0   4.13     0.715
    ## 10 pure_population> 1,000,000                  1  1.61e+0   4.13     0.715
    ## # ... with 37 more rows

``` r
predict(ridge_glmnet, s = best_lambda_ridge, type="coefficients") 
```

    ## 47 x 1 sparse Matrix of class "dgCMatrix"
    ##                                                                   1
    ## (Intercept)                                            1.080287e+02
    ## metro_ruccsNonmetro                                    1.948731e+00
    ## population_ruccsMetro > 1 million                      1.867580e+00
    ## population_ruccsMetro 250k - 1 million                -2.491570e+00
    ## population_ruccsRural metro non-adjacent               9.866911e-01
    ## population_ruccsRural metro-adjacent                  -2.210542e+00
    ## population_ruccsUrban >20,000 metro non-adjacent       1.001410e+01
    ## population_ruccsUrban >20,000 metro-adjacent          -2.699436e+00
    ## population_ruccsUrban 2,500-19,999 metro non-adjacent  3.111674e+00
    ## population_ruccsUrban 2,500-19,999 metro-adjacent     -5.612892e-01
    ## urban_influence                                       -1.854359e+00
    ## economic_typologyFederal/State government-dependent    4.154969e+00
    ## economic_typologyManufacturing-dependent               1.474207e+00
    ## economic_typologyMining-dependent                      5.875431e+00
    ## economic_typologyNonspecialized                        1.980021e+00
    ## economic_typologyRecreation                           -7.155487e+00
    ## econ__pct_civilian_labor                              -6.631385e+01
    ## econ__pct_unemployment                                 3.379435e+01
    ## econ__pct_uninsured_adults                             9.234713e+01
    ## econ__pct_uninsured_children                          -9.714977e+01
    ## demo__pct_female                                       9.700622e+01
    ## demo__pct_below_18_years_of_age                       -2.792186e+01
    ## demo__pct_aged_65_years_and_older                     -3.722435e+02
    ## demo__pct_adults_less_than_a_high_school_diploma       3.537473e+01
    ## demo__pct_adults_with_high_school_diploma              3.914648e+01
    ## demo__pct_adults_with_some_college                    -6.080707e+01
    ## demo__pct_adults_bachelors_or_higher                  -2.365987e+01
    ## demo__birth_rate_per_1k                                1.311242e+00
    ## demo__death_rate_per_1k                                7.258627e+00
    ## health__pct_adult_obesity                              1.063543e+02
    ## health__pct_adult_smoking                              4.940033e+01
    ## health__pct_diabetes                                   1.595212e+02
    ## health__pct_low_birthweight                            3.632659e+02
    ## health__pct_physical_inacticity                        2.066635e+02
    ## health__air_pollution_particulate_matter              -1.665635e+00
    ## health__motor_vehicle_crash_deaths_per_100k            4.324451e-01
    ## health__pop_per_dentist                                1.029703e-03
    ## health__pop_per_primary_care_physician                -8.217212e-04
    ## yrb                                                   -4.523900e+00
    ## demo__pct_nonwhite                                    -2.609795e+01
    ## pure_population< 250,000                              -3.385788e+00
    ## pure_population> 1,000,000                             1.613758e+00
    ## pure_population>20,000                                 1.178547e+00
    ## pure_population2,500 - <20,000                         1.507804e+00
    ## pure_population250,000 - 1,000,000                    -2.781098e+00
    ## metro_adjacencymetro                                  -2.350506e+00
    ## metro_adjacencynonadjacent                             4.908417e+00

Our best lambda is 4.1250801, but it's not an entirely stable value because RMSE does not differ substantially in the range; that is, glm chooses a different value. I think we should proceed with the lambda chosen by caret because of its more stringent shrinking power. With that, our MSE is:

``` r
ridge_cv_table %>% 
  arrange(mse) %>% 
  filter(lambda_values == best_lambda_ridge) %>% 
  knitr::kable()
```

|  lambda\_values|       mse|  mse\_upper|  mse\_lower|
|---------------:|---------:|-----------:|-----------:|
|         4.12508|  1025.875|    1069.112|    982.6371|
