project
================
Laura Cosgrove
3/26/2019

Introduction
------------

### Background (Alyssa)

### Exploratory Data Analysis (Laura)

Linear Models
-------------

### Stepwise Selection (Alyssa)

### Lasso (Charlotte)

### Ridge (Laura)

### PCR (Charlotte)

Nonlinear Models
----------------

### GAM (Alyssa)

### MARS (Laura)

Classification Setting?
-----------------------

### KNN?

Conclusions
-----------

### Training RMSE

``` r
set.seed(100)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
lm_fit <- readRDS("lm_step.rds")
ridge_fit <- readRDS("ridge.rds")
mars_fit <- readRDS("mars.rds")
gam_fit <- readRDS("gam_fit.rds")
lasso_fit <- readRDS("lasso.rds")
pcr_fit <- readRDS("pcr.rds")

res <- resamples(list(
  Stepwise = lm_fit,
  Ridge = ridge_fit,
  Lasso = lasso_fit,
  PCR = pcr_fit,
  GAM = gam_fit,
  MARS = mars_fit
  ))


ggplot(res, metric = "RMSE") +
  theme_minimal() +
  labs(title = "Resampled Training RMSE")
```

![](project_files/figure-markdown_github/unnamed-chunk-1-1.png)

The more flexible model

### Test RMSE (Alyssa)

\*\*note: transform test data by centering and scaling with TRAINING means and standard deviations. Does predict do this automatically when using `preProcess` in caret?

### Interpretations (Laura)

*Coefficient Shrinkage: Lasso and Ridge*

*Investigation of MARS*

The minimum generalized cross-validation error was achieved

*Investigation of GAM*
