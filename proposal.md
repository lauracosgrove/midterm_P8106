Project Proposal
================
Laura Cosgrove (lec2197), Charlotte Abrams (cea2151), Alyssa Vanderbeek (amv2187)
3/17/2019

Project Title
-------------

Predicting Heart Disease in U.S. Counties

Data Sources (Charlotte)
------------------------
This data was taken from https://www.kaggle.com/nandvard/microsoft-data-science-capstone/version/1. Although it is unclear how the data was compiled, we know the sources from which the different variables came from.

1.  Area:
    - USDA Economic Research Service, https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/ 
    - USDA Economic Research Service, https://www.ers.usda.gov/data-products/urban-influence-codes/
  
2.  Economics:
    - USDA Economic Research Service, https://www.ers.usda.gov/data-products/county-typology-codes.aspx
    - Bureau of Labor Statistics, http://www.bls.gov/lau/
    
3.  Demographics:
    - US Census Population Estimates
    - American Community Survey
   
4.  Health:
    - National Center for Chronic Disease Prevention and Health Promotion
    - Behavioral Risk Factor Surveillance System
    - Division of Diabetes Translation
    - National Center for Health Statistics
    - CDC WONDER, https://wonder.cdc.gov/wonder/help/pm.html
    - HRSA Area Resource File

Description of Data (Alyssa)
----------------------------

This data provides county-level information. The response of interest is the heart disease mortality rate per 100k.

Predictors are given in 4 categories:

1.  Area - specifying whether the county is metro/nonmetro, general population size, and whether the non metro area is adjacent to a metro area. More information about the classifications given is found at the US Department of Agriculture website.

2.  Economics - these variables give data regarding economic demographics of the population at the county level. These include percent unemployment, percent without health insurance, and the central economic typology of the area (e.g. nonspecialized, farming, etc).

3.  Demographics - Racial, education, and birth/death rates per 1k.

4.  Health - health-related information such as percent obese, percent smoking, percent low birthweight, homicides per 100k, access to primary care and dentistry, and air pollution.

Planned Analyses (Laura)
------------------------

*Part 1: Linear Regression and its Relatives* We plan to fit a ordinary least squares model using backward stepwise regression, a lasso model, a ridge model, and a PCR model to these data. We will select the lasso, ridge, and PCR parameters using repeated cross-validation. The model performance will be compared using the training RMSE.

*Part 2: Nonlinear Regression* We will conduct a similar analysis using nonlinear methods. We will fit a generalized additive model, a multivariate adaptive regression spline model (MARS), and k-nearest neighbors, if we detect a potential nonlinear trend in the outcome variable, with parameters chosen by cross-validation. The training RMSE will be compared among and between the nonlinear and linear models.

*Part 3: Model Comparison*: At the start, we randomly partitioned the data into a training and test set (2:1). We will compare the performance of the nonlinear models vs. the linear models using the test RMSE.
