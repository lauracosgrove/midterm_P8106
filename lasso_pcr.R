#install.packages("glmnet")
library(glmnet)

## Data import
train <- read_csv("Data/train.csv") 
test <- read_csv("Data/test.csv") 

# delete rows containing the missing data 
train <- na.omit(train)
test <- na.omit(test) #from 1066 obs to 344

# matrix of predictors (glmnet uses input matrix) 
x <- model.matrix(heart_disease_mortality_per_100k~.,train)[,-1]
# vector of response
y <- train$heart_disease_mortality_per_100k

#Test data matrix
xTest <- model.matrix(heart_disease_mortality_per_100k~.,test)[,-1]

#Test data response vector
yTest <- test$heart_disease_mortality_per_100k

set.seed(1)

#####################  Lasso  #####################
cv.lasso <- cv.glmnet(x, y, alpha = 1)

best.lambda <- cv.lasso$lambda.min
print(best.lambda)
#[1] 0.393218

plot(cv.lasso)

#Create training model using lasso regression (alpha=1) with best lambda
lasso.model <- glmnet(x, y,
                      alpha = 1,
                      lambda = best.lambda)

#Fit trainning model on test data set*************************************************************************
#pred <- predict(lasso.model, s = best.lambda, newx = xTest)
pred <- predict(lasso.model, s = best.lambda, type="coefficients")
print(pred)

#Calculate accuracy with MSE
MSE <- mean((pred - yTest)^2)
print(MSE)
# [1] _________

#Lasso Non-Zero Coefficients
#Retrieving the lasso coefficients
lasso.coef <- predict(lasso.model,type = "coefficients", s = best.lambda)[1:length(lasso.model$beta),]

#Printing non-zero coefficients
lasso.coef[lasso.coef != 0]
#There are 38 non-zero coefficients

#####################  PCR  #####################

set.seed(1)

pcr.model <- pcr(heart_disease_mortality_per_100k ~ ., 
                 data = train, 
                 scale = TRUE, 
                 validation = "CV")

summary(pcr.model)
#******************************************************************
predy2.pcr <- predict(pcr.model, test, 
                      ncomp = 35)

#Calculate accuracy with MSE
MSE = mean((predy2.pcr - yTest)^2)
print(MSE)
#[1] ________ and M=35
