#Import the “lalonde” dataset. We will be using it for the following exercises. You can do so by calling the “Matching” library.

library(Matching)
data("lalonde")

#1. Build a linear model (lm) that uses educ to predict age. Plot education in the x-axis and age in the y-axis. Also include the linear model in the same graph. 

model <- lm(age ~ educ, data = lalonde)
model

plot(lalonde$educ, lalonde$age)
abline(lm(age ~ educ, data = lalonde), col="blue")

#2. Write a root mean square error (RMSE) function that takes observed y values and a linear model as inputs. 

mean((lalonde$age - predict(model))^2) # MSE = 50.27419
sqrt(mean((lalonde$age - predict(model))^2)) # RMSE = 7.09043

mean(model$residuals^2) # MSE = 50.27419
sqrt(mean(model$residuals^2)) # RMSE = 7.09043

sqrt(mean(model$residuals^2)) #7.09043

#3. Write a R-squared (R2) function that takes observed y values and a linear model as inputs

summary(model)$r.squared #0.0005273602
rsq <- 1 - sum(model$residuals^2)/sum((lalonde$age - mean(lalonde$age))^2)
rsq #0.0005273602
  
#4. Build a generalized linear model (glm) to predict treat with all the other variables. 
# Print the predicted "treat" values. These predicted values are called propensity score, which is an important concept later on. 
 
fit <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75 + re78 + u74 + u75, data = lalonde)
fit
pred <- predict(fit)
pred # these are propensity scores
