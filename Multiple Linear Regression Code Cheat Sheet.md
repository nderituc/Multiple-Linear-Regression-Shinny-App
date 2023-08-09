# Multiple Linear Regression Code Cheat Sheet
## Load Required Libraries
```
#R code
# Load the necessary libraries
library(tidyverse)
library(tidymodels)
library(car) 
```
## Data Preparation
Load your dataset (in CSV format) and split it into predictor variables (X) and the response variable (Y). Replace "data.csv" with your actual data file. Use str(data) to examine the structure of the dataset.
```
#R code
# Load your dataset (replace 'data.csv' with your file)
data <- read.csv("data.csv")

# Check the structure of your dataset
str(data)

# Split data into predictors (X) and response (Y)
X <- data[, c("x1", "x2", "x3")]  # Select predictor variables
Y <- data$y                       # Select response variable
```
## Fit Multiple Linear Regression Model
Creating a Linear Regression Model Specification: Create a model specification using the linear_reg() function from the parsnip package. A model specification defines the type of model you want to create and specifies its settings. Here, you are specifying a linear regression model for regression analysis.

Setting Mode and Engine: Use the %>% operator (pipe operator) to chain function calls. The set_mode("regression") sets the mode of the model to "regression," indicating that you are performing a regression task. The set_engine("lm") sets the engine for the model to "lm," which means you are using the linear regression function (lm()) from the base R stats package.

Displaying the Model Specification: The lm_spec object now holds the specifications for the linear regression model you're creating. Printing lm_spec displays the details of the model specification, showing that it's set for regression analysis using the linear regression engine.
Fitting the Linear Regression Model: Here, you use the lm_spec model specification to fit a linear regression model. The %>% operator pipes the model specification into the fit() function. The formula Y ~ x1 + x2 + x3 specifies the model's structure, where Y is the dependent variable, and x1, x2, x3 represents the independent variables. Replace explanatory and response with the actual column names from your dataset. The data = data argument specifies the dataset to use for model fitting.

Displaying a Tidy Summary: After fitting the model, use the tidy() function from the broom package to obtain a tidy summary of the model. The tidy summary includes coefficient estimates, standard errors, t-values, and p-values for each predictor variable in the model. This provides a structured and readable presentation of the model results.
```
#R code
# Create a linear regression model specification
lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# Display the linear regression model specification
lm_spec
# Fit the linear regression model
model<- lm_spec %>% 
  fit(Y ~ x1 + x2 + x3, data = data)

# Display a tidy summary of the model
tidy(model)

```
## Model Evaluation
Evaluate the goodness of fit of the multiple linear regression model using the R-squared value and the adjusted R-squared value.

R-squared (Coefficient of Determination): R-squared is a statistical measure that indicates the proportion of the variance in the dependent variable (response variable, Y) that is explained by the independent variables (predictor variables, X) in the model. It quantifies the "fit" of the model to the data. R-squared ranges from 0 to 1, where:

R-squared = 0: The model does not explain any variability in the response variable.
R-squared = 1: The model perfectly explains the variability in the response variable.
However, a high R-squared value does not necessarily mean that the model is a good fit. A high R-squared can be achieved by adding more predictors, but this may lead to overfitting if the additional predictors are not truly meaningful.

Adjusted R-squared: Adjusted R-squared takes into account the number of predictors in the model. It penalizes the addition of unnecessary predictors, which helps prevent overfitting. Adjusted R-squared is particularly useful when comparing models with different numbers of predictors. It provides a more accurate assessment of the model's explanatory power.
```
#R code
# R-squared value (proportion of variance explained by the model)
rsquared <- summary(model)$r.squared
cat("R-squared:", rsquared, "\n")

# Adjusted R-squared value (accounts for number of predictors)
adj_rsquared <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_rsquared, "\n")
```
## Coefficient Interpretation
Obtain and print a summary of the model's coefficients, including their estimates, standard errors, t-values, and p-values. This information helps interpret the impact of each predictor variable on the response variable.
```
#R code
# Display coefficients with their standard errors and p-values
coef_summary <- summary(model)$coefficients
print(coef_summary)
```
## Predictions
create a new dataset new_data with predictor values for which we want to make predictions. The predict() function is then used to generate predicted response values based on the model and the new data.
```
#R code
# Make predictions using the model
new_data <- data.frame(x1 = c(10, 20, 30), x2 = c(5, 8, 12), x3 = c(2, 3, 4))
predictions <- predict(model, newdata = new_data)
print(predictions)
```
## Residual Analysis
Residual analysis helps assess the model's assumptions. The three plots displayed using the plot() function are: a residual plot (checking for linearity and homoscedasticity), a normality Q-Q plot (checking for normality of residuals), and a scale-location plot (checking for homoscedasticity again).
Residual Plot (which = 1): This plot helps us check for two main assumptions:

Linearity: If the residuals are randomly scattered around the horizontal line at zero, it suggests that the linearity assumption is met. However, if we see a distinct pattern (e.g., a curved trend), it may indicate that the model is not capturing the underlying relationship properly.
Homoscedasticity: Homoscedasticity means that the variability of residuals should be roughly constant across all levels of the predicted values. If the spread of residuals changes systematically as predicted values increase or decrease, it suggests heteroscedasticity, which violates the assumption.
Normality Q-Q Plot (which = 2): This plot is used to assess the normality assumption of residuals. A quantile-quantile (Q-Q) plot compares the distribution of the residuals to the expected normal distribution. If the points follow the diagonal line closely, it suggests that the residuals are normally distributed. Deviations from the diagonal line indicate departures from normality.

Scale-Location Plot (which = 3): Also known as the spread-location plot, this plot helps us re-evaluate the homoscedasticity assumption. It plots the square root of the standardized residuals against the fitted values. Ideally, points should be randomly scattered with relatively constant spread, indicating homoscedasticity. If the spread changes systematically, it suggests heteroscedasticity.
```
#R code
# Residual plot
plot(model, which = 1)

# Normality Q-Q plot
plot(model, which = 2)

# Scale-location plot
plot(model, which = 3)
```
## Model Assumptions
Test some assumptions of the multiple linear regression model. 
Test for Multicollinearity (Variance Inflation Factor - VIF): Multicollinearity occurs when predictor variables are highly correlated with each other. This can lead to instability in coefficient estimates and decreased interpretability. The Variance Inflation Factor (VIF) quantifies the extent of multicollinearity. VIF values greater than 5 or 10 (depending on the context) may indicate multicollinearity. Lower VIF values are preferred as they suggest lower correlation among predictor variables.

Test for Heteroscedasticity (Breusch-Pagan Test): Heteroscedasticity refers to the unequal spread of residuals across different levels of the predicted values. The Breusch-Pagan test is used to formally test for heteroscedasticity. If the p-value of the test is below a certain significance level (e.g., 0.05), it indicates evidence of heteroscedasticity. In such cases, the assumption of constant variance is violated, and model adjustments may be needed.

Test for Normality of Residuals (Shapiro-Wilk Test): The Shapiro-Wilk test is a statistical test used to assess the normality of the distribution of residuals. If the p-value of the test is less than a chosen significance level (e.g., 0.05), it suggests that the residuals deviate significantly from a normal distribution. Departures from normality could impact the validity of statistical inferences drawn from the model.
```
#R code
# Test for multicollinearity (Variance Inflation Factor)
library(car)
vif(model)

# Test for heteroscedasticity (Breusch-Pagan test)
bptest(model)

# Test for normality of residuals (Shapiro-Wilk test)
shapiro.test(residuals(model))
```




