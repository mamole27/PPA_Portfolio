# 🏆 Learning- Focused In-Class Challenge: *Best Predictive Model Competition*
  
# The Task **Predict median home value** (`B25077_001`) for PA counties using any combination of predictors

# Build the model with **lowest 10-fold cross-validated RMSE**
  
  ## Available Predictors


#clear environment
rm(list = ls()) 

#copy from lecture file
#| eval: false
#| echo: true
challenge_data_sample <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

#pa county: med income and total pop
pa_popincome <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001"
  ),
  year = 2022,
  output = "wide"
)
#visualize
# Visualize the relationship
ggplot(pa_popincome, aes(x = total_popE, y = median_incomeE)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Population vs Median Income in PA Counties",
    x = "Total Population", 
    y = "Median Household Income"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  theme_minimal()


#PREDICT MEDIAN HOME VALUE 

# Multiple regression
model2 <- lm(median_incomeE ~ total_popE + percent_collegeE + poverty_rateE,
             data = challenge_data_sample)

summary(model2)


#try poverty rate as sole variable
pa_poverty <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001",
    poverty_rate = "B17001_002"
  ),
  year = 2022,
  output = "wide"
)
poverty_lm <- lm(median_incomeE ~ total_popE + poverty_rateE,
             data = pa_poverty)
summary(poverty_lm)
# Visualize the relationship
ggplot(pa_poverty, aes(x = poverty_rateE, y = median_incomeE)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Poverty vs Median Income in PA Counties",
    x = "Poverty Rate", 
    y = "Median Household Income"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  theme_minimal()

#try med income, med age, total pop, home value
pa_income_age_pop_home <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001"      # Median age
  ),
  year = 2022,
  output = "wide"
)

#sample
model1 <- lm(median_incomeE ~ total_popE, data = challenge_data_sample)
summary(model1)


#poverty
pov_lm <- lm(home_valueE ~ total_popE + poverty_rateE, data = challenge_data_sample)
summary(pov_lm)
#plot
ggplot(pov_lm, aes(x = poverty_rateE, y = home_valueE)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Poverty vs Home Value in PA Counties",
    x = "Poverty Rate", 
    y = "Median Home Value"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar) +
  theme_minimal()




## Train/Test Split

**Solution:** Hold out some data to test predictions

```{r}
#| echo: true
#| eval: true
set.seed(123)
n <- nrow(challenge_data_sample)

# 70% training, 30% testing
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- challenge_data_sample[train_indices, ]
test_data <- challenge_data_sample[-train_indices, ]

# Fit on training data only
model_train <- lm(median_incomeE ~ total_popE, data = train_data)

# Predict on test data
test_predictions <- predict(model_train, newdata = test_data)
```

---
  
  ## Evaluate Predictions
  
  ```{r}
#| echo: true
#| eval: true
# Calculate prediction error (RMSE)
rmse_test <- sqrt(mean((test_data$median_incomeE - test_predictions)^2))
rmse_train <- summary(model_train)$sigma

cat("Training RMSE:", round(rmse_train, 0), "\n")
cat("Test RMSE:", round(rmse_test, 0), "\n")

install.packages("caret")
library(caret)

# 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

cv_model <- train(median_incomeE ~ total_popE,
                  data = pa_data,
                  method = "lm",
                 trControl = train_control)

cv_model$results


library(car)
install.packages("car")
