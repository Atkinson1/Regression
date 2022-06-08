library(here)
library(hablar)
library(tidyverse)
library(janitor)
library(semEff) # contains R2 function, which gives predicted R-2
        # and regular R-2 and adjusted R-2

# references:
  # https://www.statology.org/prediction-interval-r/

bmi_pred <- read_csv(here("Predict_BMI.csv")) %>% clean_names()

# goal: predict body fat percentage based on BMI
bmi_pred

model <- lm(percent_fat ~ bmi, bmi_pred)

plot(model)
summary(model)
# Adjusted R-squared:  0.7412

ggplot(bmi_pred, aes(x = bmi, y = percent_fat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(bmi_pred, aes(x = bmi, y = percent_fat)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(bmi_pred, aes(x = I(bmi^2), y = percent_fat)) +
  geom_point() +
  geom_smooth(se = FALSE)

# after plotting, changing bmi in model to be quadratic; leads to better fit
# Adjusted R-squared:  0.7552
model_sq <- lm(percent_fat ~ bmi + I(bmi^2), bmi_pred)
summary(model_sq)

R2(model)

# predicting a dv (in this case, body fat percentage)
  # with an IV value (in this case, BMI) set to 18
  # also includes prediction interval

# note: have to set iv name to be same as in model
df <- tibble(bmi = c(18))

# CI tells us confidence of a mean being b/t two values
predict(model, df, interval = "confidence")
# PI tells us confidence of range a value can take given some IV
predict(model, df, interval = "predict")


# simp_reg_prec <- read_csv(here("SimpleRegressionPrecision.csv")) %>%
#   clean_names()

mult_reg_prec <- read_csv(here("MultipleRegressionPrecision.csv")) %>%
  clean_names()

model_spec <- lm(temperature ~ pressure + fuel_rate, mult_reg_prec)
summary(model_spec)

# for pressure of 36, and fuel rate of 17.5
df <- tibble(pressure = 36, fuel_rate = 17.5)

# the limits of a two-sided 90% prediction interval is
# equivalent to a 95% confidence interval bounds
predict(model_spec, df, interval = "predict", level = .90)

# for example above, being used to show entering values 
# and allows us to be 95% confident upper value will not
# go above 250 (in this case, upper is 248.2738)

      # plotting prediction intervals

model_preds <- predict(model_spec, interval = "predict")

all_data <- cbind(mult_reg_prec, model_preds) # binds predictions/fitted values to the original observed/actual values

ggplot(all_data, aes(x = fuel_rate, y = temperature)) +
  geom_point() +
  geom_smooth(method = lm) + # gives CIs
  geom_line(aes(y = lwr), color = "coral2", linetype = "dashed") + # lower PI
  geom_line(aes(y = upr), color = "coral2", linetype = "dashed") # upper PI
