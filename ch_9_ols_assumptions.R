library(here)
library(hablar)
library(tidyverse)
library(janitor)
library(car)

      # Calculating VIF - variance inflation factor
# resource: https://www.statology.org/variance-inflation-factor-r/

# interpreting values:

# A value of 1 indicates there is no correlation between a given predictor variable 
  # and any other predictor variables in the model.
# A value between 1 and 5 indicates moderate correlation between a given predictor 
  # variable and other predictor variables in the model, but this is often not severe 
  # enough to require attention.
# A value greater than 5 indicates potentially severe correlation between a given 
  # predictor variable and other predictor variables in the model. In this case, the 
  # coefficient estimates and p-values in the regression output are likely unreliable.

multi_col_ex <- read_csv(here("MulticollinearityExample.csv")) %>% clean_names()

# DV is bone mineral dnsity of femoral neck
names(multi_col_ex)

multi_mod <- lm(femoral_neck ~ percent_fat + weight_kg +
                  activity + percent_fat*weight_kg, multi_col_ex)

summary(multi_mod)
vif(multi_mod)


# center to remove structural multicollinearity

multi_scale <- scale(multi_col_ex, scale = FALSE) %>% as.data.frame()
scale_mod <- lm(femoral_neck ~ percent_fat + weight_kg +
                  activity + percent_fat*weight_kg, multi_scale)

vif(scale_mod)
summary(scale_mod)


