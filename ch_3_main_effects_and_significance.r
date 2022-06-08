library(here)
library(tidyverse)
library(tidymodels)
library(janitor) # clean variable names
library(skimr)
library(glimpse)
library(Hmisc) #rcorr()
library(corrplot) # creates correlation plot
library(hablar)

height_weight <- read_csv(here("HeightWeight.csv")) %>% clean_names()

regress_htow <- lm(weight_kg ~ height_m, height_weight)

    # summary() will give summary of model (p-values, beta(Estimate), R^2, etc.)
summary(regress_htow) 

# height_m coefficient is 106.5; represents the mean increase of weight in kgs for every additional
# one meter in height.

      # applying confint to find confidence interval of values for our model
confint(regress_htow, level = .95) 

par(mfrow = c(2,2)) # puts plots in 2x2 setup
plot(regress_htow) # will plot 4 plots

# Residuals vs Fitted: tests normality of residuals (want a flat horizontal line);
# Normal Q-Q: 
# Scale-Location: 
# Residuals vs Leverage: plots outliers

      # plotting with ggplot2
ggplot(regress_htow, aes(x = height_m, y = weight_kg)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point()

# read in data, reorder factor levels
cat <- read_csv(here("Categorical_Example.csv")) %>% clean_names()
cat$major <- factor(cat$major, levels = c("Statistics", "Political Science", "Psychology"))

cat$major

# income differences by major while holding experience constant
reg <- lm(income ~ major + experience, cat)
summary(reg)
# gives p-value for experience and each major; doesn't give p-value of categorical variable as a whole
  # the negative coefficients on major indicate these majors (Poli Sci/Psych) have lower mean incomes
  # than Statistics
      # ex: -27195 estimate for poli sci means mean income for poli sci majors $27,195 less than 
      # mean income for statistics majors
      # ex: experience estimate of 5,085 means for each one-year increase, mean income increases by
      # $5,085 while holding major constant.





















































