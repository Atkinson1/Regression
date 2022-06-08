library(here)
library(tidyverse)
library(tidymodels)
library(janitor) # clean variable names
library(skimr)
library(glimpse)
library(Hmisc) #rcorr()
library(corrplot) # creates correlation plot


height_weight <- read_csv(here("HeightWeight.csv")) %>% clean_names()
glimpse(height_weight)

# creates scatterplot to compare continuous variables, with line of best fit
ggplot(height_weight, aes(height_m, weight_kg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()

# view correlation between two values
cor(height_weight$height_m, height_weight$weight_kg)

# will give a correlation matrix (useful if +2 vars) and corresponding p-value
rcorr(as.matrix(height_weight))

# creates correlation plot
  # method can be "number", "circle", or "color" 
corrplot(cor(height_weight), method = "number")





























































