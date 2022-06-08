library(here)
library(tidyverse)
library(tidymodels)
library(janitor) # clean variable names
library(skimr)
library(glimpse)
library(Hmisc) #rcorr()
library(corrplot) # creates correlation plot
library(sjPlot)
library(sjmisc)
library(hablar)

# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# install.packages(c("sjPlot", "sjmisc"))

          # Interaction Effects for Categorical Variables

cat_int <- read_csv(here("Interactions_Categorical.csv")) %>%
  clean_names() %>% convert(fct(c(food, condiment)))

int <- aov(enjoyment ~ food + condiment + food * condiment, cat_int)
summary(int)

# x.factor & trace.factor are our interaction terms;
  # x.factor is on x-axis; trace.factor is the traced out lines
  # response is our y, dependent, our outcome term

          # Interaction Effects Plot for Categorical Variables

interaction.plot(x.factor = cat_int$food,
                 trace.factor = cat_int$condiment,
                 response = cat_int$enjoyment)

    ---

          # Interaction Effects for Continuous Variables

cont_int <- read_csv(here("Interactions_Continuous.csv")) %>% clean_names()

# the values that are NOT centered match those in the book
cont_reg_int <- lm(strength ~ temperature + pressure + time + pressure*temperature, cont_int)
summary(cont_reg_int)

        # Interaction Effects Plot for Continuous Variables

cont_reg_no_scale <- lm(strength ~ temperature + pressure + time + pressure*temperature, cont_int)

# use continuous values as is when plotting (i.e., don't use scaled or centered values)
# !NOTE: required to set minimum and maximum of pressure when plotting the model, which is 63.63 and 81.1
plot_model(cont_reg_no_scale, type = "int", terms = c("temperature, pressure[63.68,81.1]")) 

                    # centering values (???)
            
            # center variables to reduce multicollinearity if using continuous variables
            # with an interaction term
            cont_scaled <- scale(cont_int, center = TRUE, scale = FALSE) %>% as_tibble() %>% clean_names()
            
            # R^2, adjust R^2 p-value are accurate; 
            # time and temp:pres estimates also accurate; 
            # for some reason, temp, pressure, and intercept estimates
            
            cont_reg <- lm(strength ~ temperature + pressure + time + pressure*temperature, cont_scaled)
            summary(cont_reg)
            
            cont_ano <- aov(strength ~ temperature + pressure + time + pressure*temperature, cont_scaled)
            summary(cont_ano)
            
            
            