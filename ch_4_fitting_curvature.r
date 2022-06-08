library(here)
library(tidyverse)
library(tidymodels)
library(janitor) # clean variable names
library(skimr)
library(glimpse)
library(Hmisc) #rcorr()
library(corrplot) # creates correlation plot
library(hablar)
library(FrF2)

hardness <- read_csv(here("Hardness.csv")) %>% clean_names()
# pressure, hardness, temp

