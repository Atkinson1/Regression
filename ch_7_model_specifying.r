library(tidyverse)
library(hablar)
library(GGally)
library(here)
library(janitor)
library(SignifReg) # used for stepwise regression - SignifReg()
library(leaps) # used for subsetting - regsubsets()
library(olsrr) # used for ols_mallows_cp() function for stepwise regression



      # using GGally package and its functions:
          # ggpairs() - give scatterplot, distribution, and correlation coefficient
          # & ggcorr() - give heatmap with corresponding correlation coefficient

mpg <- mpg
# ggpairs(). note: can set aes(color = var) like with ggplot2
ggpairs(mpg)
# ggcorr(). label = TRUE includes correlation coefficient; label_round gives number of significant digits
ggcorr(mpg, label = TRUE, label_round = 2)

      # scatterplot in base R
plot(mpg)

    # ---

stepwise regression:
https://cran.r-project.org/web/packages/SignifReg/SignifReg.pdf

      # product strength example from book (DV: Strength)
    # SignifReg package, SignifReg() function

prod_strength <- read_csv(here("ProductStrength.csv")) %>%
  clean_names()
  
prod_intercept <- lm(strength ~ 1, prod_strength)
prod_all <- lm(strength ~ ., prod_strength)

signif <- SignifReg(prod_intercept, scope = formula(prod_all),
          direction = "both", alpha = .15, criterion = "p-value")

summary(signif)

# direction Direction in variable selection: direction = "both",
# direction = "forward", and
# direction = "backward" are available. direction = "both" is a stepwise selection. Default is direction = "forward".
# criterion Criterion to select predictor variables. criterion = "AIC", criterion = "BIC",
# criterion = "r-adj" (adjusted r-square), criterion = "PRESS", and criterion
# = "p-value" are available. Default is p-value

        # Mallows Cp
https://search.r-project.org/CRAN/refmans/olsrr/html/ols_mallows_cp.html

    # to do mallows' cp, 
# ols_mallows_cp(model, fullmodel)
ols_mallows_cp(signif, prod_all)
  # note: want mallows cp to be close to intercept plus 
    # chosen terms

# ---

      # Best Subsets Regression
    # https://educationalresearchtechniques.com/2017/02/24/subset-regression-in-r/

fit <- lm(strength ~ ., prod_strength)
sub_fit <- regsubsets(strength ~., prod_strength)
best_fit <- summary(sub_fit)


par(mfrow = c(1,2))
plot(best_fit$cp) # gives number of vars for best fit
plot(sub_fit, scale = "Cp") # gives actual best fit;
                              # read from top (lowest y) to find
                              # best subset vars
