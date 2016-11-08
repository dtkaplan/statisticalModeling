## ----include = FALSE-----------------------------------------------------
library(statisticalModeling)
library(dplyr)
library(ggplot2)
library(mosaic)

## ----fig.show = "hold"---------------------------------------------------
house_mod <- lm(price ~ fireplaces * living_area + land_value + bedrooms, data = Houses_for_sale)

## ----fig.show = "hold"---------------------------------------------------
fmodel(house_mod)
fmodel(house_mod, ~ living_area + fireplaces)

## ------------------------------------------------------------------------
fmodel(house_mod, ~ living_area + land_value + bedrooms, bedrooms = 1:4)

## ------------------------------------------------------------------------
library(ggplot2)
wage_mod <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
fmodel(wage_mod, ~ age + sex + sector, nlevels = 8) + 
  ggplot2::geom_point(data = mosaicData::CPS85, alpha = 0.1)

## ------------------------------------------------------------------------
evaluate_model(wage_mod)

## ------------------------------------------------------------------------
evaluate_model(wage_mod, nlevels = 2)
evaluate_model(wage_mod, sector = "service", age = c(25, 55))

## ------------------------------------------------------------------------
evaluate_model(wage_mod, sector = "service", sex = "F", age = 50)
evaluate_model(wage_mod, sector = "service", sex = "F", age = 55)

## ------------------------------------------------------------------------
effect_size(wage_mod, ~ age, age = 50, sector = "service")

## ------------------------------------------------------------------------
baseline_mod <- lm(price ~ living_area + bathrooms + land_value, data = Houses_for_sale)

## ------------------------------------------------------------------------
fireplace_mod <- lm(price ~ living_area + bathrooms + land_value + fireplaces, 
                    data = Houses_for_sale)

## ------------------------------------------------------------------------
anova(baseline_mod, fireplace_mod)

## ------------------------------------------------------------------------
Trials <- cv_pred_error(baseline_mod, fireplace_mod, ntrials = 10)
Trials

## ------------------------------------------------------------------------
boxplot(mse ~ model, data = Trials)

## ------------------------------------------------------------------------
t.test(mse ~ model, data = Trials)

## ------------------------------------------------------------------------
effect_size(fireplace_mod, ~ fireplaces)

## ------------------------------------------------------------------------
E <- ensemble(fireplace_mod, nreps = 50)

## ------------------------------------------------------------------------
EE <- effect_size(E, ~ fireplaces)
with(data = EE, sd(slope))

