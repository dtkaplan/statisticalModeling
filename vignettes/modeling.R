## ----include = FALSE-----------------------------------------------------
library(statisticalModeling)
library(dplyr)
library(ggplot2)
library(mosaic)

## ----fig.show = "hold"---------------------------------------------------
house_mod <- lm(price ~ fireplaces * living_area + land_value + bedrooms, data = Houses_for_sale)

## ----fig.show = "hold"---------------------------------------------------
gmodel(house_mod)
gmodel(house_mod, ~ living_area + fireplaces)

## ------------------------------------------------------------------------
gmodel(house_mod, ~ living_area + land_value + bedrooms, bedrooms = 1:4)

## ------------------------------------------------------------------------
library(ggplot2)
wage_mod <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
gmodel(wage_mod, ~ age + sex + sector, nlevels = 8) + 
  ggplot2::geom_point(data = mosaicData::CPS85, alpha = 0.1)

## ------------------------------------------------------------------------
mod <- lm(log(wage) ~ age + educ + sex, data = CPS85)
gmodel(mod, post_transform = c("hourly wage" = exp))

## ------------------------------------------------------------------------
data(CPS85, package = "mosaicData")
mod <- glm(married ~ age*wage*sex + sector, family = "binomial", data = CPS85)
gmodel(mod, prob_of = "Single")


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

## ------------------------------------------------------------------------
library(mosaic)

do(10) * {
  fireplace_mod <- 
    lm(price ~ living_area + bathrooms + land_value + fireplaces, 
       data = resample(Houses_for_sale))
  effect_size(fireplace_mod, ~ fireplaces)
}

## ------------------------------------------------------------------------
library(statisticalModeling)
gf_point(mpg ~ hp, data = mtcars)

## ------------------------------------------------------------------------
gf_point(mpg ~ hp + color:cyl + size:carb + alpha:0.75, data = mtcars) +
  ggplot2::ylab("Miles per gallon") +
  ggplot2::xlab("Horsepower")

## ----fig.show = "hold", out.width = "30%", warning=FALSE-----------------
Runners <- subset(Runners, ! is.na(net))
gf_density( ~ net, data = Runners)
gf_density( ~ net + fill:sex + alpha:0.5, data = Runners)
gf_density( ~ net + fill:sex + color:NA + alpha:0.5, data = Runners)

## ----fig.show = "hold", warning=FALSE------------------------------------
gf_density( ~ net + fill:sex + color:NA + position:"stack", data = Runners)
gf_density( ~ net + fill:sex + color:NA + position:"fill", data = Runners)

## ----fig.show = "hold", warning = FALSE----------------------------------
gf_boxplot(net ~ sex + color:"red", data = Runners)
gf_boxplot(net ~ sex + color:start_position, data = Runners)

## ------------------------------------------------------------------------
Runners$the_year <- as.character(Runners$year) # in base R
Runners <- Runners %>% mutate(the_year = as.character(year)) # in dplyr
gf_boxplot(net ~ the_year + color:sex, data = Runners)

## ----fig.show = "hold"---------------------------------------------------
gf_density_2d(net ~ age, data = Runners)
gf_hex(net ~ age, data = Runners)

## ----fig.show = "hold", out.width = "30%"--------------------------------
# use a categorical variable
mtcars <- mtcars %>% mutate(n_cylinders = as.character(cyl)) 
gf_line(mpg ~ hp, data = mtcars)
gf_path(mpg ~ hp, data = mtcars)
gf_line(mpg ~ hp + color:n_cylinders, data = mtcars)

## ----fig.show = "hold", warning=FALSE------------------------------------
gf_density_2d(net ~ age, data = Runners) + facet_grid( ~ sex)
gf_density_2d(net ~ age, data = Runners) + facet_grid(start_position ~ sex)

## ------------------------------------------------------------------------
gf_jitter(age ~ sex + alpha:0.01, data = Runners)

