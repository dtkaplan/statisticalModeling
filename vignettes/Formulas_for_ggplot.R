## ----include = FALSE-----------------------------------------------------
library(statisticalModeling)
Runners <- Runners[complete.cases(Runners),]
library(mosaic)
library(rpart)

## ----fig.cap="In both base and lattice graphics, an R formula can be used to specify which variables go on the x- and y-axes. Left: base graphics. Right: lattice graphics.", fig.show = "hold"----
plot(mpg ~ wt, data = mtcars) # base graphics
xyplot(mpg ~ wt, data = mtcars) # lattice graphics

## ----echo = 1, fig.cap = "Formulas are used to identify response and explanatory variables in functions to train models."----
my_model <- rpart(mpg ~ hp + cyl, data = mtcars)
rpart.plot::prp(my_model, type = 3)

## ----tidy = FALSE--------------------------------------------------------
library(mosaic)
mean(mpg ~ cyl, data = mtcars)
  sd(mpg ~ cyl, data = mtcars)

## ----tidy = FALSE--------------------------------------------------------
 median(mpg ~ cyl, data = mtcars)
     lm(mpg ~ cyl, data = mtcars)
boxplot(mpg ~ cyl, data = mtcars)

## ------------------------------------------------------------------------
library(statisticalModeling)
gf_point(mpg ~ hp, data = mtcars)

## ------------------------------------------------------------------------
gf_point(mpg ~ hp + color:cyl + size:carb + alpha:0.75, data = mtcars)

## ----warning=FALSE-------------------------------------------------------
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

## ------------------------------------------------------------------------
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

