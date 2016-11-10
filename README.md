[![Travis-CI Build Status](https://travis-ci.org/dtkaplan/statisticalModeling.svg?branch=master)](https://travis-ci.org/dtkaplan/statisticalModeling)

# R Functions for teaching statistics and statistical modeling

This package reflects my evolving thinking about how to teach statistics and the importance of integrating *modeling* into how students think about statistics. Many of the basic ideas have been expressed in my book [*Statistical Modeling: A Fresh Approach*](http://project-mosaic-books.com/?page_id=13) (2/e, 2011):

* make statistics about *explaining* variation rather than comparing means
* place covariation at the center, since almost all modern studies, including those in the news contain some adjustment for covariates, often signalled by the phrase "after adjusting for ...."
* use modern computation not merely to make easier traditional calculations, such as means, standard deviations, and table lookups. Instead use the power of modern computation to create the conceptual basis for thinking about statistical problems.

For the *Statistical Modeling* book, I wrote a set of R functions, which were a light gloss on standard R that made it possible to avoid esoteric R commands in favor of a straightforward notation. With Randy Pruim and Nicholas Horton, we expanded and systemetized this into the `mosaic` package, distributed via CRAN. I think that's still a good way to teach statistics.

But there have been many changes in the decade since Randy, Nick, and I started working together.

```{r}
library(randomForest)
data(CPS85, package = "mosaicData")

linear_mod <-           lm(wage ~ educ + sex + age, data = CPS85)
forest_mod <- randomForest(wage ~ educ + sex + age, data = CPS85)

fmodel(forest_mod)
```


This package contains model evaluation and graphing utilities for teaching statistical modeling. It's written to gather in one place the functions used in the DataCamp statistical modeling course and for a possible third edition of *Statistical Modeling: a Fresh Approach*.

1. `effect_size(mod)` for calculating the effect size of an explanatory on a response
2. `fmodel(mod)` for making a nice but easy graph of model output vs inputs.
3. A formula interface to `ggplot2` graphics (which I intend to expand to other graphic systems, particularly interactive graphics)
4. A shiny/gadget interface for `ggplot2`. But perhas the formulas make this unnecessarily



## The formula-based interface

Making a ggplot involves mapping variables to aesthetics, setting constants to other aesthetics, and choosing a `geom_`.

In the formula interface, the function name specifies the `geom_` and a formula controls the mapping and setting of aesthetics.  For instance,
```r
data(KidsFeet, package = "mosaicData")
library(gghelper)
fpoint(width ~ length + color:sex, data = KidsFeet)
```
Note that the `x` and `y` aesthetics are derived from the formula in the usual way.  Other aesthetics are set by `property:value` pairs.  The value
may be either a variable from the data table, or a constant, e.g. color:blue.




# To install most recent version from GitHub

```
# Install devtools if necessary
install.packages("devtools")

# Install statisticalModeling
devtools::install_github("dtkaplan/statisticalModeling")
```


