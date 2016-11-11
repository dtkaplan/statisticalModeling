[![Travis-CI Build Status](https://travis-ci.org/dtkaplan/statisticalModeling.svg?branch=master)](https://travis-ci.org/dtkaplan/statisticalModeling)

# R Functions for teaching statistics and statistical modeling

Detailed examples of the use of the `statisticalModeling` package are contained in the package vignettes. This document is directed to instructors to explain the motivation behind `statisticalModeling`. 

This package reflects my evolving thinking about how to teach statistics and the importance of integrating *modeling* into how students think about statistics. Many of the basic ideas have been expressed in my book [*Statistical Modeling: A Fresh Approach*](http://project-mosaic-books.com/?page_id=13) (2/e, 2011):

1. make statistics about *explaining* variation rather than comparing means
2. place covariation at the center, since almost all modern studies, including those in the news, contain some adjustment for covariates, often signalled by the phrase "after adjusting for ...."
3. use modern computation to establish a conceptual framework for thinking about modeling, not merely to make easier traditional calculations, such as means, standard deviations, and table lookups.

This package is about (3).

Teaching about statistical modeling often starts with linear regression. I think there is an advantage to introducing other modeling techniques at the same time or even before linear regression. Why?

- Regression models are useful for some problems, classifiers are useful for others. For many audiences, classification can be a more intuitive and compelling problem type to study.
- Model forms such as classification and regression trees can be much easier for statistics students to interpret, and can tell a richer story that makes interactions among explanatory variables easier to see and understand.
- A modern sort of statistical problem is searching through masses of data for patterns and relationships. Students should see early on approaches to this problem.

R provides an infrastructure to support teaching about linear regression. This includes, of course, the `lm()` function, but also supporting functions for inference and graphics, e.g.

- `summary()` when applied to an `lm` object produces the traditional regression table and other information such as R$^2$.
- `abline()` makes it easy to plot a (single-variable) regression line over data. Functions such as `stat_smooth()` in the `ggplot2` package make it easy to extend this to functions of several variables.
- `confint()` produces confidence intervals on coefficients. 
- The `mosaic` package has added support for bootstrapping, randomization tests, and the like, as well as extending base functions such as `mean()` to allow the formula interface to modeling and to provide a straightforward and consistent template that covers a wide variety of statistical techniques.

This `statisticalModeling` package provides an alternative interface that generalizes to many different statistical modeling types, both regression and classification.  It includes:

* `evaluate_model()` produces model outputs that correspond to inputs. It simplifies quickly examining multi-variate models, since it will choose sensible values for any inputs that have not been given specific values. It also generalizes across model architectures in ways that the `predict()` family of methods does not.
* `effect_size()` for examining how a change in a model input is related to a change in model output. It is, in effect, a generalization of regression coefficients.
* `cv_pred_error()` makes it simple to apply cross-validation to compare models.
* `ensemble()` provides simple support for bootstrapping effect sizes.

In terms of graphics

* `fmodel()` is the extension to `abline()`. The `fmodel()` function makes it straightforward to visualize models with multiple variables --- variation with up to four explanatory variables can be shown (with variables beyond four being held constant). It works for many different regression model architectures as well as classification models.
* A family of graphics functions, `gf_point()`, `gf_density()`, and so on, bring the formula interface to `ggplot()`. This captures and extends the excellent simplicity of the `lattice`-graphics formula interface, while providing the intuitive "add this component" capabilities of `ggplot()`.

Installations from CRAN are done in the usual way. The development version of the package is here on GitHub. To install it, use the following commands in your R system.

```
# Install devtools if necessary
install.packages("devtools")

# Install statisticalModeling
devtools::install_github("dtkaplan/statisticalModeling")
```


