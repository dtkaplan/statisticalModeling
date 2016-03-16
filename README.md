# Functions for teaching statistical modeling

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

## Shiny Gadget

The shiny gadget interface works in much the same way.  A formula is used to set the initial frame variables, but the aesthetics will be mapped/set using dropdown menus.
```
gghelper(KidsFeet, width ~ length)
```
