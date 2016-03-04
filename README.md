# Functions for starting out with ggplot2

There are two kinds of helpers:

1. A formula-based interface to ggplot2
2. A shiny/gadget interface

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
