#' Training statistical models
#' 
#' This function coordinates four components in building statistical models:
#' 1) the data to use for training
#' 2) the response variable and
#' 3) the explanatory variables, specified as a model formula
#' 4) the architecture of the model.
#' 
#' This is a light wrapper on the architecture-specific function fitting programs
#' 
#' @param data the data to use for training
#' @param formula the formula describing the structure of the relationship among variables 
#' @param architecture the model architecture, given as a function name, quoted or not, e.g. lm or "lm"
#' 
train <- function(data, formula = formula(data), architecture = "lm") {
 
}

# Data file format: in inst/model-list.csv
# move eventually to read this in on load
# 
# name
# fit_function
# class: what kind of object is created
# formula : does it take a formula or an x-y input
# dataframe: which parts can be presented as a data frame (as opposed to a matrix)
# data_store: an expression saying where to find the data used to fit 
#             the model (e.g. data component, data_store attribute)