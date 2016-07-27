#' Plot out model values
#'
#' @param model the model to display graphically
#' @param formula setting the y ~ x + color variables
#' @param data optional data set from which to extract levels for explanatory variables
#' @param nlevels how many levels to display for those variables shown at discrete levels
#' @param at named list giving specific values at which to hold the variables.
#' @param prob_of if to show probability of a given level of the output, name the class here as a character string.
#' @param ... arguments to predict()
#'
#'
#' @examples
#' \dontrun{mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' fmodel(mod1)
#' fmodel(mod1, ~ sector + sex + age) # not necessarily a good ordering
#' require(ggplot2)
#' fmodel(mod1, ~ age + sex + sector, nlevels = 8) + 
#'   geom_point(data = mosaicData::CPS85, alpha = 0.1) +
#'   ylim(0, 20)
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' fmodel(mod3, type = "response")
#' # Adding the raw data requires a trick when it's TRUE/FALSE
#' fmodel(mod3, ~ age + sex + sector, data = CPS85, nlevels = 10, type = "response") + 
#'   geom_point(data = CPS85, aes(x = age, y = 0 + (married == "Married")), alpha = .1)
#' }
#' @export
fmodel <- function(model=NULL, formula = NULL, data = NULL, 
                   nlevels = 3, at = list(), prob_of = NULL, ...) {
  extras <- list(...)
  if (is.null(model)) {
    stop("Must provide a model for graphing.")
  } else if (inherits(model, 
                      c("rpart", "glm", "lm", "groupwiseModel",
                        "randomForest", "gbm"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for fmodel().")
  }
  if( inherits(model, "gbm")) stop("gbm models still not working.")

  # try to figure out what are the possible levels of variables
  if (is.null(data)) data <- data_from_model(model)
  response_var <- response_var(model)
  # is the response categorical?  If so, plot the probability of the given level
  response_values <- 
    if (response_var %in% names(data)) {data[[response_var]]}
    else {eval(parse(text = response_var), envir = data)}
  if (! inherits(response_values, c("numeric", "logical"))) {
    # It's categorical
    if (is.null(prob_of)) prob_of <- names(sort(table(response_values), decreasing = TRUE))[1]
    if ( ! prob_of %in% response_values) stop("Level '", prob_of, "' doesn't exist in the response variable.")
    response_var <- prob_of

  }

  explan_vars <- explanatory_vars(model)
  if (is.null(formula)) show_vars <- explan_vars
  else show_vars <- all.vars(mosaic::rhs(formula))

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  # Use the fourth as the second <facet> variable.

  if (length(show_vars) > 4) show_vars <- show_vars[1:4]

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(Inf, rep(nlevels, length(show_vars) - 1)))
  names(how_many) <- show_vars
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )

  # set up so that glms are plotted, by default, as the response rather than the link
  if (inherits(model, "glm") && ( ! "type" %in% names(extras))) {
    extras$type = "response"
  }
  model_vals <- 
    do.call(predict,c(list(model, newdata = eval_levels, level = prob_of), 
                      extras))
  if (inherits(model, "rpart") && inherits(model_vals, c("data.frame", "matrix"))) {
    # handle the matrix values from predict.rpart()
    keepers <- colnames(model_vals) == prob_of
    model_vals <- model_vals[,keepers] # just the first class
  }

  # convert any quantiles for numerical levels to discrete
  first_var_quantitative <- is.numeric(eval_levels[[show_vars[1]]])
  eval_levels <- convert_to_discrete(eval_levels)
  # Deal with the response variable being a complex name
  # e.g. when it includes operations on a data variable.
  clean_response_name <- response_var
  if( ! response_var %in% names(data))
    clean_response_name <- "clean"
  eval_levels[[clean_response_name]] <- model_vals

  # figure out the components of the plot

  P <- 
    ggplot(data = eval_levels,
           aes_string(x = show_vars[1], y = clean_response_name), group = NA) + 
    ylab(response_var)

  if (length(show_vars) == 1) {
    if (first_var_quantitative) {
      P <- P + geom_line()
    }
    else {
      P <- P + geom_point()
    }
  } else { # more than one explanatory variable
    if (first_var_quantitative) {
      P <- P + geom_line(
        aes_string(color = show_vars[2], linetype = show_vars[2]), 
        alpha = 0.8)
    } else {
      P <- P + geom_point(
        aes_string(color = show_vars[2], linetype = show_vars[2]), 
        alpha = 0.8) +
        geom_line(aes_string(group = show_vars[2], color = show_vars[2]), alpha = 0.8)
    }
  }


  if (length(show_vars) == 3)
    P <- P + facet_wrap(show_vars[3], labeller = label_both)
  if (length(show_vars) == 4)
    P <- P + facet_grid(paste(show_vars[3], "~", show_vars[4]), 
                        labeller = label_both)

  P # return the plot
}

