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
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' fmodel(mod3, type = "response")
#' }
#' @export
fmodel <- function(model=NULL, formula = NULL, data = NULL, nlevels = 3, at = list(), prob_of = NULL, ...) {
  extras <- list(...)
  if (is.null(model)) {
    stop("Must provide a model for graphing.")
  } else if (inherits(model, c("rpart", "glm", "lm", "groupwiseModel"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for fmodel().")
  }

  # try to figure out what are the possible levels of variables
  if (is.null(data)) data <- data_from_model(model)
  response_var <- response_var(model)
  # is the response categorical?  If so, plot the probability of the given level
  response_values <- 
    if (response_var %in% names(data)) {data[[response_var]]}
    else {eval(parse(text = response_var), envir = data)}
  if (! is.numeric(response_values)) {
    # It's categorical
    if (is.null(prob_of)) prob_of <- names(sort(table(response_values), decreasing = TRUE))[1]
    if ( ! prob_of %in% response_values) stop("Level '", prob_of, "' doesn't exist in the response variable.")
    response_var <- prob_of

  }

  if (is.null(formula)) explan_vars <- explanatory_vars(model)
  else explan_vars <- all.vars(mosaic::rhs(formula))

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  # Use the fourth as the second <facet> variable.

  if (length(explan_vars) > 4) explan_vars <- explan_vars[1:4]

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(Inf, rep(nlevels, length(explan_vars) - 1)))
  names(how_many) <- explan_vars
  eval_levels <- reference_values(data, n = how_many, at = at )

  # set up so that glms are plotted, by default, as the response rather than the link
  if (inherits(model, "glm") && ( ! "type" %in% names(extras))) {
    extras$type = "response"
  }
  model_vals <- do.call(predict,c(list(model, newdata = eval_levels, level = prob_of), extras))
  if (inherits(model, "rpart") && is.data.frame(model_vals)) {
    # handle the matrix values from predict.rpart()
    keepers <- colnames(model_vals) == prob_of
    model_vals <- model_vals[,keepers] # just the first class
  }

  # convert any quantiles for numerical levels to discrete
  first_var_quantitative <- is.numeric(eval_levels[[explan_vars[1]]])
  eval_levels <- convert_to_discrete(eval_levels)
  eval_levels$response <- model_vals

  # figure out the components of the plot

  P <- 
    ggplot(data = eval_levels,
           aes_string(x = explan_vars[1], y = "response"), group = NA) + 
    ylab(response_var)

  if (length(explan_vars) == 1) {
    if (first_var_quantitative) {
      P <- P + geom_line()
    }
    else {
      P <- P + geom_point()
    }
  } else { # more than one explanatory variable
    if (first_var_quantitative) {
      P <- P + geom_line(aes_string(color = explan_vars[2]), alpha = 0.8)
    } else {
      P <- P + geom_point(aes_string(color = explan_vars[2]), alpha = 0.8) +
        geom_line(aes_string(group = explan_vars[2], color = explan_vars[2]), alpha = 0.8)
    }
  }


  if (length(explan_vars) == 3)
    P <- P + facet_wrap(explan_vars[3])
  if (length(explan_vars) == 4)
    P <- P + facet_grid(paste(explan_vars[3], "~", explan_vars[4]))

  P # return the plot
}

