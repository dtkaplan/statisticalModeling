#' Calculate effect sizes in a model
#'
#' Like a derivative or finite-difference

#' @param model the model from which the effect size is to be calculated
#' @param formula a formula whose right-hand side is the variable with respect
#' to which the effect size is to be calculated.
#' @param at center values for evaluating the model
#' @param step list of numbers for numerical finite-difference, or comparison groups for
#' @param raw for 2nd-order effects, show the sets of first-order effects
#' @param ... additional arguments for \code{predict()}. For instance, for a glm, perhaps you
#' want \code{type = "response"}.
#'
#' @examples
#' mod1 <- lm(wage ~ age * sex * educ + sector, data = mosaicData::CPS85)
#' effect_size(mod1, ~ sex)
#' effect_size(mod1, ~ sector)
#' effect_size(mod1, ~ age, at = list(sex = "M"))
#' effect_size(mod1, ~ age, at = list(sex = "F"))
#' effect_size(mod1, ~ sex + age) # the difference in differences
#' @details If two different variables are listed in \code{formula}, the second-order (interaction) effect size
#' will be calculated.  To Do? Allow 2nd deriv w.r.t a single variable? Get multiple effect sizes all at once.


#' @export
effect_size <- function(model, formula, at = NULL, step = NULL, raw = FALSE, ... ) {
  # grab the explanatory variable to use for the difference
  change_vars <- all.vars(mosaic::rhs(formula))

  data <- data_from_model(model)
  response <- response_var(model)
  explan_vars <- explanatory_vars(model)

  centers <- as.list(rep(NA, length(explan_vars)))
  names(centers) <- explan_vars
  for (var_name in explan_vars) {
    values <- data[[var_name]]
    centers[[var_name]] <-
      if (is.numeric(values)) {
        median(values, na.rm = TRUE)
      } else {
        # get the most popular level
        names(table(sort(values, decreasing = TRUE)))[1]
      }
  }

  # loop over <at> and update any nominal values
  for (k in seq_along(at)) {
    nm <- names(at)[k]
    if ( ! nm %in% explan_vars) stop(nm, "isn't an explanatory variable.")
    centers[[nm]] <- at[[k]]
  }

  change_ranges <- as.list(rep(NA, length(change_vars)))
  names(change_ranges) <- change_vars
  change_ranges[names(step)] <- step
  for (vname in change_vars) {
    if ( ! vname %in% names(step)) {
      vals <- data[[vname]]
      if (is.numeric(vals)) change_ranges[[vname]] <- sd(vals, na.rm = TRUE)
      else {
        center_category <- centers[[vname]]
        new_categories <- names(sort(table(vals)))
        new_categories <- setdiff(new_categories, center_category)
        change_ranges[[vname]] <- new_categories[1:pmin(length(new_categories), 10)]
      }
    }

    if (is.numeric(centers[[vname]]))
      centers[[vname]] <- centers[[vname]] + c(-1, 1) * change_ranges[[vname]]
    else
      centers[[vname]] <- c(centers[[vname]], change_ranges[[vname]])
  }

  input_data <- reference_values(data, at = centers)

  mod_vals <- predict(model, newdata = input_data)
  input_data[[response]] <- mod_vals
  # Set the response variable
  # PROCESS
  # for the variables in formula, each has a high and low value
  # set the nominals for that variable to be high value
  #  - calculate the model output
  # set the nominals for the variable to be the low value(s) --- plural if categorical
  #  - calculate the model output
  # subtract the low value(s) from the high values

  # If there is a second variable
  # perform the process for the high and low values of the second variable
  # just let there be 2 values for the second variable

  one_change <- function(in_data, var_name, response_names) {
    var_levels <- in_data[[var_name]]
    ref_level <- var_levels[1]
    remaining_levels <- var_levels[-1]
    response <- in_data[response_names]
    ref_val <- response[1,]
    others <- response[-1,]
    effect <- ref_val - others
    if (is.numeric(ref_level)) {
      res <- effect / (ref_level - remaining_levels)
      names(res) <- paste0("delta_", response_names, "_by_", var_name)
    } else {
      res <- - effect
      names(res) <- paste0(response_names, "..from..", ref_level, "..to..", remaining_levels)
    }

    res
  }

  if (length(change_vars) == 1) {
    if (raw) effects <- input_data
    else effects <- one_change(input_data, change_vars, response)
  } else if (length(change_vars) == 2) {
    effects <- NULL
    vals <- unique(input_data[[change_vars[[2]]]])
    if (is.factor(vals)) vals <- as.character(vals)
    input_data <- split(input_data, input_data[[change_vars[2]]])
    for (k in 1:length(input_data)) {
      res <- one_change(input_data[[k]], change_vars[[1]], response)
      effect <- data.frame(as.list(res))
      row.names(effect) <- NULL
      names(effect) <- names(res)
      effects <- rbind(effects, data.frame(effect, vals[k], stringsAsFactors = FALSE))
    }
    effects <- data.frame(effects, stringsAsFactors = FALSE)
    names(effects)[ncol(effects)] <- change_vars[[2]]
    if ( ! raw) effects <- one_change(effects, change_vars[2], names(effects)[-length(names(effects))])
  } else {
    stop("Only 1st and 2nd-order interactions supported.")
  }

  effects
}
