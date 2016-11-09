# returns a vector of predictions or likelihoods
kfold_trial <- function(mod, 
                        k=10, 
                        type = c("response", "prob", "likelihood", "class")) {
  # This isn't working for null models
  # null_mod <- lm(net ~ 1, data = Runners)
  # null_output <- kfold_trial(null_mod)
  
  
  
  type <- match.arg(type)
  # Grab the data and the call from the model.
  data <- data_from_model(mod)
  # For cross validation, we don't want the constructed terms
  constructed <- grep("\\(.*\\)", names(data))
  if (length(constructed) > 0) data[[constructed]] <- NULL # get rid of them
  
  # set up the call for fitting the model to the training data
  architecture <- mod$call[[1]]
  fit_call <- mod$call
  fit_call[["data"]] <- as.name("training")
  # construct the groups for the k-fold divisions
  groups <- sample(rep(1:k, length.out = nrow(data) ))
  # Create a holder for the result
  # output <- evaluate_model(mod, data = data, type = type)
  output <- numeric(nrow(data))
  
  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]
    
    testing  <- data[group == groups, , drop = FALSE ]
    
    this_model <- eval(fit_call)
    
    output[group == groups] <- 
      if (type == "likelihood") {
        likelihood_helper(this_model, data = testing)
      } else if (type == "class") {
        class_helper(this_model, data = testing)
      } else {
        mse_helper(this_model, data = testing)
      }
  }
  
  output
}

# helper for reference levels
get_range <- function(var_name, n, data) {
  # get an appropriate set of levels
  values <- eval(parse(text = var_name), envir = data)
  conversion <- NA
  value_set <-
    if (n == Inf) {
      if (is.numeric(values)) {
        if (length(unique(values)) < 100) unique(values)
        else seq(min(values, na.rm = TRUE), 
                 max(values, na.rm = TRUE), length = 100)
      } else {
        as.character(unique(values))
      }
    } else {
      if (is.numeric(values)) {
        conversion <- "as.discrete"
        if (n == 1) {
          median(values, na.rm = TRUE)
        } else {
          bottom <- ifelse(n == 2, 0.3, 
                           ifelse(n > 4, 
                                  ifelse(n > 9, 0.0, 0.1), 
                                  0.2))
          most <- quantile(values, c(bottom, 1 - bottom), na.rm = TRUE)
          pretty(most, n = pmax(1, n - 2))
        }
        #quant_levels <- seq(0,1, length = n + 2)[c(-1, -(n + 2))]
        #quantile(values, quant_levels, na.rm = TRUE)
      } else {
        level_names <- names(sort(table(values), decreasing = TRUE))
        level_names[1:pmin(n, length(level_names))]
      }
    }
  # will the variable ultimately be represented as discrete
  attr(value_set, "convert") <- conversion
  value_set
}

# make reference levels for a model evaluation
create_eval_levels <- function(model, formula, from = NULL, at = NULL, data = NULL){
  # grab the explanatory variable to use for the difference
  change_var <- all.vars(mosaic::rhs(formula))
  
  if (is.null(data)) data <- data_from_model(model, data = data)
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
        popular <- names(sort(table(values), decreasing = TRUE))[1]
        if (is.factor(values)) {
          popular <- factor(popular, levels = levels(values))
        }
        popular
      }
  }
  if ( ! is.null(from)) centers[[change_var]] <- from
  
  # loop over <at> and update any nominal values
  for (k in seq_along(at)) {
    nm <- names(at)[k]
    if ( ! nm %in% explan_vars) stop(nm, "isn't an explanatory variable.")
    centers[[nm]] <- at[[k]]
    if (is.factor(data[[nm]]))
      centers[[nm]] <- factor(centers[[nm]], levels = levels(data[[nm]]))
  }
  # input values for the explanatory variables
  reference_values(data[,explan_vars, drop = FALSE], at = centers)
}

get_step = function(ref_vals, change_var, data, step = NULL, from = NULL) {
  if (is.null(step)) { # Need to set the step
    vals <- data[[change_var]]
    
    if (is.numeric(vals)) step <- sd(vals, na.rm = TRUE)
    else {
      if ( ! is.null(from)) 
        vals <- vals[ ! vals %in% from]
      else {
        vals <- vals[ ! vals %in% ref_vals[[change_var]]]
        if (length(vals) == 0) vals <- data[[change_var]]
      }
      step <- names(sort(table(vals), decreasing = TRUE))[1] 
    }
  }
  step
}

# separate ... into the components that match explanatory variables ($at) 
# and those that don't ($extras)

handle_dots_as_variables <- function(mod, ...) {
  xvars <- explanatory_vars(mod)
  All <- list(...)
  res <- list()
  res$at <- All[names(All) %in% xvars]
  res$extras <- All[ ! names(All) %in% xvars]
  
  res
}


