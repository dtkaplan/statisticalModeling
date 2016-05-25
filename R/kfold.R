#' Compare models with k-fold cross-validation
#'
#' @export 
cv_pred_error <- function(..., k = 10, ntrials = 10, 
                     output = c("mse", "likelihood", "error_rate")) {
  output <- match.arg(output)
  models <- list(...)
  # default_names <- paste0("mod_", 1:length(models)) # not using
  full_names <- as.character(lapply(lazyeval::lazy_dots(...), FUN = function(x) x$expr))
  # model can be a list. If so, repeat over all the models.
  # If it's a single model, put it in a list.
#  if (! inherits(model, c("list"))) 
#    model <- list(model)
  
  
  # Just a first stab at the problem.
  type <- switch(output, 
                 "mse" = "response",
                 "likelihood" = "response",
                 "error_rate" = "class")
  
  
  result = NULL
  for (counter in 1:length(models)) {
    this_mod <- models[[counter]]
    truth <- response_values(this_mod)
    pred_error_results <- numeric(ntrials)
    for (this_trial in 1:ntrials) {
      mod_output <- kfold_trial(this_mod, type = type)$model_output
      pred_error_results[this_trial] <- 
        if( type == "class") mean(truth != mod_output, na.rm = TRUE)
        else mean((truth - mod_output)^2, rm.na = TRUE)
    }
    from_this_mod <- data.frame(pred_error_results, model = full_names[counter],
                                stringsAsFactors = FALSE)
    names(from_this_mod)[1] <- output

    result <- rbind(result, from_this_mod)
  }

  
  result
}

#' @export
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
  
  architecture <- mod$call[[1]]
  fit_call <- mod$call
  fit_call[["data"]] <- as.name("training")
  # Create a holder for the result
  output <- evaluate_model(mod, data = data, type = type)
  groups <- sample(rep(1:k, length.out = nrow(data) ))
  
  convert_to_likelihood <- FALSE
  if (type == "likelihood") {
    convert_to_likelihood <- TRUE
    type <- "prob"
  }
  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]

    testing  <- data[group == groups, , drop = FALSE ]
    this_model <- eval(fit_call)
    output[group == groups, ] <- evaluate_model(this_model, data = testing, type = type)
  }

  if (convert_to_likelihood) {
    # get the response variable from the data
    
    # figure out the different formats and how to make them work as likelihood.
    
    stop("Likelihood not yet implemented.")
  }
  
  output
}

