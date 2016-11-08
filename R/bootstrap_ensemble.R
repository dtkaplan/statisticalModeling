#' Create bootstrapped ensembles  of a model
#' 
#' @param mod a model whose data will be used for resampling
#' @param nreps how many resampling trials should be created
#' @param data a data table to use for resampling. This is not needed for many common
#' model types, such as \code{lm}, \code{glm}, etc. See details.
#' 
#' @details The approach to bootstrapping implemented by this function is to create a set of bootstrap
#' trials all in one go. Then, other functions such as \code{effect_size} and \code{evaluate_model()} will be 
#' used to extract the information from each of the bootstrap replicates. Many model types in R carry the
#' data used to train the model as part of the model object produced. For these types of models, e.g. \code{lm} and 
#' \code{glm}, there is no need to provide a value for the \code{data} argument. But there are some 
#' types of models for which the training data cannot be extracted from the model object. In such situations, 
#' you use \code{data =} to provide the data set to use for resampling. 
#' 
#' @export
ensemble <- function(mod, nreps = 2, data = NULL) {
  # not yet dealing with poly(x,2) situations
  # does this have to be something where mod was built by train()?
  if (is.null(data)) 
    data <- data_from_model(mod)
  architecture <- mod$call[[1]]
  res <- list()
  res$original_model <- mod
  res$data <- data
  fit_call <- res$call <- mod$call
  fit_call[[3]] <- as.name("train_data")
  res$replications <- list()
  res$oob <-list() # out of bag cases for each replication
  for (k in 1:nreps) {
    this_rep <- list()
    inds <- sample(nrow(data), size = nrow(data), replace = TRUE)
    train_data <- data[inds, ]
    # Find the cases not used in training the model.
    # These are the out-of-bag (oob) cases, useful for cross-validation
    oob <- 1:nrow(data)
    oob <- oob[ ! oob %in% inds]
    res$oob[[k]] <- oob
    res$replications[[k]] <- eval(fit_call)
  }
  
  class(res) <- "bootstrap_ensemble"
  res
}

#' @export
print.bootstrap_ensemble <- function(x, ...) {
  cat(
    paste("An ensemble of", length(x$replications), "bootstrap replications", 
        "of\n"))
    print(x$original_model$call)
    invisible(x)
}


