#' functions to apply to bootstrap replications of models
#' 
#' @rdname boot_funs
#' @export
boot_rsq <- function(bsreps) {
  n_mods <- length(bsreps$replications)
  res <- numeric(n_mods)
  for (k in 1:n_mods) {
    res[k] <- mosaic::rsquared(bsreps$replications[[k]]$mod)
  }
  res
}

# REFACTOR effect_size() so that there is a ref_levels() function that produces
# input_data matrix. Allow this to be passed as an argument. Then, in the boot_effect_size()
# you can capture this matrix on the first call to effect_size() and re-use it 
# on others.
#
# Also, in bootstrap_rep() use `try()` to deal with situations where there is a
# missing level in a categorical variable.

#' @export

boot_effect_size <- function(bsreps, formula, data = NULL){
  n_mods <- length(bsreps$replications)
  res <- list(n_mods)
  for (k in 1:n_mods) {
    this_mod <- bsreps$replications[[k]]$mod
    res[[k]] <- effect_size(this_mod, formula, data = data)
  }
  do.call(rbind, res) # convert it to a data frame
}
