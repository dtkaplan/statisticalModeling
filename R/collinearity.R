#' Calculate measures of collinearity
#' 
#' 
#' @param formula a formula giving, on the right-hand side, the explanatory
#' variables to be considered. Iteractions, etc. may also be specified.
#' @param data a data frame from which to draw the variables in the formula
#' @param format choice of \code{"SeIF"} for inflation of standard errors, 
#' \code{"degrees"} or \code{"radians"} for collinearity described as an angle
#' or \code{"VIF"} for the variance inflation factor (which is the square of SeIF).
#' @examples 
#' collinearity( ~ cyl * disp * hp, data = mtcars)
#' collinearity( ~ cyl * disp * hp, data = mtcars, format = "degrees")
#' @export
collinearity <- function(formula, data, 
                         format = c("SeIF", "degrees", "radians", "VIF")) {
  format <- match.arg(format)
  data <- model.matrix(formula, data)[,-1] # no intercept
  expl_vars <- colnames(data)
  nvars <- length(expl_vars)
  angles <- numeric(nvars)
  for (k in 1:length(expl_vars)) {
    response <- data[ , k]
    explan <- data[ , -k]
    
    angles[k] <- acos(sqrt(mosaic::rsquared(lm(response ~ explan))))
  }
  
  result <- data.frame(expl_vars, stringsAsFactors = FALSE)
  value <- switch(format,
                  "SeIF" = 1 / sin(angles),
                  "VIF" = (1 / sin(angles))^2,
                  "degrees" = 180 * angles / pi,
                  "radians" = angles)
  result[[format]] <- value
  
  result
}

