#' Swimming speed of tadpoles.
#' 
#' Tim Watkins examined the swimming speed of tadpoles as a function of the water temperature, and the 
#' water temperature at which the tadpoles had been raised. Since size is a major determinant of speed,
#' the tadpole's length was measured as well.
#' 
#' It was hypothesized that tadpoles would swim faster in the temperature of water close to that in which they had been raised.
#' 
#' @format A data frame \code{Trucking_jobs} with 129 rows and 11 variables. 
#'   \itemize{
#'     \item{\code{group}} {Whether the tadpole was raised in cold water ("c") or warm ("w").}
#'     \item{\code{rtemp}} {The temperature (C) of the water in which the swimming speed was measured. (The swimming channel is called a "race".)}
#'     \item{\code{length}} {The tadpole's length, in mm.}
#'     \item{\code{vmax}} {The maximum swimming speed attained in one trial, in mm/sec.}
#'   }
#'   
#' @docType data
#' @name Tadpoles
#' @usage data(Tadpoles)
#'
#' @keywords datasets
#'

#' @examples
#' mod_1 = lm(vmax ~ poly(rtemp, 2) * group + length, data = Tadpoles)
"Tadpoles"
