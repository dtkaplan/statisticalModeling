#' Birth weights and maternal data 
#' 
#' Birth weight, date, and gestational period collected as part 
#' of the Child Health and Development Studies in 1961 and 1962.  
#' Information about the baby's parents --- age, education, height, 
#' weight, and whether the mother smoked is also recorded.  
#' The data were present by Nolan and Speed to address the question 
#' of whether there is a link between maternal smoking and the 
#' baby's health.
#' 
#' @docType data
#' @name Birth_weight
#' @usage data(Birth_weight)
#'
#' @keywords datasets
#' 
#' @source D. Nolan and T.P. Speed (2009) "Stat Labs: Mathematical
#' Statistics Through Applications"
#' @format
#'   A data frame with 886 observations on the following variables.
#'   \itemize{
#'     \item{\code{baby_wt}} {Birth weight of baby, in ounces.}
#'     \item{\code{mother_wt}} {in pounds}
#'     \item{\code{gestation}} {Length of the pregnancy, in days.}
#'     \item{\code{smoker}} {Whether the mother smoked during the pregnancy.}
#'     \item{\code{income}}{Family yearly income in 2500USD increments 0 = under 2500, 1=2500-4999, ..., 8= 12,500-14,999, 9=15000+}
#'   }
#' 
#' @examples
#' mod_1 <- lm(baby_wt ~ gestation + mother_wt, data = Birth_weight)
#' effect_size(mod_1, ~ gestation)
"Birth_weight"
