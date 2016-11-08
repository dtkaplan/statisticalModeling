#' Prices for life insurance 
#' 
#' The AARP (original named the "American Association of Retired People" 
#' but now just AARP) offers life insurance
#' to it's members. The data come from a full-page advertisement (circa 2012) in the "AARP
#' Bulletin", which has the second largest circulation in the world of any magazine, with
#' upward of 40 million subscribers. (Only the "AARP Magazine" has a larger circulation.)
#' 
#' Life insurance provides a "death benefit", money paid out to the insured person's survivors
#' upon death of the insured. There is a cost for the insurance. Among other factors, the cost
#' depends on both age and sex. (For this type of insurance, called "term insurance", the cost
#' changes as the insured person ages.) 
#' 
#' @docType data
#' @name AARP
#' @usage data(AARP)
#'
#' @keywords datasets
#' 
#' @source The "AARP Bulletin". 
#' A copy of the ad is available \href{http://tiny.cc/mosaic/AARP-insurance-ad.pdf}{at this link.}
#'
#' @format
#'   A data frame with 36 observations on the following variables.
#'   \itemize{
#'     \item{\code{Age}} {The age of the person covered by the insurance policy.}
#'     \item{\code{Sex}} {The sex of the person covered by the insurance policy.}
#'     \item{\code{Coverage}} {The "death benefit" in 1000 USD.}
#'     \item{\code{Cost}} {Monthly cost in USD.}
#'   }
#' 
#' @examples
#' mod_1 <- lm(Cost ~ Age + Coverage, data = AARP)
#' effect_size(mod_1, ~ Coverage)
"AARP"
