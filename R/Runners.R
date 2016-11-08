#' Performance of runners in a ten-mile race as they age
#' 
#' These data are assembled from the records of the "Cherry Blossom Ten Miler" 
#' held in Washington, DC each April. The records span the years 1999 to 2008.
#' 
#' There are about 10,000 participants each year, of whom roughly half have run the race previously. During the
#' ten years of these records, some 25 runners ran in ech of the years, 73 ran in nine of the years, and so on.
#' The data allow you to track the performace of runners as they age. This simplified version of the data does
#' not include personal identifying data other than sex, age, and number of previous runs in any given year. (Runs
#' before 1999 are not considered.)
#' 
#' @format A data frame \code{Runners} with 24,334 rows and 8 variables. 
#'   \itemize{
#'     \item{\code{age}} {The runners age at the time of the race.}
#'     \item{\code{net}} {The time (in min.) elapsed from when the runner crossed the start line until the finish.}
#'     \item{\code{gun}} {The time (in min.) from when the starter's gun was fired to when the runner finished the race. With
#'     so many participants, some runners do not reach the start line until several minutes after the gun.}
#'     \item{\code{sex}} {The runner's sex.}
#'     \item{\code{previous}} {How many times the runner participated before this year's race.}
#'     \item{\code{nruns}} {How many of the 10 years' runs the runner eventually participated in.}
#'     \item{\code{start_position}} {A made-up categorical description of where the runner was situated in the 
#'     line up for the race..}
#'   }
#'   
#' @docType data
#' @name Runners
#' @usage data(Runners)
#'
#' @keywords datasets
#'

#' @examples
#' mod_1 = lm(net ~ age + sex, data = Runners)
"Runners"
