#' Nitrogen fixing by alder plants
#'
#' These data were collected by biologist Mike Anderson in a study of
#' nitrogen fixation by bacteria growing on the root nodules of alder
#' bushes.  
#'
#' Two questions that Anderson wanted to answer are:
#' (1) Can any variation in nitrogen fixation (variable \code{SNF}) 
#' be attributed to genotype (variable \code{RF})? (2) What are 
#' the major sources of variation in \code{SNF} and
#' \code{PERLEAFN}? Variables of biological interest are 
#' seasonality (\code{SAMPPER} or \code{JULDAY}), 
#' soil temperature and moisture, and habitat differences (\code{STAGE} for 
#' host species AT and\code{STAGE} and \code{LAND} for host species AC).
#'
#' Three replicate sites were sampled for each landscape/stage combination in three sampling periods across the growing season. Site sampling was arranged in a Latin Square design in order to systematize any effects of seasonality on N2-fixation rates. 

#' @format A data frame \code{Alder} with 196 rows and 24 variables:
#' \itemize{
#' \item LAND. landscape, floodplain vs. upland.
#' \item SAMPPER. sampling period: early, mid, late
#' \item SPECIES. host species, Alnus tenuifolia (AT) vs. A. crispa (AC)
#' \item STAGE.  successional stage, early vs. late in 
#' floodplain and upland landscapes. This is not equivalent across landscapes.
#' \item JULDAY. Julian day
#' \item PERNODN. nodule percent nitrogen by mass
#' \item RF. bacterial genotype
#' \item SNF. nitrogen fixation rate of nodule tissue, umol N2/gram of nodule dry weight/hr
#' \item SLA. specific leaf weight, grams of leaf weight/square-meter, dry
#' \item ONECM. soil temperature at 1 cm depth
#' \item FIVECM. soil temperature at 5 cm depth
#' \item PERH2O. soil moisture, percent H2O by mass
#' \item DEL. del15N of leaf tissue
#' \item DELNOD. del15N of nodule tissue
#' \item NPERAREA. leaf nitrogen content per unit leaf area
#' \item NDiff. nitrogen content difference between leaf and nodule 
#' of the same plant
#' \item delDiff. del15N difference between leaf and nodule of the same plant
#' \item SITE. Site designations: 1A,B,C for replicate early succession floodplain sites, 4A,B,C for late succession floodplain, UP1A,B,C 
#' for early succession upland and UP3A,B,C for late succession upland
#' \item HABSPEC. habitat+species, concatenated LAND, STAGE, SPECIES
#' \item SITESPEC. concatenated SITE, SPECIES
#' \item REP. replicate site within a given level of HABSPEC
#' \item PLNO. plant number, unique for individuals of each species (AT1-180, AC1-270)
#' }


#'
#' @docType data
#' @name Alder
#' @usage data(Alder)
#'
#' @keywords datasets
#'
#' @references Anderson MD, Ruess RW, Myrold DD, Taylor DL. 
#' ``Host species and habitat affect modulation by specific 
#' Frankia genotypes in interior Alaska''  Oecologia (2009) 160:619-630.

#' @source Michael Anderson
#' 
#' @examples
#' mod <- lm(logSNF ~ RF + SITESPEC, data = Alder)
"Alder"
