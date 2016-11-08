#' Gene expression in cancer cells.
#'
#' The data come from a National Cancer Institute study of gene expression in cell lines 
#' drawn from various sorts of cancer. Each row corresponds to a different cell line. The type
#' of cancer is identified by the first two or three letters of the row names, e.g. CO is colon, ME is melanoma, RE is kidney.
#' 
#' For each row, there are 6000 measurements of the gene expression as identified by activity on
#' a microarray probe. The variable names are the names of the probes.
#' 
#' @format A data frame \code{NCI60_snippet} with 60 rows and 6000 variables. 



#'
#' @docType data
#' @name NCI60_snippet
#' @usage data(NCI60_snippet)
#'
#' @keywords datasets
#'

#' @examples
#' data(NCI60_snippet)
"NCI60_snippet"
