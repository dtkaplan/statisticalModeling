#' Functions for constructing ggplot command strings
#'
frame_string <- function(data = NULL, x = NULL, y = NULL) {
  ystring <- ifelse(is.null(y), "", paste0(", y = ", y) )
  res <- sprintf("ggplot(%s, aes(x = %s%s))", data, x, ystring)
  res
}

# The aesthetics for each supported geom
.aesthetics. <- list(
  point = list(x = NULL, y=NULL, alpha = NULL, color = NULL, colour = NULL,
        fill = NULL, shape =  NULL, size = NULL, stroke = NULL),
  line = list(x = NULL, y=NULL, alpha=NULL, group = NULL, color = NULL, colour=NULL, linetype=NULL, size=NULL),
  path = list(x = NULL, y=NULL, alpha=NULL, group = NULL, color = NULL, colour=NULL, linetype=NULL, size=NULL)
)

layer_string <- function(var_names, geom = "point", ...) {
  map_list <- list()
  set_list <- list()
  candidates <- list(...)
  aes <- .aesthetics.[[geom]]
  aes_names <- names(aes)
  candidate_names <- names(candidates)
  if (length(candidates) > 0) {
    for (k in 1:length(candidates)) {
      if ( ! candidate_names[k] %in% c(aes_names, "data")){
        warning("<", candidate_names[k],"> not in geom_", geom)
      } else if (candidates[[k]] %in% var_names) {
        map_list[candidate_names[k]] <- candidates[[k]]
      } else {
        set_list[candidate_names[k]] <-
          ifelse(candidate_names[[k]] == "data",
                 candidates[[k]],
                 paste0("'",candidates[[k]], "'"))
      }
    }
  }
  set_string <- paste(collapse = ", ",
                      paste(names(set_list), set_list, sep = " = "))
  map_string <- paste0("aes(",
                        paste( collapse = ", ",
                               paste(names(map_list), map_list, sep = " = ")
                        ), ")")
  paste0("geom_", geom, "(",
         ifelse(length(map_list) > 0, map_string, ""),
         ifelse(length(map_list) > 0 & length(set_list) != 0, ", ", ""),
         ifelse (length(set_list) > 0, set_string, ""),
         ")"
  )
}
