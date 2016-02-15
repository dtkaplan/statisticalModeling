#' Functions for constructing ggplot command strings
#'
frame_string <- function(data = NULL, x = NULL, y = NULL) {
  ystring <- ifelse(is.null(y), "", paste0(", y = ", y) )
  res <- sprintf("ggplot(%s, aes(x = %s%s))", data, x, ystring)
  res
}

density_string <- function(data_name = NULL, var_names, type, xframe, position, color, fill, ...){
  one <- frame_string(data = data_name, xframe)
  two <- layer_string(var_names, geom = type, position = position, color=color, fill=fill, ...)
  paste(one, two, sep = "+")
}

map_string <- function(location, zoom, source, type) {
  sprintf( "ggmap::ggmap(ggmap::get_map(location = '%s', zoom = %d, source = '%s', maptype = '%s', crop=FALSE))",
           location, zoom, source, type)
}

# variable for facetting as a character string
facet_string <- function(var) {
  if (var == "") ""
  else sprintf("+ facet_wrap( ~ %s)", var)
}

legend_position_string <- function(where) {
  if (where == "") return("")
  sprintf("+ theme(legend.position='%s')", where)
}
# The aesthetics for each supported geom
.aesthetics. <- list(
  point = list(x = NULL, y=NULL, alpha = NULL, color = NULL, colour = NULL,
        fill = NULL, shape =  NULL, size = NULL, stroke = NULL),
  line = list(x = NULL, y=NULL, alpha=NULL, group = NULL, color = NULL, colour=NULL, linetype=NULL, size=NULL),
  path = list(x = NULL, y=NULL, alpha=NULL, group = NULL, color = NULL, colour=NULL, linetype=NULL, size=NULL),
  boxplot = list(x = NULL, y=NULL, alpha=NULL, group = NULL, fill = NULL,
                 color = NULL, colour=NULL, linetype=NULL, size=NULL),
  density = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL),
  freqpoly = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL),
  histogram = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL)

)

model_string <- function(which) {
  res <- switch(which,
         none = "",
         linear = " + stat_smooth(method=lm, se=FALSE)",
         smoother = " + stat_smooth(method=loess, se=FALSE)",
         "linear+bands" = " + stat_smooth(method=lm, se=TRUE, level=0.95)",
         "smoother+bands" = " + stat_smooth(method=loess, se=TRUE, level=0.95)"
         )

  res
}

log_axes_string <- function(which = c("none", "both", "x", "y")) {
  if (which == "none") return("")
  x <- "scale_x_log10()"
  y <- "scale_y_log10()"
  if (which == "both")
    return(paste("", x, y, sep = " + "))
  if (which == 'x')
    return(paste0(" + ", x))
  if (which == "y")
    return(paste0(" + ", y))
}

layer_string <- function(var_names, geom = "point", ...) {
  map_list <- list()
  set_list <- list()
  candidates <- list(...)
  aes <- .aesthetics.[[geom]]
  aes_names <- names(aes)
  candidate_names <- names(candidates)
  if (length(candidates) > 0) {
    for (k in 1:length(candidates)) {
      if (candidates[[k]] == "") next # skip it!
      if ( ! candidate_names[k] %in% c(aes_names, "data")){
        warning("<", candidate_names[k],"> not in geom_", geom)
      } else if (candidates[[k]] %in% var_names) {
        map_list[candidate_names[k]] <- candidates[[k]]
      } else {
        # for set_list
        val <- candidates[[k]]
        as_number <- suppressWarnings(as.numeric(val))
        if (is.na(as_number)) { # val should be quoted
          val <- paste0("'", val, "'")
        } else {
          val <- as_number
        }
        set_list[candidate_names[k]] <-
          ifelse(candidate_names[[k]] == "data",
                 candidates[[k]],
                 val)
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
