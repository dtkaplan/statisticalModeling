# Functions for constructing ggplot command strings
#
# Internals: nothing to export here!


frame_string <- function(data = NULL, x = NULL, y = NULL, aes_pairs = NULL) {
  ystring <- ifelse(is.null(y), "", paste0(", y = ", y) )
  for (nm in names(aes_pairs)) 
    ystring <- paste0(ystring, ", ", nm, " = ", aes_pairs[[nm]] )
  res <- sprintf("ggplot(%s, aes(x = %s%s))", data, x, ystring)
  res
}

density_string <- function(data_name = NULL, var_names, type, xframe, position, color, fill, ...){
  one <- frame_string(data = data_name, xframe)
  two <- layer_string(var_names, geom = type, position = position, color=color, fill=fill, ...)
  paste(one, two, sep = "+")
}

map_string <- function(map_name, location, zoom, source, type, extent = "normal", ...) {
  map_source <- if (length(map_name) == 0) {
    sprintf( "ggmap::get_map(location = '%s', zoom = %d, source = '%s', maptype = '%s', crop=FALSE)",
             location, zoom, source, type)
  } else {
    map_name
  }

  sprintf("ggmap::ggmap(%s, extent = '%s')", map_source, extent)
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
  violin = list(x = NULL, y=NULL, alpha=NULL, group = NULL, fill = NULL,
                color = NULL, colour=NULL, linetype=NULL, size=NULL),

  # single variable plot types
  density = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL),
  freqpoly = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL),
  histogram = list(x = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL),
  bar = list(x = NULL, y = NULL, alpha = NULL, fill = NULL, color = NULL, colour = NULL, position = NULL)

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

layer_string <- function(var_names, geom = "point", extras = "", details = NULL,
                         data_name = NULL, formula = NULL, ...) {
  map_list <- map_list <- list()
  if ( ! is.null(formula)) {
    nms <- all.vars(formula)
    map_list$x = nms[2]
    map_list$y = nms[1]
  }

  set_list <- list()
  if ( ! is.null(data_name)) {
    set_list$data = data_name
  }
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
  set_list <- c(set_list, details)
  set_string <- paste(collapse = ", ",
                      paste(names(set_list), set_list, sep = " = "))
  map_string <- paste0("aes(",
                        paste( collapse = ", ",
                               paste(names(map_list), map_list, sep = " = ")
                        ), ")")
  res <- paste0("geom_", geom, "(",
         ifelse(length(map_list) > 0, map_string, ""),
         ifelse(length(map_list) > 0 & length(set_list) != 0, ", ", ""),
         ifelse (length(set_list) > 0, set_string, ""),
         ifelse (extras == "", "", paste(",", extras)),
         ")"
  )

  res
}


capture_extras <- function(...) {
  extras <- list(...)
  res <- ""
  values = list()
  if (length(extras) > 0) {

    for (k in seq_along(extras)) {
      as_number <- suppressWarnings(as.numeric(extras[[k]]))
      if (is.na(as_number)) { # val should be quoted
        values[k] <- paste0("'", extras[[k]], "'")
      } else {
        values[k] <- as_number
      }
    }
    res <- paste(paste(names(extras), unlist(values), sep = " = "), collapse = ", ")
  }
  res
}

# Take a character string of possibilities and prepend it with NA
add_NA <- function(levels) {
  res <- as.list(levels)
  names(res) <- levels

  c(list(none = ""), res)
}

add_arg_list_to_function_string <- function(S, extras) {
  empty <- grepl("\\(\\s?\\)$", S)
  res <- if (length(extras) == 0 ) { 
    S
  } else {
    more <- paste0(names(extras), " = ", unlist(extras), collapse = ", ")
    S <- gsub("\\)$", "", S)
    paste0(S, ifelse(empty, "", ", "), more, ")")
  }
 
  res
}

#' @export
fg_generic <- function(placeholder = NULL, formula = NULL, data = NULL, 
                       extras = list(), geom = "geom_point", ... ) {
  data_name <- as.character(substitute(data))
  if (inherits(placeholder, c("gg", "ggplot"))) {
    # things are already set up
  } else if (inherits(placeholder, "formula")) {
    formula <- placeholder
    placeholder <- NULL
  }
  
  gg_string <- fg_master(formula = formula, data = data, 
                         geom = geom, gg_object = placeholder, 
                         extras = extras, data_name = data_name)
  gg_string
}





#' @export
# this will be internal
fg_master <- function(formula = NULL, data = NULL, add = FALSE, 
                      data_name = NULL, 
                      geom = "geom_point", extras = list(),
                      gg_object = NULL) {
  
  data_string <- 
    if (is.null(data)) ""
  else paste("data =", data_name)
  
  if ( (! add) && is.null(data) )
    stop("Must provide a frame or a data argument for a frame.")
  
  var_names <- 
    if (is.null(data)) {
      if (is.null(gg_object)) {
        character(0)
      } else {
        names(gg_object$data)
      }
    } else {
      names(data)
    }
  # arguments for the frame or, if add == TRUE, for the geom
  main_arguments <- 
    formula_to_aesthetics(formula, var_names, 
                          prefix = data_string)
  
  gg_string <-
    if (add) { # don't need the ggplot() call
      add_arg_list_to_function_string(
        paste0(geom, main_arguments),
        extras)
    } else {
      paste0("ggplot", main_arguments, " + ", 
             # always add extras to geom string
             add_arg_list_to_function_string(
               paste0(geom, "()"),
               extras
             ))
    } 
  
  gg_string
}

#' @export 
# change the above to keep this internal
formula_to_df <- function(formula = NULL, data_names = character(0)) {
  if (is.null(formula)) 
    return(data.frame(role = character(0), 
                      var = character(0), 
                      map = logical(0)))
  fc <- as.character(formula)
  parts <- unlist(strsplit(fc, "+", fixed = TRUE))
  # trim leading blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)
  # identify the pairs
  pairs <- parts[grepl(":+", parts)]
  nonpairs <- parts[ ! grepl(":+", parts)] # the x- and y-part of the formula
  if (length(nonpairs) > 3) {
    warning("No role specified for ", 
            paste(nonpairs[-(1:3)], collapse = " and "))
  }
    res <- if (length(nonpairs) == 3) {
    list(y = nonpairs[[2]], x = nonpairs[[3]])
  } else if (length(nonpairs)) {
    list(x = nonpairs[[2]])
  }
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    res[this_pair[1] ] <- this_pair[2]
  }
  
  res <- data.frame(role = names(res), 
             var = unlist(res), 
             map = unlist(res) %in% data_names)
  row.names(res) <- NULL
  
  res
}
#' @export
formula_to_aesthetics <- function(formula, 
                                  data_names = NULL, 
                                  prefix = "") {
  df <- formula_to_df(formula, data_names)
  aes_substr <- 
    if (is.null(data_names) || is.null(formula)) {
      ""
    } else {
      paste0("aes(", 
             with(subset(df, map), 
                  paste(role, var, sep = " = ", collapse = ", ")),
             ")",
             ifelse(any( ! df$map), ", ", "") # prepare for more args
             )
    }
  S <- paste0("(", prefix, 
              ifelse(nchar(prefix) > 0, ", ", ""),
              aes_substr, 
              with(subset(df, ! map), 
                   paste(role, var, sep = " = ", collapse = ", ")),
              ")")
  S
}

# pull out the pairs from a formula like color::red + alpha:0.5
# return them as a named list
pairs_in_formula <- function(formula) {
  fc <- as.character(formula)
  parts <- unlist(strsplit(fc, "+", fixed = TRUE))
  # trim leading blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)
  # identify the pairs
  pairs <- parts[grep(":+", parts)]
  res <- list()
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    res[this_pair[1] ] <- this_pair[2]
  }
  res
}


