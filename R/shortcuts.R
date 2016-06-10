# Helper function to build the specific types
.gf_factory <- function(type, details=NULL) {
  res <-
    function(data=NULL, formula=NULL, add=FALSE, verbose = TRUE, 
             system = "ggplot2", ...) {
      data_name <- as.character(substitute(data))
      
      # Eventually, test whether <system> is ggplot2 and translate accordingly
      
      gg_command_string <-
        formula_to_gg(data=data, formula=formula, add=add,
                      geom=type, .use_name = data_name,
                      details = details, ...)
      if (verbose) cat(gsub("+", "+\n", gg_command_string, fixed = TRUE), "\n")
      eval(parse(text = gg_command_string))
    }
  
  res
}



