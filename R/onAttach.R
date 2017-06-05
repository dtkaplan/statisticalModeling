.onAttach <- function(libname, pkgname) {
  if (! require(ggformula)) 
    packageStartupMessage("Install ggformula package from github: projectMOSAIC/ggformula")
}