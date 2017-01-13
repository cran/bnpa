#'Check and install packages if necessary.
#'
#'Receives a package name as parameter, check it and install if necessary.
#'
#'@param package.name is the name of package to be verified.
#'@details
#'This funcion receives the name of package and check if it is not installed. If the package is not installed, it is done, except for "RGraphviz" and "qgraph" packages that have a different way to be installed.
#'@return an error message if package not installed.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Choose the package name
#'package.name <- c("gtools")
#'# Check and install if not installed
#'check.package(package.name)
#'@export

check.package <- function(package.name)
{
  if (!is.element(package.name, utils::installed.packages()))
  {

    print(c("========== Installing: ", package.name, "=========="), sep="")

    if (package.name == "Rgraphviz")
    {
      cat(paste("\nYou do not have the ", package.name," package installed. They are not on CRAN but on bioconductor, enter the commands below in the R console to install it"))
      cat("\nsource('http://bioconductor.org/biocLite.R')")
      cat("\nbiocLite(c('graph', 'RBGL', 'Rgraphviz'))")
    } # else if (package.name == 'Rgraphviz')
    else if (package.name == 'graph')
    {
      cat(paste("\nYou do not have the ", package.name," package installed. Please enter the commands below in the R console to install it"))
      cat("\nsource('http://bioconductor.org/biocLite.R')")
      cat("\nbiocLite('graph', suppressUpdates=TRUE)")

    } # else if (package.name == 'graph')
    else
    {

      utils::install.packages(package.name, verbose = FALSE, dependencies = TRUE)
      if (!is.element(package.name, utils::installed.packages())) stop("Package not found")

    } # else
  } # if (!is.element(package.name, installed.packages()))
} # check.package <- function(package.name)
