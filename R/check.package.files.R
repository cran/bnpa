#'Keep a list os packages necessary to bnpa.
#'
#'Check if all needed packages are installed.
#'
#'@details
#'This funcion keeps a list of packages that must be installed to the correct working of bnpa and call 'check.packages" function to check if needed packages are installed.
#'@return An error message if package not installed
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Check and install if not installed
#'check.package.files()
#'@export

check.package.files <- function()
{
  # see the list of packages on DESCRIPTION FILE
  packages.name.file <- c("bnlearn",
                          "gdata",
                          "lavaan",
                          "plyr",
                          "qgraph",
                          "Rgraphviz",
                          "semPlot"
                          )

  for (x in 1:length(packages.name.file))
  {
    check.package(packages.name.file[x])
  }

} # check.package.files <- function()
