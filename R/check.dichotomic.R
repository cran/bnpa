#'Verify if the variables of a dataset are dichotomic.
#'
#'Scan each variable of a dataset and verify if any of it is dichotomic.
#'
#'@param data.to.work is a data frame containing the variables to be checked.
#'@details
#'This function receives a dataset, scan each variable of it, and verify if any variable is dichotomic or not. If 'yes' then the function return TRUE.
#'@return TRUE or FALSE
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load package
#'library(bnpa)
#'# Use working datasets from package
#'data.to.work <- dataQuantC 
#'head(data.to.work)
#'###################################################################################
#'# check.dichotomic.R - Verify if some variables of a dataset is dichotomic
#'###################################################################################
#'# Show the structure of dataset
#'str(data.to.work)
#'# dataset has not dichotomic variables and function will return FALSE
#'check.dichotomic(data.to.work)
#'# Adding dichotomic data to data.to.work
#'data.to.work$Z <- round(runif(500, min=0, max=1),0)
#'# Show the structure of dataset
#'str(data.to.work)
#'# No dataset has dichotomic variables and function will return TRUE
#'check.dichotomic(data.to.work)
#'@export

check.dichotomic <- function(data.to.work) {

  variables <- names(data.to.work)

  for(x in 1: length(variables))
  {
    commandAssign <- paste("length(unique(data.to.work$", variables[x],"))", sep = "")
    if ((eval(parse(text=commandAssign))) == 2)
    {
      return(TRUE)
      break
    }
  } # for(x in 1: length(variables))
  return(FALSE)

} # gera.dichotomic<-function
