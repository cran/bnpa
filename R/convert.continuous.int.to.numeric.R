#'Convert int data into numeric data.
#'
#'Convert variables of type int into numeric.
#'
#'@param data.to.work is a data frame containing the variables to be converted.
#'@details
#'This function receives a dataset as parameter and convert all variables with type int 
#'into numeric type.
#'@return A new data frame with numeric type variables.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Load datasets from package
#'data.to.work <- dataQuantC # Pre-Loaded'
#'head(data.to.work)
#'# Converting some variables to int justto test the function
#'data.to.work$A <- as.integer(data.to.work$A )
#'data.to.work$C <- as.integer(data.to.work$C )
#'data.to.work$G <- as.integer(data.to.work$G )
#'data.to.work <- convert.continuous.int.to.numeric(data.to.work)
#'@export

convert.continuous.int.to.numeric <-function(data.to.work) {
  columnName <-""
  # mount a vector marking TRUE for variable which is factor
  f <- sapply(data.to.work, is.integer)

  # scan all lines of f and convert int to numeric
  for (x in 1:length(f))
  {
    # if variable is integer convert it to numeric
    if(f[[x]])
    {
      cmdChangeType <- paste("data.to.work$",names(data.to.work[x]), " <- as.numeric(data.to.work$",names(data.to.work[x]),")", sep="")
      eval(parse(text=cmdChangeType))
    }
  } # for (x in 1:ncol(data.to.work))

  # Return the dataset converted
  return(data.to.work)

} # convert.continuous.int.to.numeric <-function(data.to.work)
