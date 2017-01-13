#'Return the type of score needed to be used with Bayesian Networks learning algorithms.
#'
#'Verify what kind of variable data has and return a type of score needed to be used with 
#'Bayesian Networks learning algorithms.
#'
#'@param type.variable A variable with the code of types: 1=integer, 2=numeric, 3=factor,
#'                     4=integer and numeric, 5=integer and  factor, 6=numeric and factor,
#'					   7=integer, numeric and factor.
#'@details
#'This function receives a variable with the type of data in a dataset and return a type
#'of score needed to learn the Bayesian Networks structure.
#'@return type of score.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'################################################################################################
#'# check.types.score.R - Return the type of score needed to be used with Bayesian Networks 
#'# learning algorithms.
#'################################################################################################
#'# Load quantitative data
#'data.to.work <- dataQuantC
#'head(data.to.work)
#'# Check and return a numeric value
#'type.variable <- check.types(data.to.work)
#'# Check the type of score
#'check.type.score(type.variable)
#'# Load qualitative data
#'data.to.work <- dataQualiN
#'head(data.to.work)
#'# Check and return a numeric value
#'type.variable <- check.types(data.to.work)
#'# Check the type of score
#'check.type.score(type.variable)
#'@export

check.type.score <- function(type.variable)
{
  # evaluate the type of variable to return a type of score
  if (type.variable == 3) # discrete variables
  {
    type.score = "loglik"
  } else if (type.variable >= 5 && type.variable <=7) # hybrid - mixed discrete and continuous variables
  {
    type.score = "loglik-cg"
  } else # continuous variables
  {
    type.score = "bge"
  }

  return(type.score)

} # check.type.score <- function
