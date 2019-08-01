#'Verify if one specific variable of a data set is an ordered factor
#'
#'Receives a data set, the name of a specific variable and verify if it is an ordered factor or not. If 'yes' then the function return TRUE.
#'@param data.to.work is a data set containing the variables to be checked.
#'@param var.name is the name of variable to be checked.
#'@return TRUE or FALSE
#'@author Elias Carvalho
#'@references HAYES, A F; PREACHER, K J. Statistical mediation analysis with a multicategorical independent variable. British Journal of Mathematical and Statistical Psychology, v. 67, n. 3, p. 451-470, 2014.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Use working data sets from package
#'data(dataQualiN)
#'head(dataQualiN)
#'# Transform variable A into ordered factor
#'dataQualiN$A <- ordered(dataQualiN$A)
#'# Check variable A and return TRUE
#'var.name <- "A"
#'check.ordered.one.var(dataQualiN, var.name)
#'# Check variable B and return FALSE
#'var.name <- "B"
#'check.ordered.one.var(dataQualiN, var.name)
#'@export

check.ordered.one.var <- function(data.to.work, var.name)
{
  # mount a comand to verify if variable is ordered and return true or false
  commandAssign <- paste("class(data.to.work$", var.name,") =='ordered'", sep = "")
  if (eval(parse(text=commandAssign))[1]) # Is ordered
  {
     return(TRUE)
  } else  # if ((eval(parse(text=commandAssign))) == 2)  # Is NOT ordered
  {
     return(FALSE)
  } # else if ((eval(parse(text=commandAssign))) == 2)
} # check.ordered.one.var <- function(data.to.work, var.name)
