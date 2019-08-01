#'Verify if one specific variable of a data set is dichotomic
#'
#'This function receives a data set and the name of a specific variable and verify if it is dichotomic or not. If 'yes' then the function return TRUE.
#'@param data.to.work is a data set containing the variables to be checked.
#'@param variable.name is the name of a variable to be checked.
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
#'data(dataQuantC)
#'head(dataQuantC)
#'# Show the structure of data set
#'str(dataQuantC)
#'# Set variable name
#'variable.name = "A"
#'# data set has not dichotomic variables and function will return FALSE
#'check.dichotomic.one.var(dataQuantC, variable.name)
#'# Adding dichotomic data to dataQuantC
#'dataQuantC$Z <- round(runif(500, min=0, max=1),0)
#'# Show the new structure of data set
#'str(dataQuantC)
#'# Set variable name
#'variable.name = "Z"
#'# Now data set has dichotomic variables and function will return TRUE
#'check.dichotomic.one.var(dataQuantC, variable.name)
#'@export

check.dichotomic.one.var <- function(data.to.work, variable.name)
{
  # Calculates the size of a variable the be sure it has 2 values.
  commandAssign <- paste("length(unique(na.omit(data.to.work$", variable.name,")))", sep = "")
  # Verifies the size of variable
  if ((eval(parse(text=commandAssign))) == 2)
   {
     return(TRUE)
  } else { # if ((eval(parse(text=commandAssign))) == 2)
     return(FALSE)
   } # else if ((eval(parse(text=commandAssign))) == 2)
} # check.dichotomic.one.var <- function(data.to.work, variable.name)
