#'Check the levels of a categorical variable
#'
#'This function receives a data set and a variable name, check the type of variable to be sure it is categorical (factor) and then count the number of levels it has.
#'@param data.to.work is a data set with variable.
#'@param variable.name is the name of variable to be checked.
#'@return NULL
#'@author Elias Carvalho
#'@references GUJARATI, Damodar N. Basic econometrics. Tata McGraw-Hill Education, 2009.
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
#'# Adding random data to dataQualiN, function will return TRUE
#'dataQualiN$Z <- round(runif(500, min=0, max=1000),2)
#'# Converting the numeric variable into factor
#'dataQualiN$Z <- factor(dataQualiN$Z)
#'# Set the variable name to a non categorical one
#'variable.name = "Z"
#'# Count the number o levels of a specific variable
#'number.of.levels <- check.levels.one.variable(dataQualiN, variable.name)
#'number.of.levels
#'# Set the variable name to a categorical variable
#'variable.name = "A"
#'# Count the number o levels of a specific variable
#'number.of.levels <- check.levels.one.variable(dataQualiN, variable.name)
#'number.of.levels
#'@export

check.levels.one.variable <- function (data.to.work, variable.name)
{
  # Check if variable is factor
  if (bnpa::check.type.one.var(data.to.work, show.message=0, variable.name) != 3)
  {
	# Show a message in case of not factor variables
	cat ("\nYour variable:", variable.name," does not looks to be factor, please verify !")
    return()
  } else # if (bnpa::check.type.one.var(data.to.work, show.message=0, variable.name != 3)
  {
    number.of.levels <- 0
    # Mount a command to calculate the number of levels
    commandAssign <- (paste("number.of.levels <- nlevels(data.to.work$", variable.name,")", sep = ""))
    eval(parse(text=commandAssign))
    # Return the number of levels
    return(number.of.levels)
  } #  else # if (bnpa::check.type.one.var(data.to.work, show.message=0, variable.name != 3)
} # check.levels.one.variable <- function
