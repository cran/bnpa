#'Check if the variables need to be ordered
#'
#'This function receives a data set and check the level of each factor variable, if they have more than 2 levels the function recommend to check the need to transform it to ordered factor.
#'@param data.to.work is a data set with variables to check.
#'@return TRUE or FALSE if need or not to tranform the variable into ordered factor.
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
#'# Show first lines of data set
#'head(dataQualiN)
#'# Insert categorical variables with more than 2 levels
#'dataQualiN$test.variable[dataQualiN$A == "yes"] <- "low"
#'dataQualiN$test.variable[dataQualiN$B == "yes"] <- "medium"
#'dataQualiN$test.variable[dataQualiN$X == "yes"] <- "high"
#'# Transform it to factor variable
#'dataQualiN$test.variable <- as.factor(dataQualiN$test.variable)
#'# Check the necessity to transform in ordered variables
#'bnpa::check.variables.to.be.ordered(dataQualiN)
#'@export

check.variables.to.be.ordered <- function (data.to.work)
{
  # Create a variable to alert if need transform o not
  need.transform <- FALSE
  # Scan all variables
  for (variable.name in names(data.to.work))
  {
    # Count the number o levels of a specific variable
    number.of.levels <- check.levels.one.variable(data.to.work, variable.name)
    # if it has more than 2 levels show an alert
    if (number.of.levels > 2)
    {
      cat("\nVariable:", variable.name, " probably would be categorical ordered because it has ", number.of.levels, " levels.\n Check if it is TRUE and in positive situation transform it before start the process !")
      need.transform <- TRUE
    } # if (check.levels.one.variable(data.to.work, variable.name) > 2)
  } # for (variable.name in names(data.to.work))
  # Return an alert
  return(need.transform)
} # check.levels.one.variable <- function
