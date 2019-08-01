#'Verify types of variable
#'
#'This function receives a data set as parameter and check each type of variable returning a number indicating the type of variables in the whole data set.
#'The variables can be 1=integer, 2=numeric, 3=factor, 4=integer and numeric, 5=integer and  factor, 6=numeric and factor, 7=integer,
#'numeric and factor, 8=character.
#'@param data.to.work is a data set containing the variables to be verified.
#'@param show.message is a parameter indicating if the function will or not show a message.
#'@return A variable with the code indicating the type of variable and a message (or not)
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
#'data(dataQuantC)
#'# Show first lines of data set
#'head(dataQuantC)
#'# Check and return a numeric value
#'show.message <- 1
#'bnpa::check.types(dataQuantC, show.message)
#'# Adding random data to dataQuantC, function will return TRUE
#'dataQuantC$Z <- round(runif(500, min=0, max=1000),2)
#'# Converting the numeric variable into factor
#'dataQuantC$Z <- factor(dataQuantC$Z)
#'# Check and return a numeric value correspondig to: 1=integer, 2=numeric, 3=factor, 4=integer and
#'# numeric, 5=integer and  factor, 6=numeric and factor or 7=integer, numeric and factor.
#'show.message <- 1
#'bnpa::check.types(dataQuantC, show.message)
#'# Supressing the message
#'show.message <- 0
#'bnpa::check.types(dataQuantC, show.message)
#'@export

check.types <- function(data.to.work, show.message=0)
{
  # creates a vector of 4 position to identify types: position 1=1 is character,
  # 2=1 is integer, 3=1 is numeric, 4=1 is factor.
  typeVars <- vector(mode = "numeric", length = 4)
  # verify character
  typeVars[1] <- length(sapply(data.to.work, is.character)[sapply(data.to.work, is.character)==TRUE])

  # verify integer
  typeVars[2] <- length(sapply(data.to.work, is.integer)[sapply(data.to.work, is.integer)==TRUE])

  # verify numeric
  typeVars[3] <- (length(sapply(data.to.work, is.numeric)[sapply(data.to.work, is.numeric)==TRUE]) -
                    length(sapply(data.to.work, is.integer)[sapply(data.to.work, is.integer)==TRUE]))

  # verify factor
  typeVars[4] <- length(sapply(data.to.work, is.factor)[sapply(data.to.work, is.factor)==TRUE])

  # create a variable to register what type of variable we have
  result.type <- 0

  # check what we have
  if (typeVars[1] > 0) # variables ara character only
  {
    if (show.message==1) cat("\n Your data set has variables of type character\n\n")
    result.type <- 8 # character
  } else if (typeVars[2] >  0 &&  # only integer
             typeVars[3] == 0 &&
             typeVars[4] == 0
  )
  {
    if (show.message==1) cat("\n Your data set has variables of type integer\n\n")
    result.type <- 1 # only integer
  } else if (typeVars[2] == 0 &&
             typeVars[3] >  0 && # only numeric
             typeVars[4] == 0
  )
  {
    if (show.message==1) cat("\n Your data set has variables of type numeric\n\n")
    result.type <- 2 # only numeric
  } else if (typeVars[2] == 0 &&
             typeVars[3] == 0 &&
             typeVars[4] >  0)   # only factor
  {
    if (show.message==1) cat("\n Your data set has variables of type factor\n\n")
    result.type <- 3 # only factor
  } else if (typeVars[2] >  0 &&  # integer and numeric
             typeVars[3] >  0 &&
             typeVars[4] == 0)
  {
    if (show.message==1) cat("\n Your data set has variables of type integer and numeric\n\n")
    result.type <- 4 # integer and  numeric
  } else if (typeVars[2] >  0 &&  # integer and  factor
             typeVars[3] == 0 &&
             typeVars[4] >  0)
  {
    if (show.message==1) cat("\n Your data set has variables of type integer and factor\n\n")
    result.type <- 5 # integer and  factor

  } else if (typeVars[2] == 0 &&  # numeric and factor
             typeVars[3] >  0 &&
             typeVars[4] >  0)
  {
    if (show.message==1) cat("\n Your data set has variables of type numeric and factor\n\n")
    result.type <- 6 # numeric and factor

  } else if (typeVars[2] >  0 &&  # integer, numeric and factor
             typeVars[3] >  0 &&
             typeVars[4] >  0)
  {
    if (show.message==1) cat("\n Your data set has variables of type integer, numeric and factor\n\n")
    result.type <- 7 # integer, numeric and factor
  }
  # Return the type of variable
  return(result.type)
} # check.types <-function(data.to.work)
