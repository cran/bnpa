#'Verify types of variable.
#'
#'Verify what are the types of variables the dataset has.
#'
#'@param data.to.work is a data frame containing the variables to be verified.
#'@param message is a parameter indicating if the function will or not show a message.
#'@details
#'This function receives a dataset as parameter and check each type of variable returning a number indicating the type oa variables. 
#'The variables can be 1=integer, 2=numeric, 3=factor, 4=integer and numeric, 5=integer and  factor, 6=numeric and factor or 7=integer, numeric and factor. 
#'@return A variable with the code indicating the type of variable and a message (or not).
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Use working datasets from package
#'data.to.work <- dataQuantC
#'head(data.to.work)
#'####################################################################################
#'# check.types.R - Verify types of variable.
#'####################################################################################
#'#Check and return a numeric value
#'check.types(data.to.work, message = 1)
#'# Adding random data to data.to.work, function will return TRUE
#'data.to.work$Z <- round(runif(500, min=0, max=1000),2)
#'# Converting the numeric variable into factor
#'data.to.work$Z <- factor(data.to.work$Z)
#'# Check and return a numeric value correspondig to: 1=integer, 2=numeric, 3=factor, 4=integer and
#'# numeric, 5=integer and  factor, 6=numeric and factor or 7=integer, numeric and factor.
#'check.types(data.to.work, message = 1)
#'# Supressing the message
#'check.types(data.to.work, message = 0)
#'@export

check.types <- function(data.to.work, message=0) {
  # creates a veCtor of 4 position to identify types: position 1=1 is character and return an error,
  # 2=1 is integer, 3=1 is numeric, 4=1 is factor.
  typeVars <- vector(mode = "numeric", length = 4)

  # the variables not numeric (factor or character) - variables are factor = variables are character
  typeVars[1] <- (length(sapply(data.to.work, is.numeric)[sapply(data.to.work, is.numeric)==FALSE]) -
                    length(sapply(data.to.work, is.factor)[sapply(data.to.work, is.factor)==TRUE])
  )

  # verify integer
  typeVars[2] <- length(sapply(data.to.work, is.integer)[sapply(data.to.work, is.integer)==TRUE])

  # verify numeric
  typeVars[3] <- (length(sapply(data.to.work, is.numeric)[sapply(data.to.work, is.numeric)==TRUE]) -
                    length(sapply(data.to.work, is.integer)[sapply(data.to.work, is.integer)==TRUE])
  )

  # verify factor
  typeVars[4] <- length(sapply(data.to.work, is.factor)[sapply(data.to.work, is.factor)==TRUE])

  # create a variable to register what type of variable we have
  result.type <- 0

  # check what we have
  if (typeVars[1] > 0) # There are some character var
  {

    stop("Check your dataset! Different type of variable found. Must be integer, numerical or factor")

  } else if (typeVars[2] >  0 &&  # only integer
             typeVars[3] == 0 &&
             typeVars[4] == 0
  )
  {
    if (message==1) cat("\n Your dataset had variables of type integer\n\n")
    result.type <- 1 # only integer
  } else if (typeVars[2] == 0 &&
             typeVars[3] >  0 && # only numeric
             typeVars[4] == 0
  )
  {
    if (message==1) cat("\n Your dataset had variables of type numeric\n\n")
    result.type <- 2 # only numeric
  } else if (typeVars[2] == 0 &&
             typeVars[3] == 0 &&
             typeVars[4] >  0)   # only factor
  {
    if (message==1) cat("\n Your dataset had variables of type factor\n\n")
    result.type <- 3 # only factor
  } else if (typeVars[2] >  0 &&  # integer and numeric
             typeVars[3] >  0 &&
             typeVars[4] == 0)
  {
    if (message==1) cat("\n Your dataset had variables of type integer and numeric\n\n")
    result.type <- 4 # integer and  numeric
  } else if (typeVars[2] >  0 &&  # integer and  factor
             typeVars[3] == 0 &&
             typeVars[4] >  0)
  {
    if (message==1) cat("\n Your dataset had variables of type integer and factor\n\n")
    result.type <- 5 # integer and  factor

  } else if (typeVars[2] == 0 &&  # numeric and factor
             typeVars[3] >  0 &&
             typeVars[4] >  0)
  {
    if (message==1) cat("\n Your dataset had variables of type numeric and factor\n\n")
    result.type <- 6 # numeric and factor

  } else if (typeVars[2] >  0 &&  # integer, numeric and factor
             typeVars[3] >  0 &&
             typeVars[4] >  0)
  {
    if (message==1) cat("\n Your dataset had variables of type integer, numeric and factor\n\n")
    result.type <- 7 # integer, numeric and factor
  }

  return(result.type)

} # check.types <-function(data.to.work)
