#'Verify the type of one variable
#'
#'Receives a specific variable and return a number indicating its type. The variables can be 1 is integer, 2 is numeric, 3 is factor, 8 is character.
#'@param data.to.work is a data set containing the variables to be verified.
#'@param show.message is a parameter indicating if the function will or not show a message.
#'@param variable.name is the name of variable to be checked.
#'@return A variable with the code indicating the type of variable and a message (or not).
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
#'head(dataQuantC)
#'# Adding random data to dataQuantC, function will return TRUE
#'dataQuantC$Z <- round(runif(500, min=0, max=1000),2)
#'# Converting the numeric variable into factor
#'dataQuantC$Z <- factor(dataQuantC$Z)
#'# Check and return a numeric value correspondig to the variable type
#'# Set the variable name
#'variable.name = "A"
#'# identify the type
#'check.type.one.var(dataQuantC, show.message=0, variable.name)
#'# Set the variable name
#'variable.name = "Z"
#'# identify the type
#'check.type.one.var(dataQuantC, show.message=0, variable.name)
#'@export

check.type.one.var <- function(data.to.work, show.message=0, variable.name)
{
  # creates a vector of 4 position to identify types: position #1=1 is character,#2=1 is integer, #3=1 is numeric, #4=1 is factor.
  typeVars <- vector(mode = "numeric", length = 4)

  # 1 verify character
  commandAssign <- paste("typeVars[1] <- length(sapply(data.to.work$", variable.name,", is.character)[sapply(data.to.work$", variable.name,", is.character)==TRUE])", sep = "")
  eval(parse(text=commandAssign))

  # 2 verify integer
  commandAssign <- paste("typeVars[2] <- length(sapply(data.to.work$", variable.name,", is.integer)[sapply(data.to.work$", variable.name,", is.numeric)==TRUE])", sep = "")
  eval(parse(text=commandAssign))

  # 3 verify numeric
  commandAssign <- paste("typeVars[3] <- length(sapply(data.to.work$", variable.name,", is.numeric)[sapply(data.to.work$", variable.name,", is.numeric)==TRUE])", sep = "")
  eval(parse(text=commandAssign))

  # 4 Verify factor
  commandAssign <- paste("typeVars[4] <- length(sapply(data.to.work$", variable.name,", is.factor)[sapply(data.to.work$", variable.name,", is.factor)==TRUE])", sep = "")
  eval(parse(text=commandAssign))

  # create a variable to register what type of variable we have
  result.type <- 0

  # check what we have
  if (typeVars[1] > 0) # is character
  {
    result.type <- 8
  } else if (typeVars[2] >  0) # is integer
  {
    result.type <- 1

  } else if (typeVars[3] >  0) # is numeric
  {
    # verify if is dichotomic
    if (check.dichotomic.one.var(data.to.work, variable.name) == TRUE)
    {
      result.type <- 9

    }  else # if (check.dichotomic.one.var(data.to.work, variable.name) == TRUE)
    {
      result.type <- 2 # only numeric

    } #  else # if (check.dichotomic.one.var(data.to.work, variable.name) == TRUE)
  } else if (typeVars[4] >  0) # is factor
  {
    result.type <- 3 # factor
  }

  # Return the type of variable
  return(result.type)

} # check.type.one.var <-function(data.to.work)
