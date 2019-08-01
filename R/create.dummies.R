#'Creates dummy variables in the data set and remove master variables
#'
#'This function receives a data set and the name of variables to be transformed into dummies. Then it create the dummy variables, transform it into numeric (to work with PA generation) and
#'remove the master variables that originates the dummies.
#'@param data.to.work is a data set containing the variables to tranform.
#'@param dummy.vars are the variables to be transformed.
#'@return the new data set
#'@author Elias Carvalho
#'@references Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2),1-36.
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
#'# Show the structure before
#'str(dataQualiN)
#'# Set possible dummy variables
#'dummy.vars <- c("A", "B")
#'# Create dummies
#'dataQualiN <- bnpa::create.dummies(dataQualiN, dummy.vars)
#'# Show the structure before
#'str(dataQualiN)
#'@export

create.dummies <- function(data.to.work, dummy.vars)
{
  # Mount the begining of variable
  commandAssign <- paste("data.to.work <- fastDummies::dummy_cols(data.to.work, select_columns = c(")
  for (x in 1:length(dummy.vars))
  {
    # Add the variables
    commandAssign <- paste(commandAssign, "'", dummy.vars[x],"',", sep = "")

    # If is the last variable remove extra comma
    if (x==length(dummy.vars))
    {
      # Finalize the variable
      commandAssign <- paste(substr(commandAssign, 1, nchar(commandAssign)-1),"))", sep = "")
    } # if (x==length(dummy.vars))
  } # for (x in 1:length(dummy.vars))

  # Execute the command
  eval(parse(text=commandAssign))

  # Remove the master variables
  for (x in 1:length(dummy.vars))
  {
    # Mount the command to select the variable to be removed
    commandAssign <- paste("data.to.work$", dummy.vars[x],"<- NULL", sep = "")
    # Execute the command
    eval(parse(text=commandAssign))
  }
  # Return the new data set
  return(data.to.work)

} # create.dummies
