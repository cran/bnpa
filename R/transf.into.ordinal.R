#'Transform categorical variables into ordinal
#'
#'This function receives a data set with categorical variables, scan all variables and transform it into odered factors.
#'@param data.to.work is a data set where all variables will be transformed into odered factors.
#'@return The data set transformed
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
#'#Load Data
#'data(dataQualiN)
#'# Transform all variables into ordinal
#'dataQualiN <- bnpa::transf.into.ordinal(dataQualiN)
#'str(dataQualiN)
#'@export

transf.into.ordinal <-function(data.to.work){

  # Scan all variables into data set
  for (var.name in names(data.to.work))
  {
    # Verify if the variable is categorical (factor)
    if(bnpa::check.type.one.var(data.to.work, "", var.name) == 3)
    {
      # check if the categorical variable is no ordered
      if (!bnpa::check.ordered.one.var(data.to.work , var.name))
      {
        # Mount a command to transform the variables
        commandAssign <- paste("data.to.work$",var.name , " <- as.ordered(data.to.work$", var.name , ")", sep = "")
        # Execute the command
        eval(parse(text=commandAssign))
      } # if (!bnpa::check.ordered.one.var(data.to.work , var.name))
    } else # if((bnpa::check.type.one.var(data.to.work, "", var.name) == 3
    {
      cat("Your data is not categorical ! Review the process please !")
    }
  } # for (var.name in names(data.to.work))

  return (data.to.work)

} # transf.into.ordinal <-function
