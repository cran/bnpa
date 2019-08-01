#'Builds a black list of predictor and/or outcome variable
#'
#'This function receives a data set, an outcome/predictor variable, the type of variable and a black list. If this variable is classfied as type outcome the function builds a black list from it to all other variable. If it is classified as type predictor builds a list from all other variables to it. You can pass a previously black list and then this function will append a new list in the end of it.
#'@param data.to.work is a data set containing the variables to build a list.
#'@param var.name is the outcome/predictor variable name.
#'@param type.var is a type of variable: <o>utcome or <p>redictor.
#'@param black.list is a previous black list, it would be empty or loaded.
#'@return a black list with from - to variables
#'@author Elias Carvalho
#'@references KATZ, M H. Multivariable analysis: a primer for readers of medical research. Annals of internal medicine, v. 138, n. 8, p. 644-650, 2003.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("To your working directory")
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# Load data sets from package
#'data(dataQuantC)
#'# Show first lines
#'head(dataQuantC)
#'# Create an empty list or fill it before start
#'black.list <- ""
#'# Setting the type of var as typical "outcome" what means it will not point to any var
#'type.var <- "o"
#'# Setting variable "A" as "outcome" will create a black from this variable to all others
#'var.name <- "A"
#'# Creating the black list
#'black.list <- outcome.predictor.var(dataQuantC, var.name, type.var, black.list)
#'black.list
#'# Setting the type of var as typical "predictor" it will not be pointed from any other var
#'type.var <- "p"
#'# Setting variable "D" as "predictor" will create a blacklist from all others to it
#'var.name <- "D"
#'# Creating the black list
#'black.list <- outcome.predictor.var(dataQuantC, var.name, type.var, black.list)
#'black.list
#'@export

outcome.predictor.var <-function(data.to.work, var.name, type.var, black.list)
{
  # Scan all variables of data.to.work
  for (x in 1:length(names(data.to.work)))
  {
    # If the currently variable is not the outcome var
    if (names(data.to.work)[x] != var.name)
    {
      if (type.var == "o") # is typically outcome variable
      {
        # if the black list is empty add the black list at the begining
        if (black.list=="")
          black.list <- paste(var.name, "-",  names(data.to.work)[x], sep = "")
        else # if the black list is not empty add the black list at the end of currently bl
          black.list <- paste(black.list, ",", var.name, "-",  names(data.to.work)[x], sep = "")
      } else # # is typically predictor variable
      {
        # if the black list is empty add the black list at the begining
        if (black.list=="")
          black.list <- paste(names(data.to.work)[x], "-", var.name, sep = "")
        else # if the black list is not empty add the black list at the end of currently bl
          black.list <- paste(black.list, ",", names(data.to.work)[x], "-", var.name, sep = "")
      } # if (type.var == "o")
    } # if (names(data.to.work)[x] != var.name)
  } # for (x in 1:length(names(data.to.work)))
  return (black.list)
} # outcome.predictor.var <-function
