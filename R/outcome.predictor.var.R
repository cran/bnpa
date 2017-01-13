#'Builds a black list to predictor
#'
#'Builds a black list from all others variables to a possible predictor or from a possible outcome to all others variavbles (the reverse). This procedure is to be used on a variable with characteristics of predictor or ouctome.
#'
#'@param data.to.work is a data frame containing the variables to build a list.
#'@param var.name is the outcome/predictor variable name.
#'@param type.var is a type of variable: <o>utcome or <p>redictor.
#'@param black.list is a black list sent, it would be empty or loaded.
#'@details
#'This function receives a dataset and a possible outcome/predictor variable. Then if this variable is classfied as an outcome variable it builds a black list from it to all other variable or from all other variables to it if is classified as a predictor variable. The parameter 'vlist' is a variable which will receive a black list. You can pass it with a previously list and then this function will append a new list in the end of it.
#'@return a black list with from - to variables
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# Load data sets from package
#'data1<-dataQuantC
#'###################################################################################
#'# mount.wl.bl.list.R - Mounts a white or black list.
#'###################################################################################
#'# Load data.work
#'data.to.work <- data1
#'head(data.to.work)
#'# Create an empty list or fill it before start
#'bl <- ""
#'# Setting the type of var as typical "outcome" type what means it will not point to any variable
#'type.var <- "o"
#'# Setting variable "A" as "outcome" will create a black from this variable to all others
#'var.name <- "A"
#'# Creating the black list
#'bl <- outcome.predictor.var(data.to.work, var.name, type.var, bl)
#'bl
#'# Setting the type of var as typical "predictor" type what means it will not be point from
#'# any other variable
#'type.var <- "p"
#'# Setting variable "D" as "predictor" will create a blacklist from all others to it
#'var.name <- "D"
#'# Creating the black list
#'bl <- outcome.predictor.var(data.to.work, var.name, type.var, bl)
#'bl
#'# Setting variable "F" as "predictor" will create a blacklist from all others to it
#'var.name <- "F"  # setting this variable as a typically outcome
#'# Creating the black list
#'bl <- outcome.predictor.var(data.to.work, var.name, type.var, bl)
#'bl
#'# Setting variable "G" as "predictor" will create a blacklist from all others to it
#'var.name <- "G"  # setting this variable as a typically outcome
#'# Creating the black list
#'bl <- outcome.predictor.var(data.to.work, var.name, type.var, bl)
#'bl
#'# Mounting a black list in bnlearn sintax
#'bl <- mount.wl.bl.list(bl)
#'bl
#'# Learn the Bayesian Networks structure without black and white list
#'bn.structure <- hc(data.to.work, blacklist = bl)
#'# Set the name of a text to save the Bayesian Network parameters
#'param.name <- "docbnparamHC.txt"
#'# Generates a Bayesian Networks parameters from the Bayesian Networks structure and 
#'# write a text file with this
#'bn.param <- gera.bn.param(bn.structure, data.to.work, param.name)
#'# Set the graph name
#'graph.name <- "imgBNHC"
#'# Save the graph
#'mount.graph(bn.structure, bn.param, graph.name, data.to.work)
#'@export

outcome.predictor.var <-function(data.to.work, var.name, type.var, black.list) {
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

}
