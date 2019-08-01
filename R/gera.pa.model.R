#'Generates PA input model
#'
#'This function is called from 'gera.pa' function. It receives a BN structure and a data set, build a PA input model string based on BN structure and return it.
#'@param bn.structure is a BN structure learned from data.
#'@param data.to.work is a data set containing the variables of the BN.
#'@return the PA input modeo string
#'@author Elias Carvalho
#'@references Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2),1-36.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("To your working directory")
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# load data sets from package
#'data(dataQualiN)
#'# Show first lines
#'head(dataQualiN)
#'# Learn BN structure
#'bn.structure <- hc(dataQualiN)
#'bnlearn::graphviz.plot(bn.structure)
#'# Set variables
#'# Generates the PA model from bn structure
#'pa.model <- gera.pa.model(bn.structure, dataQualiN)
#'pa.model
#'@export

gera.pa.model <-function(bn.structure, data.to.work)
{
  # Starts working variables
  paModel <-""

  # Show a message
  cat ("\nMounting a PA input model...\n") ################### cat

  # Creates a counter for the coefficients
  number.of.coefficient <- 0

  # Scan all columns of data.to.work to mount a PA input model
  for (x in 1:ncol(data.to.work))
  {
    # Scan all parents of a variable
    for (y in 1:length(bnlearn::parents(bn.structure,names(data.to.work[x]))))
    {
      # If lenght of parents is zero, means the variable does not goes to the model
      if (length(bnlearn::parents(bn.structure,names(data.to.work[x]))) != 0)
      {
        # If is the first element, build the main variable followed by "~" and first parent
        if (y == 1)
        {
          number.of.coefficient <-  number.of.coefficient + 1
          paModel <- noquote(paste(paModel, "\n", names(data.to.work[x]), " ~ ", paste('c',number.of.coefficient,'*', sep = ""), bnlearn::parents(bn.structure,names(data.to.work[x]))[y]))
        } # if (y == 1)
        else
        {
          number.of.coefficient <-  number.of.coefficient + 1
          # for second, third..., element just add "+" and the next parent
          paModel <- noquote(paste(paModel, "+", paste('c', number.of.coefficient,'*', sep = ""), bnlearn::parents(bn.structure,names(data.to.work[x]))[y]))
        }
      } # if (length(parents(bn.structure,names(data.to.work[x]))) != 0)
    } # for (y in 1:length(parents(bn.structure,names(data.to.work[x]))))
  } # for (x in 1:ncol(bn.structure, data.to.work))
  # Return the new data set
  return(paModel)
} # gera.pa.model <-function
