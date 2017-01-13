#'Learn the Bayesian Network Parameters from a Bayesian Network structure
#'
#'Receives a Bayesian Network (BN) structure, a dataset and learn the parameters of BN.
#'
#'@param bn.structure is a Bayesian Networks structure learned from data.
#'@param data.to.work is a data frame containing the variables of the Bayesian Networks learned.
#'@param param.name is a name of text file to be saved with parameters of BN.
#'@details
#'This funcion receives a Bayesian Networks structure and a dataset, then learn the parameters 
#'of each node connection with 'bn.fit' function of bnlearn package. Finally it exports the 
#'Bayesian Networks parameters as a text file and return this same object.
#'@return A set of parameters learned
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# load datasets from package
#'data.to.work<-dataQuantC
#'# Set the name of a text to save the Bayesian Networks parameters
#'param.name <- "docbnparamHC.txt"
#'# Learn a Bayesian Networks structure from data
#'bn.structure <- hc(data.to.work)
#'# Generates a Bayesian Networks parameters from the Bayesian Networks structure and 
#'# write a text file with this
#'bn.param <- gera.bn.param(bn.structure, data.to.work, param.name)
#'@export

gera.bn.param <-function(bn.structure, data.to.work, param.name) {

  # Check if needed packages are installed
  bnpa::check.package.files()

  # if continuous data, transform all int in numeric
  if (!plyr::is.discrete(data.to.work)) # data are continuous
  {
    data.to.work <- convert.continuous.int.to.numeric(data.to.work)
  } # if (!is.discrete(data.to.work))

  bn.param <- bnlearn::bn.fit(bn.structure, data.to.work)

  # Export parameters
  cat ("\nExport a Bayesian Networks parmeters...\n")

  pfile <- file( param.name, "w")
  sink(pfile)
  print(bn.param)
  sink()
  close(pfile)

  return(bn.param)
} # gera.bn.param <-function(data.to.work)
