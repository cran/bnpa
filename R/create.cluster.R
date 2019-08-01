#'Create a Parallel Socket Cluster
#'
#'This function counts the number of cores of your computer processor and mount a parallel socket cluster. It always creates N-1 node to the cluster to let 1 core to the other tasks.
#'@return an object of class "cluster"
#'@author Elias Carvalho
#'@references R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
#'@examples
#'\dontrun{
#'## Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Use working data sets from package
#'data(dataQualiN)
#'# Start the cluster
#'cl <- bnpa::create.cluster()
#'# Set the number of replications
#'R=1000
#'# Set the algorithm to be used
#'algorithm="hc"
#'# Executes a parallel bootstrap process
#'data.bn.boot.strap=boot.strength(data=dataQualiN,R,algorithm,cluster=cl,
#'                                 algorithm.args=list(score="bic"),cpdag = FALSE)
#'# Release the cluster
#'parallel::stopCluster(cl)
#'}
#'@export

create.cluster <-function()
{
  # Calculate the number of cores
  no_cores <-parallel::detectCores() - 1
  # Initiate cluster
  cl <- parallel::makeCluster(no_cores)
  return(cl)
} # create.cluster <-function()
