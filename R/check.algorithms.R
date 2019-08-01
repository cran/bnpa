#'Verifies the BN learning algorithms
#'
#'This function receives a list of algorithms of bnlearn package and check if it would be used in bnpa package.
#'@param bn.learn.algorithms is a list of algorithms (present in bnlearn package) to be used in BN structure learning pocess in bnpa.
#'@return NULL
#'@author Elias Carvalho
#'@references Scutari M (2017). Bayesian Network Constraint-Based Structure Learning Algorithms: Parallel and Optimized Implementations in the bnlearn R Package. Journal of Statistical Software, 77(2), 1-20.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("~/your working directory")
#'# Load packages
#'library(bnpa)
#'# Set what BN learning algorithms will be used
#'bn.learn.algorithms <- c("gs", "hc")
#'# Check these algorithms
#'check.algorithms(bn.learn.algorithms)
#'@export

check.algorithms <-function(bn.learn.algorithms)
  {
  # Show a message
  cat("\n\nVerifying the BN learn algorithms...")
  # Print a list of algorithms
  print(bn.learn.algorithms)
  # Check each algorithm
  for (w in 1:length(bn.learn.algorithms))
  {
    if (bn.learn.algorithms[w] != "gs" &&
        bn.learn.algorithms[w] != "iamb" &&
        bn.learn.algorithms[w] != "inter.iamb"&&
        bn.learn.algorithms[w] != "fast.iamb"&&
        bn.learn.algorithms[w] != "hc"&&
        bn.learn.algorithms[w] != "tabu")
    {
      # Show a message in case of not allowed algorithms
      stop("\nThe BN learning algorithm algorithm '", bn.learn.algorithms[w],"' is not allowed, only 'gs','iamb','inter.iamb','fast.iamb', 'hc', and 'tabu' ! Please review your code.")
    } else # if (bn.learn.algorithms[w] != "gs" &&
    {
      cat("\nAlgorithm ", bn.learn.algorithms[w],"is OK")
	  } # else # if (bn.learn.algorithms[w] != "gs" &&
  } # for (w in 1:length(bn.learn.algorithms))
} # gera.bn.pa <-function
