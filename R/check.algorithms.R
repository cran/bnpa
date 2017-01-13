#'Verifies the BN learning algorithms
#'
#'This function verifies if a list of BN learning algorithms passed has allowed algorithms.
#'@param bn.learn.algorithms is a list of algorithms (present in bnlearn) to be used in BN structure learning pocess.
#'@details
#'This function receives a list of algorithms and verifies if they are allowed to learn the BN structure.
#'@return NULL.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Set what BN learning algorithms will be used
#'bn.learn.algorithms <- c("hc", "mmhc")
#'# Check these algorithms
#'check.algorithms(bn.learn.algorithms)
#'# Changing for not allowed algorithms
#'bn.learn.algorithms <- c("tabu-search", "mhc")
#'# Check these algorithms
#'check.algorithms(bn.learn.algorithms)
#'@export

check.algorithms <-function(bn.learn.algorithms)
  {

  cat("\nVerifying the BN learn algorithms...")
  print(bn.learn.algorithms)

  for (w in 1:length(bn.learn.algorithms))
  {

    if (bn.learn.algorithms[w] != "hc" &&
        bn.learn.algorithms[w] != "mmhc" &&
        bn.learn.algorithms[w] != "rsmax" &&
        bn.learn.algorithms[w] != "tabu")
    {
      cat (paste("\nThe BN learning algorithm algorithm '",bn.learn.algorithms[w],"' is not allowed,
                 only 'hc', 'mmhc', 'rsmax' and 'tabu' ! Please review your code.", sep=""))
      return(NULL)
    }

  } # for (w in 1:length(bn.learn.algorithms))

  return (NULL)

} # gera.bn.pa <-function
