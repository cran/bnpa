#'Learn the Bayesian Network structure from data
#'
#'Receives a dataset and learn the structure of Bayesian Network (BN) diagram using algorithms implemented in bnlearn package.
#'
#'@param data_bn is a data frame containing the variables of the Bayesian Networks to be learned.
#'@param bn.learn.algorithm is a list of algorithms (present in bnlearn) to be used in Bayesian Networks learning process.
#'@param wl is a list with a couple of variables to build the white list.
#'@param bl is a list with a couple of variables to build the black list.
#'@param type.variable is the type of variable inside the data frame
#'@param nreplicates is the number os procedures to be executing during bootstraping.
#'@details
#'This function receives a dataset, a list of algorithms, a list of required nodes and another list of forbidden
#'nodes, the type of variables in the dataset and the number of replicates the bootstrap process will execute as
#'parameters. Then it learn the Bayesian Network structure and return it .
#'@return The Bayesian Networks strucutre learn bootstrapped
#'@author Elias Carvalho
#'@export

gera.bn.structure <-function(data_bn, bn.learn.algorithm, wl, bl, type.variable, nreplicates = 100)
{
  # Check if the Bayesian Networks learning algorithms are allowed
  check.algorithms <-(bn.learn.algorithm)

  # Mount a white/black list
  if (wl != "")
  {
    cat("\nBuilding the white list...")
    white.list <- mount.wl.bl.list(wl)
  } else
  {
    white.list <- ""
  }
  if (bl != "")
  {
    cat("\nBuilding the black list...")
    black.list <- mount.wl.bl.list(bl)
  }
  else
  {
    black.list <- ""
  }

  # Learn a Bayesian Networks structure
  bn.total <- list()

  # according type of dataset the type of score to be used in the structure learning
  type.score <- check.type.score(type.variable)

  # Calculate nreplicates bns
  cat("\n\n")
  utils::timestamp()

  if (bl != "" && wl != "")
  {
    # Learn the Bayesian Networks with black and white list
    cat (paste("\nLearning ", nreplicates, " bn(s) with boot.strap, -->", bn.learn.algorithm,"<-- algorithm and", type.score,
         "score, with black list:", bl, "and white list:", wl, "at", format(Sys.time(), "%b %d %Y %X"),"\n"), sep = "")

    bn.boot.strap = bnlearn::boot.strength(data = data_bn, R = nreplicates, algorithm = bn.learn.algorithm,
                                           algorithm.args = list(score = type.score, whitelist = white.list,
                                                                 blacklist = black.list), cpdag = FALSE)
  } else if (bl != "" && wl == "")
  {
    # Learn the Bayesian Networks with black list only
    cat (paste("\nLearning ", nreplicates, " bn(s) with boot.strap, -->", bn.learn.algorithm,"<-- algorithm and", type.score,
         "score, with black list:", bl, "at", format(Sys.time(), "%b %d %Y %X"),"\n"), sep = "")

    bn.boot.strap = bnlearn::boot.strength(data = data_bn, R = nreplicates, algorithm = bn.learn.algorithm,
                                           algorithm.args = list(score = type.score, blacklist = black.list), cpdag = FALSE)

  } else if (bl == "" && wl != "")
  {
    # Learn the Bayesian Networks with white list only
    cat (paste("\nLearning ", nreplicates, " bn(s) with boot.strap, -->", bn.learn.algorithm,"<-- algorithm and", type.score,
         "score, with white list:", wl, "at", format(Sys.time(), "%b %d %Y %X"),"\n"), sep = "")

    bn.boot.strap = bnlearn::boot.strength(data = data_bn, R = nreplicates, algorithm = bn.learn.algorithm,
                                           algorithm.args = list(score = type.score, whitelist = white.list), cpdag = FALSE)
  } else if (bl == "" && wl == "")
  {
    # Learn the Bayesian Networks without black and white list
    cat (paste("\nLearning ", nreplicates, " bn(s) with boot.strap, -->", bn.learn.algorithm,"<-- algorithm and", type.score,
         "score, whithout black and white list at", format(Sys.time(), "%b %d %Y %X"),"\n"), sep = "")

    bn.boot.strap = bnlearn::boot.strength(data = data_bn, R = nreplicates, algorithm = bn.learn.algorithm,
                                           algorithm.args = list(score = type.score), cpdag = FALSE)
    }

  return(bn.boot.strap)

} # gera.bn.structure <-function
