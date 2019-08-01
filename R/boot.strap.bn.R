#'Executes a bootstrap during the learning of a BN structure
#'
#'This function receives a list of parameters, executes the bootstrap process and learn the Bayesian Network (BN) from the data set, then executes the process of model averaging to extract the final BN structure and print it.
#'@param bn.algorithm is a list of algorithms to learn the BN structure.
#'@param bn.score.test is list of conditional independence tests and the network scores to be used.
#'@param data.to.work is a data from which the BN structure will be learned.
#'@param black.list is a list of forbiden connections of BN structure to be created.
#'@param white.list  is a list of mandatory connections of BN structure to be created.
#'@param nreplicates is the number of replications to be done in the bootstrap process.
#'@param type.of.algorithm is the type of algorithm to learn the BN sctructure, it would be constrained or score based.
#'@param outcome.var is the variable to be used as outcome (dependent) and be highlighted in the BN.
#'@return The final BN structure learned.
#'@author Elias Carvalho
#'@references Claeskens N, Hjort N (2009) Model selection and model avaraging. Cambridge University Press, Cambridge, England.
#'@references Koller D, Friedman N (2009) Probabilistic graphical models: principles and techniques. MIT Press, Cambridge.
#'@references Scutari M (2017). Bayesian Network Constraint-Based Structure Learning Algorithms: Parallel and Optimized Implementations in the bnlearn R Package. Journal of Statistical Software, 77(2), 1-20.
#'@examples
#'\dontrun{
#'# Clean environment
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
#'nreplicates=1000
#'# Set the algorithm to be used
#'bn.algorithm="hc"
#'# Executes a parallel bootstrap process
#'data.bn.boot.strap=bnlearn::boot.strength(data = dataQualiN, R = nreplicates, algorithm =
#'bn.algorithm, cluster=cl, algorithm.args=list(score="bic"), cpdag = FALSE)
#'# Release the cluster
#'parallel::stopCluster(cl)
#'head(data.bn.boot.strap)
#'}
#'@export

boot.strap.bn <-function(bn.algorithm, bn.score.test, data.to.work, black.list, white.list, nreplicates = 1000, type.of.algorithm, outcome.var)
{
  # Set the BN param and image name
  bn.param.name <- paste("docBN_Param_", bn.algorithm, "_", bn.score.test, "_", nrow(data.to.work), ".txt", sep = "")
  bn.imgname <- paste("imgBN_", bn.algorithm, "_", bn.score.test, "_", nrow(data.to.work), ".png", sep = "")
  data.bn.boot.strap <- ""
  if (type.of.algorithm=="constrained-based")
  {
    commandAssign <- paste("data.bn.boot.strap = bnlearn::boot.strength(data = data.to.work, R = nreplicates, algorithm = bn.algorithm, cluster = cl, algorithm.args = list(test = bn.score.test, whitelist=white.list, blacklist = black.list), cpdag = FALSE)")
  } else if (type.of.algorithm=="score-based") #  if (type.of.algorithm=="constrained-based")
  {
    commandAssign <- paste("data.bn.boot.strap = bnlearn::boot.strength(data = data.to.work, R = nreplicates, algorithm = bn.algorithm,cluster = cl, algorithm.args = list(score = bn.score.test, whitelist=white.list, blacklist = black.list), cpdag = FALSE)")
  } # else if (type.of.algorithm=="score-based") #  if (type.of.algorithm=="constrained-based")

  # Creates a cluster based on sockets to execute bootstraping in parallel using number of processor cores -1
  cl <- bnpa::create.cluster()

  # Start bootstrap
  cat (paste("\n\n==== Bootstraping with algorithm '", bn.algorithm, "' and '", bn.score.test, "' replications: ", nreplicates,"...", sep = ""))

  eval(parse(text=commandAssign))

  # Release the cluster
  parallel::stopCluster(cl);

  # If the BN is not empty print BN param and graph
  if (!is.na((data.bn.boot.strap$from[1])))
  {
    # Averaging the BN selecting the best arcs with the arc strength greather equal
    # attributes(data.bn.boot.strap)$threshold and direction >= 0.5
    bn.structure <- bnlearn::averaged.network(data.bn.boot.strap)

    # If there area directed arcs
    outcome.parents<-""
    parents.of.outcome<-0
    # If there are directed arcs
    if (length(bnlearn::directed.arcs(bn.structure))>0)
    {
      # Calculates how much parents outcome.var has
      commandAssign <- paste("parents.of.outcome <- length(bnlearn::parents(bn.structure, '", outcome.var, "'))", sep = "")
      eval(parse(text=commandAssign))
      # If has parents
      if (parents.of.outcome > 0)
      {
        # Find the parents of the outcome variable
        commandAssign <- paste("outcome.parents <- bnlearn::parents(bn.structure,'", outcome.var, "')", sep = "")
        eval(parse(text=commandAssign))
        # Set a working var to store highligh vars
        outcome.highlights <- ""
        # Scan the parents and fill  the matrix
        for (x in 1: length(outcome.parents))
        {
          # In the first time create a matrix
          if (x==1)
          {
            # Create a matrix to store variables to highlight
            commandAssign <- paste("outcome.highlights <- matrix(c(",  sep = "")
          } # if (x==1)
          # If is not the last element
          if (x !=length(outcome.parents))
          {
            # mount the commmand joining parents of outcome
            commandAssign <- paste(commandAssign,"'", outcome.parents[x],"','", outcome.var,"',", sep = "")
          } else # if (x !=length(outcome.parents)) # Is the last element
          {
            commandAssign <- paste(commandAssign,"'", outcome.parents[x],"','", outcome.var,"')", sep = "")
          } # else # if (x !=length(outcome.parents))
        } #  for (x in 1: length(outcome.parents))
        # Creates a matrix with 'from' and 'to' highlight the outcome
        commandAssign <- paste(commandAssign, ", ncol = 2, byrow = TRUE, dimnames = list(NULL, c('from', 'to')))", sep = "")
        eval(parse(text=commandAssign))

        # If nas no parents
        if (length(outcome.parents) == 0)
        {
          # Set name and type of image
          grDevices::png(filename=bn.imgname, units="in", width=7, height=7, pointsize=6, res=150)
          # Plot a BN structure learned without highlights
          bnlearn::strength.plot(bn.structure, data.bn.boot.strap, shape = "ellipse")
          # Turn off device and save into disk
          grDevices::dev.off()
        } else # if (length(outcome.parents) == 0)
        {
          # Set name and type of image
          grDevices::png(filename=bn.imgname, units="in", width=7, height=7, pointsize=6, res=150)
          # Plot a BN structure learned
          bnlearn::strength.plot(bn.structure, data.bn.boot.strap, shape = "ellipse",
                                highlight = list(arcs = outcome.highlights))
          # Turn off device and save into disk
          grDevices::dev.off()
        } # else # if (length(outcome.parents) == 0)
      } else # if (parents.of.outcome > 0)
      {
        # Set name and type of image
        grDevices::png(filename=bn.imgname, units="in", width=7, height=7, pointsize=6, res=150)
        # Plot a BN structure learned
        bnlearn::strength.plot(bn.structure, data.bn.boot.strap, shape = "ellipse")
        # Turn off device and save into disk
        grDevices::dev.off()
      } # else # if (parents.of.outcome > 0)
    } else # if (length(bnlearn::directed.arcs(bn.structure))>0)
    {
      # Set BN structure as NULL
      bn.structure <- NULL
    } # else # if (length(bnlearn::directed.arcs(bn.structure))>0)
  } else #  if (!is.na((data.bn.boot.strap$from[1]))) # The BN structure is empty
  {
    # Set BN structure as NULL
    bn.structure <- NULL
  } # else #  if (!is.na((data.bn.boot.strap$from[1])))
  # Return the BN structure
  return(bn.structure)
} # boot.strap.bn <-function
