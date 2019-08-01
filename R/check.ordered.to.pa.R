#'Verifies if there are ordered factor variables to be declared in the pa model building process
#'
#'Receives a BN structure and a data set, then verifies if there are ordered variables. In a positive case return TRUE.
#'@param bn.structure is a BN structure learned from data used to identify if the variable is endogenous or exogenous when building the PA model.
#'@param data.to.work is a data set containing the variables of the BN.
#'@return a data frame with ordered variables.
#'@author Elias Carvalho
#'@references HAYES, A F; PREACHER, K J. Statistical mediation analysis with a multicategorical independent variable. British Journal of Mathematical and Statistical Psychology, v. 67, n. 3, p. 451-470, 2014.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("~/your working directory")
#'# Load packages
#'library(bnpa)
#'# Load the dataset
#'data(dataQualiN) # Pre-Loaded
#'# Build the BN structure
#'bn.structure<-bnlearn::hc(dataQualiN)
#'# Show the BN structure learned
#'bnlearn::graphviz.plot(bn.structure)
#'# Tranforms variables A and B in ordered factor
#'dataQualiN$A <- as.ordered(dataQualiN$A)
#'dataQualiN$B <- as.ordered(dataQualiN$B)
#'# Generates a list with variables to be ordered and exogenous variables
#'cat.var.to.use.in.pa <- bnpa::check.ordered.to.pa(bn.structure, dataQualiN)
#'# Show the variables
#'cat.var.to.use.in.pa
#'@export

check.ordered.to.pa <-function(bn.structure, data.to.work)
{
  # Set working variables to store ordered variables
  dummy.to.create <- ""
  matrix.dummy.to.create <- data.frame(matrix(ncol = 1, nrow = 1))
  names(matrix.dummy.to.create)[1] <- "Variables"
  ncount.dummy.to.create <- 0

  ordered.to.declare <- ""
  matrix.ordered.to.declare <- data.frame(matrix(ncol = 1, nrow = 1))
  names(matrix.ordered.to.declare)[1] <- "Variables"
  ncount.ordered.to.declare <- 0

  # Scan all columns of data.to.work to
  for (x in 1:ncol(data.to.work))
  {
    #Check type if variable
    type.of.variable<-check.type.one.var(data.to.work, 0,names(data.to.work[x]))
    # if is factor
    if (type.of.variable==3)
    {
      # Verifies if is ordered factor
      if (bnpa::check.ordered.one.var(data.to.work , names(data.to.work[x]))) # Are ordered variables
      {
        # Check if variable has no parents
        if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0)
        # Variable is ordered and exogenous then create dummy
        {
          # Add the variable to a dummy to create list
          dummy.to.create <- paste(dummy.to.create,"'", names(data.to.work[x]),"',", sep = "")
          # Add 1 to ncount.ordered.to.declare
          ncount.dummy.to.create <- ncount.dummy.to.create + 1
          # Add the variable to matrix
          matrix.dummy.to.create[ncount.dummy.to.create,1] <- names(data.to.work[x])
        }
        else # if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0)
        {
          # Add the variable to chr 'prdered.to.declare'
          ordered.to.declare <- paste(ordered.to.declare,"'", names(data.to.work[x]),"',", sep = "")
          # Add 1 to ncount.ordered.to.declare
          ncount.ordered.to.declare <- ncount.ordered.to.declare + 1
          # Add the variable to matrix
          matrix.ordered.to.declare[ncount.ordered.to.declare,1] <- names(data.to.work[x])
        } # else # if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0)
        # names(data.to.work[x])[1,ncount.ordered.to.declare] <- c(names(data.to.work[x]))
      } else # if (bnpa::check.ordered.one.var(data.to.work , names(data.to.work[x])))
      # Dichotomic variables
      {
        if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0) # Exogenous Binary (dummy nuneric)
          # Variable is binary and exogenous then create dummy
        {
          # Add the variable to a dummy to create list
          dummy.to.create <- paste(dummy.to.create,"'", names(data.to.work[x]),"',", sep = "")
          # Add 1 to ncount.ordered.to.declare
          ncount.dummy.to.create <- ncount.dummy.to.create + 1
          # Add the variable to matrix
          matrix.dummy.to.create[ncount.dummy.to.create,1] <- names(data.to.work[x])
        } else # if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0) # Endogenous Binary (ordered factor/ordered in lavaan)
        {
          # Add the variable to chr
          ordered.to.declare <- paste(ordered.to.declare,"'", names(data.to.work[x]),"',", sep = "")
          # Add 1 to ncount.ordered.to.declare
          ncount.ordered.to.declare <- ncount.ordered.to.declare + 1
          # Add the variable to matrix
          matrix.ordered.to.declare[ncount.ordered.to.declare,1] <- names(data.to.work[x])
        } # else # if (length(bnlearn::parents(bn.structure, names(data.to.work[x])))==0)
      } # else # if (bnpa::check.ordered.one.var(data.to.work , names(data.to.work[x])))
    } # if (type.of.variable==3)
  } # for (x in 1:ncol(bn.structure, data.to.work))


  # Remove last comma
  if (ordered.to.declare!="")
  {
    ordered.to.declare <- substr(ordered.to.declare,1,nchar(ordered.to.declare)-1)
  }
  if (dummy.to.create!="")
  {
    dummy.to.create <- substr(dummy.to.create,1,nchar(dummy.to.create)-1)
    # commandAssign <- paste("data.to.work <- fastDummies::dummy_cols(data.to.work, select_columns = c(", noquote(dummy.to.create),"))", sep = "")
    # eval(parse(text=commandAssign))

    # for (x in 1:nrow(matrix.dummy.to.create))
    # {
    #   commandAssign <- paste("data.to.work$", matrix.dummy.to.create[x,1],"<- NULL", sep = "")
    #   eval(parse(text=commandAssign))
    # } # for (x in 1:nrow(matrix.dummy.to.create))
  }
  # Return a data frame with ordered variables
  return(list(ordered.to.declare, matrix.ordered.to.declare, dummy.to.create, matrix.dummy.to.create))
} # check.ordered.to.pa
