#'Learn the Bayesian Network and Path Analysis
#'
#'This function learn the Bayesian Network from the dataset and builds a Path Analysis model.
#'@param data_bn is a data frame containing the variables from which the Bayesian Networks structure will be learned.
#'@param bn.learn.algorithms is a list of algorithms (present in bnlearn) to be used in Bayesian Networks structure learning pocess.
#'@param wl is a list with a couple of variables to build the white list (mandatory nodes of BN).
#'@param bl is a list with a couple of variables to build the black list (forbidden nodes of BN).
#'@param nreplicates is the size of each Bayesian Network bootstrap replicate.
#'@details
#'This function receives a dataset, a list of algorithms, a list of required nodes, a list of forbidden nodes
#'and the number of replicates the bootstrap process will execute as parameters. Then the function identify
#'the type of variable and convert it to numeric if necessary, then learn the Bayesian Network structure with
#'one or more Bayesian Networks structure learn algorithm, learn the Bayesian Networks parameters and build the Path Analysis model. Finally it
#'exports a Bayesian Networks / Path Analysis graph figure and text file with Bayesian Networks / Path Analysis parameters.
#'@return NULL.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Show datasets
#'data1<-dataQuantC # Pre-Loaded
#'data2<-dataQualiN # Pre-Loaded
#'head(data1)
#'head(data2)
#'# Set a dataset to work
#'data.to.work <- data1
#'# Transform some variables into integer types
#'data.to.work$A<-as.integer(data.to.work$A)
#'data.to.work$C<-as.integer(data.to.work$C)
#'data.to.work$E<-as.integer(data.to.work$E)
#'data.to.work$G<-as.integer(data.to.work$G)
#'# Creates a white and black list empty
#'wl=""
#'bl=""
#'# Set what Bayesian Networks learning algorithms will be used
#'bn.learn.algorithms <- c("hc", "rsmax2")
#'# Learn a Bayesian Networks structure from data to work and builds a Path Analysis model
#'bn.pa<-gera.bn.pa(data.to.work, bn.learn.algorithms)
#'@export

gera.bn.pa <-function(data_bn, bn.learn.algorithms, wl="", bl="", nreplicates = 100)
  {

  check.algorithms <-(bn.learn.algorithms)

  # Check if needed packages are installed
  cat ("\nCheck if all packages needed are installed... If not will install.")
  bnpa::check.package.files()

  # check if the dataset has NAs
  cat ("\nCheck if the dataset has NAs...")
  total.na <- check.na(data_bn)

  if (total.na > 0)
  {
    cat ("\nYour dataset has",total.na," NA(s). The bnpa package does not allow it. Fix this problem, please.")
    return()
  }

  # check what type of variables the dataset has: 1=integer, 2=numeric, 3=factor,
  # 4=integer and numeric, 5=integer and  factor, 6=numeric and factor,
  # 7=integer, numeric and factor
  type.variable <- bnpa::check.types(data_bn)

  if (type.variable != 2)
  {
    if (type.variable == 1)
    {
      type.data <- ("integer")
    } else if  (type.variable == 3)
    {
      type.data <- ("factor")
    } else if (type.variable == 4)
    {
      type.data <- ("integer and numeric")
    } else if (type.variable == 5)
    {
      type.data <- ("integer and  factor")
    } else  if (type.variable == 6)
    {
      type.data <- ("numeric and factor")
    } else if (type.variable == 7)
    {
      type.data <- ("numeric and factor,")
    }

    if (type.variable == 1 || (type.variable == 4))
    {
      cat (paste("\nVariable type must be only numeric! Your is ", type.data,". In this case the bnpa package will automatically convert it into numeric.", sep = ""))
      # check if variable is int and transform into numeric
      data_bn <- bnpa::convert.continuous.int.to.numeric(data_bn)
    } else if (type.variable != 2)
    {
      cat (paste("\nVariable type must be only numeric! Please convert your ", type.data," data into numeric.", sep = ""))
      return()
    }
  }

  # Creates a direction variable to be used in the bn.boot.strap.strength.dir
  bn.suitable.direction <- 0.50

  for (w in 1:length(bn.learn.algorithms))
  {
    bn.boot.strap <- gera.bn.structure(data_bn, bn.learn.algorithms[w], wl, bl, type.variable)

    # select the best threshold for this dataset
    bn.suitable.threshold <-attributes(bn.boot.strap)$threshold

    # Select the best edges based on threshold and direction, normally 0.85 and 0.50
    cat ("\nSelecting the best edges based on threshold and direction values...\n") ################### cat
    bn.boot.strap.strength.dir <- bn.boot.strap[(bn.boot.strap$strength >= bn.suitable.threshold) &
                                                  (bn.boot.strap$direction > bn.suitable.direction), ]

    # If bn.boot.strap.strength.dir result in a 0 row dataframe then give a message and return
    if (nrow(bn.boot.strap.strength.dir) == 0)
    {
      cat("\nSorry, but the generated Bayesian Network is empty, check your data and try again, please.\n")
      return()
    }

    # Over the best edges selected on the previous step, calculate the averaged BN
    cat("\n")
    utils::timestamp()

    cat ("\nCalculating the averaged BN...\n")
    bn.averaged <- bnlearn::averaged.network(bn.boot.strap.strength.dir)

    cat("\nChecking compatibility of data and final Bayesian Network before generating Bayesian Network parameters...\n")
    # Creates a matrix with names de data rows and 2 columns (column 1 = node,
    #  col 2 = 1:exist in bn.boot.strap.strength.dir, 2: does not exist)

    nodes.matrix <- matrix(, nrow = length(names(data_bn)), ncol = 2)
    # Load nodes.matrix with the names of nodes of data
    for (x in 1:length(names(data_bn)))
    {
      nodes.matrix[x,1] <- names(data_bn)[x]
      nodes.matrix[x,2] <- 0
    }

    # Check if nodes of data  are in bn.boot.strap.strength.dir, if not
    # remove it from data before call gera.pa
    for (x in 1:length(bn.boot.strap.strength.dir$from))
    {
      for (y in 1:nrow(nodes.matrix))
      {
        if (nodes.matrix[y,2] == 0 && (nodes.matrix[y,1] == bn.boot.strap.strength.dir$from[x]))
          nodes.matrix[y,2] <- 1
      } # for y(in 1:nrow(nodes.matrix))
    } # for (x in 1:length(bn.boot.strap.strength.dir$from))

    for (x in 1:length(bn.boot.strap.strength.dir$to))
    {
      for (y in 1:nrow(nodes.matrix))
      {
        if (nodes.matrix[y,2] == 0 &&
            (nodes.matrix[y,1] == bn.boot.strap.strength.dir$to[x]))
          nodes.matrix[y,2] <- 1
        # Check if a node has no parents and no childrens
        if (nodes.matrix[y,2] == 1 &&
            ((length(bnlearn::parents(bn.averaged, nodes.matrix[y,1])))  == 0 &&
             (length(bnlearn::children(bn.averaged, nodes.matrix[y,1]))) == 0))
        {
          cat("\nYour Bayesian Network sructure has nodes without parents and childrens, remove from you dataset...\n\n") ################### cat
        }
      } # for y(in 1:nrow(nodes.matrix))
    } # for (x in 1:length(bn.boot.strap.strength.dir$to))

    # check what node remove and do it
    for (x in 1:nrow(nodes.matrix))
    {
      if (nodes.matrix[x,2] == 0 )
      {
        cat("\nRemoving variable:", nodes.matrix[x,1], "...\n\n") ################### cat

        commandAssign <- (paste("data_bn$", nodes.matrix[x,1], " <- NULL", sep = ""))
        eval(parse(text=commandAssign))
      }
    } # for y(in 1:nrow(nodes.matrix))

    # Generating Bayesian Networks Parameters ################
    cat("\n")
    utils::timestamp()
    cat ("\nGenerating Bayesian Networks parameters (using linear regression)...\n") ################### cat
    param.name <- paste0("docbnparam", bn.learn.algorithms[w],".txt", sep = "")
    bnhc.param <- ""
    bnhc.param <- gera.bn.param(bn.averaged, data_bn, param.name)

    # Export a Bayesian Networks graph and do not show it on Plot Panel ################
    cat("\n")
    utils::timestamp()
    cat ("\nExport a Bayesian Networks graph...\n")
    graph.name <- paste0("imgBN", bn.learn.algorithms[w], ".png", sep = "")
    mount.graph(bn.averaged, bnhc.param,  graph.name, data_bn)

    # Creating Path Analysis model ################
    cat("\n")
    utils::timestamp()
    cat ("\nCreating a Path Analysis model calculation...\n") ################### cat
    pa.name <- paste0("docPAParam",bn.learn.algorithms[w]," .txt", sep = "")
    pa.imgname <- paste("imgPA",bn.learn.algorithms[w],".png", sep = "")

    pa.param <- ""
    pa.param <- gera.pa(bn.averaged, data_bn, pa.name, pa.imgname)

  } # for (w in 1:length(bn.learn.algorithms))


  cat("\n\n===== Fim do proceidmento=====")
  return ()

} # gera.bn.pa <-function
