#'Mounts a Bayesian Network graph with graphviz function
#'
#'Using a Bayesian Network structure learned builds a Directly Acyclic Graph (DAG).
#'
#'@param bn.averaged is a Bayesian Network structure learned.
#'@param bn.param is a Bayesian Network parameters learned.
#'@param graph.name is a name to save the Path Analysis graph.
#'@param data.to.work is a data frame containing the variables of the Bayesian Networks learned.
#'@details
#'This function receives a Bayesian Network structure and parameters learned, a dataset and builds
#'a DAG showing the parameters on edges.
#'@return NULL
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# Load datasets from package
#'data.to.work <- dataQuantC
#'# Set the name of a text to save the Bayesian Networks parameters
#'param.name <- "docbnparamHC.txt"
# Learn a Bayesian Networks structure from data
#'bn.structure <- hc(data.to.work)
#'# Generates a Bayesian Networks parameters from the Bayesian Networks structure and 
#'# write a text file with this
#'bn.param <- gera.bn.param(bn.structure, data.to.work, param.name)
#'# set the graph name
#'graph.name <- "imgBNHC"
#'# Set the option to show or not the parameters on each edge
#'# Save the graph
#'mount.graph(bn.structure, bn.param, graph.name, data.to.work)
#'@export

mount.graph <-function(bn.averaged, bn.param, graph.name, data.to.work) {

  # Check if needed packages are installed
  bnpa::check.package.files()

  # CREATING A Bayesian Networks OBJECT TO WORK ON GRAPHIC LATER

  # nodeRenderInfo(g1) <- list(shape=c(g="box", i="triangle", j="circle", c="plaintext"))
  graph.structure<-bnlearn::graphviz.plot(bn.averaged)

  labels <- ""
  # STORE THE NAME OF ALL EDGES
  labels1 <- graph::edgeNames(graph.structure)

  # STORE A STRENGTH OF EACH EDGE OF GRAPH
  strength<-bnlearn::arc.strength(bn.averaged, data.to.work)

  # CREATE A MATRIX WITH ROWS EQUAL STRENTGTH AND 2 COLUMS (from AND to)
  strength.var.length <- matrix(nrow=nrow(strength),ncol=ncol(strength)-1)

  # Create a variable to store edge color settings
  setColorEdge <- ""
  setColorEdge <- matrix(nrow=nrow(strength),ncol=2)

  # STORE THE LENGTH OF "from" AND "to" IN THE MATRIX
  for (x in 1 : nrow(strength))
  {
    for (y in 1: ncol(strength)-1)
    {
      strength.var.length[x,y] <- nchar(strength[x,y])
    } # for (y in 1: ncol(strength)-1)
  } # for (x in 1 : nrow(strength))

  # CREATES A MATRIX TO STORE THE POSITION OF "~" AND THE SIZE OF EACH LABEL
  labels.var.length <- matrix(nrow=length(labels1),ncol=2)

  # store in the first column of a matrix the position of character "~"
  labels.var.length[,1]<-unlist(gregexpr(pattern ='~',labels1))
  for (x in 1 : length(labels1))
  {
    # and in the second column the lenght of label
    labels.var.length[x,2] <- nchar(labels1[x])
  } # for (x in 1 : length(labels1))

  # SCAN ALL LINE OF VARIABLE LABELS1  ####################################################
  for (y in 1:length(labels1))
  {

    # Scan all strength variables to compare with each labels1 variable
    for (w in 1:nrow(strength))
    {
      # if outcome and preditor in labels1 is the same from and to in strenght variable I can extract coefficients
      if (
        substring(labels1[y],1,labels.var.length[y,1]-1) == strength[w,1] &&
        substring(labels1[y],labels.var.length[y,1]+1, labels.var.length[y,2]) == strength[w,2]
      )
      {

        # BUILD A STRING TO TAKE THE INTERCEPT OF THE ARC
        intercept<-paste("bn.param", "$",strength[w,2], "$coefficients", "[1]", sep="")
        intercept<-eval(parse(text=intercept))
        # Take all coefficients
        coefficientsG <- paste("bn.param", "$", strength[w,2], "$coefficients", sep="")
        coefficientsG <- eval(parse(text=coefficientsG))

        # Extract all
        for (z in 2:length(coefficientsG))
        {
          # when there is no more coefficients it leaves from for loop
          if (is.na(names(coefficientsG[z]))) break
          # verify from where is incomming arc to take correctly regression coefficient
          if (substring(labels1[y],1,labels.var.length[y,1]-1) == names(coefficientsG[z]))
          {
            coefficients <- paste("bn.param", "$", strength[w,2], "$coefficients[",z,"]", sep="")
            coefficients<-eval(parse(text=coefficients))
          }
        } # for (z in 2:length(coefficientsG))

        labels[y] <- paste(toString(round(coefficients,3)),"(c)",sep="")

        if (coefficients < 0)
        {
          setColorEdge[y,1] <- paste ("graph::edgeRenderInfo(graph.structure) <- list(col=c('",
                                      substring(labels1[y],1,labels.var.length[y,1]-1),"~",
                                      substring(labels1[y],labels.var.length[y,1]+1, labels.var.length[y,2]),
                                      "'='red'), lwd =c('", substring(labels1[y],1,labels.var.length[y,1]-1),"~",
                                      substring(labels1[y],labels.var.length[y,1]+1, labels.var.length[y,2]),
                                      "' = ", collapse="", sep=""
          )
          setColorEdge[y,2] <- coefficients
        } else # if (round(coefficients,2) < 0)
        {
          setColorEdge[y,1] <- paste ("graph::edgeRenderInfo(graph.structure) <- list(col=c('",
                                      substring(labels1[y],1,labels.var.length[y,1]-1),"~",
                                      substring(labels1[y],labels.var.length[y,1]+1, labels.var.length[y,2]),
                                      "'='blue'), lwd =c('", substring(labels1[y],1,labels.var.length[y,1]-1),"~",
                                      substring(labels1[y],labels.var.length[y,1]+1, labels.var.length[y,2]),
                                      "' = ", collapse="", sep=""
          )
          setColorEdge[y,2] <- coefficients
        } # else if (round(coefficients,2) < 0)
        # eval(parse(text=setColorEdge))

        # GET OUT OF LOOPING WHEN CONDITION IS SATISFIED
        break
      } # if (substring(labels1[y],1,labels.var.length[y,1]-1) == strength[w,1]...
    } # for (w in 1:nrow(strength))
  } # for (y in 1:length(labels1))   ####################################################

  # Graph parameters - source: http://goo.gl/H2z09I
  # Insert name of connection to labels to put each value on right edge
  names(labels) <- labels1

  # Graph parameters - source: http://goo.gl/H2z09I
  # creates a graph object with new parameters
  graph.structure<- Rgraphviz::layoutGraph(graph.structure, edgeAttrs=list(label=labels))

  graph::graph.par(list(graph=list(layout="circo",
                                   edges=list(fontsize=5,  col="red", lty="solid", lwd=2),
                                   nodes=list(fontsize=5, col="blu", lty="solid", lwd=2, width=.5,
                                              shape="circle", fixedsize=FALSE),
                                   cex.main=1.4, col.main="black", cex.sub=1.0, col.sub="black"
                                  )
                        )
                  )

  # Calculate a quarter of maxim positive and maxim negative value
  # The idea is to se the 4 (positive and negative) parts and set
  # the size of line for edge. i.e. 1 = thin line 2 = bold line...4 = more bold line
  nquarter <- 0
  pquarter <- 0

  # Scan all setColor Edge to set the high positive / negative quarter
  for (x in 1:(length(setColorEdge)/2))
  {
    if ((as.numeric(setColorEdge[x,2])) >= 0)
    {
      if ((as.numeric(setColorEdge[x,2])) > pquarter)
      {
        pquarter <- as.numeric(setColorEdge[x,2])
      }
    }    else {
      if ((as.numeric(setColorEdge[x,2])) < nquarter)
      {
        nquarter <- (as.numeric(setColorEdge[x,2]))
      }

    } # else {
  } #   for (x in 1:(length(setColorEdge)/2))
  # change nquarter to a positive number
  nquarter <- nquarter * -1

  for (x in 1:(length(setColorEdge)/2))
  {

    if (as.numeric(setColorEdge[x,2]) >= 0)
    {
      if (as.numeric(setColorEdge[x,2]) <= (pquarter/4))
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"1))", collapse = "")
      } else if (as.numeric(setColorEdge[x,2]) <= (pquarter/4)*2)
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"2))", collapse = "")
      } else if (as.numeric(setColorEdge[x,2]) <= (pquarter/4)*3)
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"3))", collapse = "")
      } else
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"4))", collapse = "")
      }

    } # if (as.numeric(setColorEdge[x,2]) >= 0)

    else {
      if ((as.numeric(setColorEdge[x,2]) * -1) <= (nquarter/4))
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"1))", collapse = "")
      } else if ((as.numeric(setColorEdge[x,2]) * -1) <= (nquarter/4)*2)
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"2))", collapse = "")
      } else if ((as.numeric(setColorEdge[x,2]) * -1) <= (nquarter/4)*3)
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"3))", collapse = "")
      } else
      {
        setColorEdge[x,1] <- paste(substr(setColorEdge[x,1],1, nchar(setColorEdge[x,1])),"4))", collapse = "")
      }
    } # else {

    commandSetColor <- setColorEdge[x,1]
    eval(parse(text=commandSetColor))

  } #   for (x in 1:(length(setColorEdge)/2))

  graph::nodeRenderInfo(graph.structure)
  #renderGraph(graph.structure)

  # PLOT AND SAVE THE GRAPH
  grDevices::png(filename=graph.name, units="in", width=10, height=8, pointsize=12, res=150)
  graph <- Rgraphviz::renderGraph(graph.structure)
  grDevices::dev.off()

} # mount.graph <-function(bn.averaged, bn.param, graph.name, data.to.work)
