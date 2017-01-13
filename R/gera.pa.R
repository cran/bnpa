#'Generates a model as input to execute a Path Analysis (PA).
#'
#'Builds a model as input to Path Analysis from a Bayesian Network (BN) learned.
#'
#'@param bn.averaged is a Bayesian Network structure learned from data.
#'@param data.to.work is a data frame containing the variables of the BN.
#'@param pa.name is a variable to store the name of file to save Path Analysis parameters.
#'@param pa.imgname is a variable to store the name of file to save Path Analysis graph.
#'@details
#'This function receives a Bayesian Networks structure and a dataset, calculates 
#'the correlation matrix from data, scan all columns of dataset, scan all parents 
#'of each column variable and build a Path Analysis model string. Then run the Path Analysis model 
#'using Strucutural Equation Model functions and export a Path Analysis graph figure with 
#''semPaths' function and also export a Path Analysis model summary information.
#'@return pa.model.fit an object of class lavaan.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# load datasets from package
#'data.to.work<-dataQuantC # Pre-Loaded
#'# Learn Bayesian Networks structure
#'bn.structure <- hc(data.to.work)
#'# Set the name of a doc to save Path Analysis parameters
#'pa.name = "docPAHC"
#'# Set the name of an image with Path Analysis graph
#'pa.imgname = "imgPAHC"
#'# Generates the Path Analysis model from Bayesian Networks structure
#'pa.model.fit <- gera.pa(bn.structure, data.to.work, pa.name, pa.imgname)
#'@export

gera.pa <-function(bn.averaged, data.to.work, pa.name, pa.imgname) {

  # Calculates the correlation table
  # if (check.dichotomic(data.to.work))
  # {
  # data.cor<-qgraph::cor_auto(as.matrix(data.to.work)) # polychoric correlation, for dichotomic and mixed data
  # }   else {
     data.cor<-stats::cor(as.matrix(data.to.work)) # normal correlation for non dichotomic data
  # } # if (check.dichotomic(data.to.work))

  # Starts building a Path Analysis model string
  paModel <-""

  # Scan all columns of data.to.work to mount a model for each variable
  for (x in 1:ncol(data.to.work))
  {

    # Scan all parents of a variable
    for (y in 1:length(bnlearn::parents(bn.averaged,names(data.to.work[x]))))
    {

      # If lenght of parents is zero, means the variable does not goes to the model
      if (length(bnlearn::parents(bn.averaged,names(data.to.work[x]))) != 0)
      {

        # If is the first element, build the main variable followed by "~" and first parent
        if (y == 1)
          paModel <- noquote(paste(paModel, "\n", names(data.to.work[x]), " ~ ", bnlearn::parents(bn.averaged,names(data.to.work[x]))[y]))
        else
          # for second, third..., element just add "+" and the next parent
          paModel <- noquote(paste(paModel, "+", bnlearn::parents(bn.averaged,names(data.to.work[x]))[y]))
      } # if (length(parents(bn.averaged,names(data.to.work[x]))) != 0)
    } # for (y in 1:length(parents(bn.averaged,names(data.to.work[x]))))
  } # for (x in 1:ncol(bn.averaged, data.to.work))

  # Run the Path Analysis model
  pa.model.fit    <- lavaan::sem(paModel, sample.cov = data.cor, sample.nobs = nrow(data.to.work))
  pa.std.param    <- lavaan::standardizedSolution(pa.model.fit) #standardized parameters
  pa.fit.measures <- lavaan::fitMeasures(pa.model.fit)

  txtMeasures <- paste(     "\nX2: ",format(round(lavaan::fitMeasures(pa.model.fit, "chisq"),3),nsmall=3),
                       "  P-Value: ",format(round(lavaan::fitMeasures(pa.model.fit, "pvalue"),3),nsmall=3),
                            "  Df: ",format(round(lavaan::fitMeasures(pa.model.fit, "df"),3),nsmall=3),
                         "  X2/df :",format(round(lavaan::fitMeasures(pa.model.fit, "chisq"),3) /
                                            round(lavaan::fitMeasures(pa.model.fit, "df"),3),nsmall=3),

                          "\nAGFI : ",format(round(lavaan::fitMeasures(pa.model.fit, "agfi"),3),nsmall=3),
                           "  GFI : ",format(round(lavaan::fitMeasures(pa.model.fit, "gfi"),3),nsmall=3),
                          "  SRMR : ",format(round(lavaan::fitMeasures(pa.model.fit, "srmr"),3),nsmall=3),
                          "  RMR  : ",format(round(lavaan::fitMeasures(pa.model.fit, "rmr"),3),nsmall=3),

                       "\nCFI : ",format(round(lavaan::fitMeasures(pa.model.fit, "cfi"),3),nsmall=3),
                       "  TLI : ",format(round(lavaan::fitMeasures(pa.model.fit, "tli"),3),nsmall=3),
                       "  NFI : ",format(round(lavaan::fitMeasures(pa.model.fit, "nfi"),3),nsmall=3),

                       "\nRMSEA: ",format(round(lavaan::fitMeasures(pa.model.fit, "rmsea"),3),nsmall=3),

                       sep = "")

  #define the label that will go into the nodes
  lbls<-pa.model.fit@pta$vnames$ov.model[[1]]

  #  -0.5, 0.5 |    0, 0.5 |   0.5, 0,5
  #  -0.5,  0  |    0,   0 |   0.5,   0
  #  -0.5,-0.5 |    0,-0.5 |   0.5,-0.5

  cat ("\nExport a Path Analysis graph...\n") ################### cat

  grDevices::png(filename=pa.imgname, units="in", width=7, height=7, pointsize=6, res=150)

  semPlot::semPaths(pa.model.fit,  what="std", whatLabels = "par",  layout = "spring",
                    posCol = "#0000FF", negCol = "#FF0000", residuals=FALSE, nCharNodes=0, nodeLabels=lbls,
                    sizeMan=10, edge.label.cex=1.4, title=TRUE,  legend=FALSE, cut=0.1)

  graphics::title(txtMeasures)

  grDevices::dev.off()

  # Export a summary Path Analysis results and do not show results on console
  cat ("\nExport a summary Path Analysis results...\n") ################### cat
  pfile <- file(pa.name, "w")
  sink(pfile)

  print(lavaan::summary(pa.model.fit, standardized=TRUE))
  print(lavaan::standardizedSolution(pa.model.fit))
  #standardized parameters
  print(lavaan::fitMeasures(pa.model.fit))

  #print(tfile)
  sink()
  close(pfile)

  # Return the fitted model
  return (pa.model.fit)

} # gera.pa <-function
