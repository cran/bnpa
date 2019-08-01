#'Generates a PA model
#'
#'This function receives a BN structure learned, the data set and some parameters and build a PA input model string. Then run the PA model using Structural Equation Model functions and export a PA graph and a PA model summary information.
#'@param bn.structure is a BN structure learned from data.
#'@param data.to.work is a data frame containing the variables of the BN.
#'@param pa.name is a variable to store the name of file to save PA parameters.
#'@param pa.imgname is a variable to store the name of file to save PA graph.
#'@param bn.algorithm is a list of algorithms to learn the BN structure.
#'@param bn.score.test is a list of tests to be used during BN structure learning.
#'@param outcome.var is the outcome variable.
#'@return NULL
#'@author Elias Carvalho
#'@references Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2),1-36.
#'@examples
#'\dontrun{
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("To your working directory")
#'# Load packages
#'library(bnpa)
#'# Load data sets from package
#'data(dataQualiN)
#'# Show first lines
#'head(dataQualiN)
#'# Learn BN structure
#'bn.structure <- bnlearn::hc(dataQualiN)
#'bnlearn::graphviz.plot(bn.structure)
#'# Set variables
#'pa.name<-"docPAHC"
#'pa.imgname<-"imgPAHC"
#'bn.algorithm<-"hc"
#'bn.score.test<-"aic-g"
#'outcome.var<-"D"
#'# Generates the PA model from bn structure
#'gera.pa(bn.structure, dataQualiN, pa.name, pa.imgname, bn.algorithm, bn.score.test, outcome.var)
#'}
#'@export

gera.pa <-function(bn.structure, data.to.work, pa.name, pa.imgname, bn.algorithm, bn.score.test, outcome.var)
{

  # Build a pa input model
  paModel <-gera.pa.model(bn.structure, data.to.work)

  # Creates a variable to store variables to be declared as ordered in sem command
  ordered.to.declare <- ""

  cat ("\nVeryfying binary and ordered categorical variables...\n") ################### cat

  # Verify if endogenous variables are binary or ordered categorial and mount a list do declare in sem command
  ##### ONLY FOR ENDOGENOUS ###
  # [1] and [2] are endogenous order factor &
  # [3] and [4] are exogenous order factor
  result.ordered.to.declare <- bnpa::check.ordered.to.pa(bn.structure, data.to.work)

  # Run the Path Analysis model
  cat ("\nRunning the Path Analysis model...\n") ################### cat

    # If there are ordered variables as endogenous then use ordered parameter to create the PA Model fit
    if (result.ordered.to.declare[[1]]!="")
    {
      # Scan all variables to tranform into numeric
      for (x in 1:ncol(data.to.work))
      {
        # When flag is 0 we
        flag.ordered <- 0
        for (y in 1:nrow(result.ordered.to.declare[[2]]))
        {
          # if the variable of data set match the variable to be transformed to numeric do it
          if (names(data.to.work)[x]==(result.ordered.to.declare[[2]][y,1]))
          {
            # if variable has parents
            if (length(bnlearn::parents(bn.structure, names(data.to.work[x]))) != 0)
            {
              # if variable is factor
              if (check.type.one.var(data.to.work,0,names(data.to.work[x]))==3)
              {
                # It must become ordered
                flag.ordered <- 1
              } # if (check.type.one.var(names(data.to.work[x]))
              # Leave for (y...
              break
            } # if (length(bnlearn::parents(bn.structure, names(data.to.work[x]))) != 0)
          } # if (names(data.to.work)[x]==(result.ordered.to.declare[[2]][y,1]))
        } # for (y in 1:nrow(result.ordered.to.declare[[2]]))

        # The variable do not need to be ordered, so transform it in numeric
        if (flag.ordered ==0)
        {
          # Transform the variable to numeric
          commandAssign <- paste("data.to.work$", names(data.to.work)[x], "<- as.numeric(data.to.work$", names(data.to.work)[x],")",sep = "")
          eval(parse(text=commandAssign))
        } # if (flag.ordered ==0)
      } # for (x in 1:ncol(data.to.work))
      # Creates a PA Model using 'categorical'ordered' parameters to endogenous variables
      commandAssign <- paste("pa.model.fit    <- lavaan::sem(paModel, data = data.to.work, ordered = c(", result.ordered.to.declare[[1]], "), std.lv=TRUE)", sep = "")
      eval(parse(text=commandAssign))

    } else # if (result.ordered.to.declare[[1]]!="")
    {
      # Scan all variables to tranform into numeric, once there are no variable to declare as ordered
      for (x in 1:ncol(data.to.work))
      {
        commandAssign <- paste("data.to.work$", names(data.to.work)[x], "<- as.numeric(data.to.work$", names(data.to.work)[x],")",sep = "")
        eval(parse(text=commandAssign))
      }# for (x in 1:ncol(data.to.work))
      # Creates a PA model in a standard way
      pa.model.fit    <- lavaan::sem(paModel, data.to.work, std.lv=TRUE)
    }

    # set a name to save the measure index
    sem.index.name <- paste("semIndex_", bn.algorithm, "_", bn.score.test, "_", nrow(data.to.work), ".xlsx", sep = "")
    #residuals.corr.name <- paste("residuals_corr_", bn.algorithm, "_", bn.score.test, "_", nrow(data.to.work), ".xlsx", sep = "")

    # Creates a dataframe to store indexed (X2, df, p-value, SRMR, RMSEA, CFI, TLI, MFI, X2/DF)
    sem.indexes.names   <- c("chisq", "df", "pvalue",  "srmr", "rmsea",   "cfi",   "tli", "mfi","chisq/df")
    sem.indexes.cutoff  <- c(     "",   "",  ">0.05", "<0.08", "<0.07", ">0.92", ">0.92",">0.92",  "<3")
    df.sem.indexes <- data.frame(matrix(ncol = 4, nrow = length(sem.indexes.names)))
    names(df.sem.indexes)[1] <- "Index"
    names(df.sem.indexes)[2] <- "Cut-off"
    names(df.sem.indexes)[3] <- "Value"
    names(df.sem.indexes)[4] <- "Has God Fit"

    cat ("\nStoring Path Analysis indexes...\n") ################### cat
    # Scanning sem indexes
    for (x in 1:length(sem.indexes.names))
    {
      # If is the last index
      if (x == length(sem.indexes.names)) # calculates x2/df
      {
        df.sem.indexes[x,1] <-  toupper(sem.indexes.names[x])
        df.sem.indexes[x,2] <-  (sem.indexes.cutoff[x])
        commandAssign <- paste("df.sem.indexes[x,3] <- round(df.sem.indexes[1,3]/df.sem.indexes[2,3],5)", sep = "")
        eval(parse(text=commandAssign))

      } else # if (x == length(sem.indexes.names))
      {
        df.sem.indexes[x,1] <-  toupper(sem.indexes.names[x])
        df.sem.indexes[x,2] <-  (sem.indexes.cutoff[x])
        commandAssign <- paste("df.sem.indexes[x,3] <- round(lavaan::fitMeasures(pa.model.fit,'", sem.indexes.names[x], "'),5)", sep = "")
        eval(parse(text=commandAssign))
      } # else # if (x == length(sem.indexes.names))

      # Verifies if the fit index is good or not according cut-off
      if(df.sem.indexes[x,2] != "")
      {
        commandAssign <- paste("df.sem.indexes[x,4] <- ifelse((df.sem.indexes[x,3]", df.sem.indexes[x,2],") == 'TRUE','Yes', 'No')", sep = "")
        eval(parse(text=commandAssign))
      } else # if(df.sem.indexes[x,2] != "")
      {
        df.sem.indexes[x,4] <- ""
      } # else # if(df.sem.indexes[x,2] != "")
    } # for (x in 1:length(sem.indexes.names))

    xlsx::write.xlsx(df.sem.indexes, sem.index.name)
    # xlsx::write.xlsx(residuals.corr$cov, residuals.corr.name)

    # Mount a line with main fit indexes
    txtMeasures <- paste("\n", df.sem.indexes[1,1],": ", format(round(df.sem.indexes[1,3],3), nsmall=3),
                         "  ", df.sem.indexes[2,1],": ", format(round(df.sem.indexes[2,3],3), nsmall=3),
                         "  ", df.sem.indexes[3,1],": ", format(round(df.sem.indexes[3,3],3), nsmall=3),
                         "   Chisq/df : ", format(round(df.sem.indexes[1,3] / df.sem.indexes[2,3],3), nsmall=3),
                         "\n", df.sem.indexes[4,1],": ", format(round(df.sem.indexes[4,3],3), nsmall=3),
                         "  ", df.sem.indexes[5,1],": ", format(round(df.sem.indexes[5,3],3), nsmall=3),
                         "  ", df.sem.indexes[6,1],": ", format(round(df.sem.indexes[6,3],3), nsmall=3),
                         "  ", df.sem.indexes[7,1],": ", format(round(df.sem.indexes[7,3],3), nsmall=3),
                         sep = "")

    # Export a summary Path Analysis results and do not show results on console
    cat ("\n\n==== Export a summary Path Analysis results...\n") ################### cat
    pfile <- file(pa.name, "w")
    sink(pfile)

    print(lavaan::summary(pa.model.fit, rsquare = TRUE, standardized = TRUE))
    print(lavaan::standardizedSolution(pa.model.fit))
    #standardized parameters
    print(lavaan::fitMeasures(pa.model.fit))

    #print(tfile)
    sink()
    close(pfile)

    # Set the PA param and image name
    pa.name <- paste("docPA_Param_", bn.algorithm, "_", bn.score.test,"_", nrow(data.to.work), ".txt", sep = "")
    pa.imgname <- paste("imgPA_", bn.algorithm, "_", bn.score.test,"_", nrow(data.to.work), ".png", sep = "")

    # Show a message
    cat ("\n\n==== Export a Path Analysis graph...\n") ################### cat
    # Set name and type of image for PA model
    grDevices::png(filename=pa.imgname, units="in", width=7, height=7, pointsize=6, res=150)
    # Plot PA model
    semPlot::semPaths(pa.model.fit,
                      #"col",
                      "stand",               # This will display the parameter estimates as weighted edges
                      edge.color="black",    # A value indicating the color of all edges
                      edge.label.cex = 1.5,  # Controls the font size of the edge labels.
                      freeStyle=1,           # how the nodes should be placed.
                      intercepts = FALSE,    # should intercepts be included in the path diagram?
                      label.scale=TRUE,      # Logical indicating if labels should be scaled to fit the node.
                      layout = "spring",     # indicating how the nodes should be placed.
                      mar = c(2, 2, 2, 2),   # Same as the 'mar' argument in qgraph.
                      nCharNodes=0,          # Number of characters to abbreviate node labels, 0 omit abbreviation.
                      residuals=FALSE,       # should residuals (and variances) be included in the path diagram?
                      rotation = 1,          # indicating the rotation of the layout when "tree" or "tree2" layout is used. 1, 2, 3 and 4 indicate
                      # that exogenous variables are placed at the top, left side, bottom and right side respectively.
                      sizeMan=15,            # Width of the manifest nodes
                      style = "ram",         # Style to use, indicates what the (residual) variances look like. Use "ram", "mx" or "OpenMx"
                      # for double headed selfloops and "lisrel" for single headed edges with no node as origin.
                      thresholds = FALSE
    )
    # Turn off device and save into disk
    grDevices::dev.off()
} # gera.pa <-function
