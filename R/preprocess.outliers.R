#'Extract information of outliers
#'
#'This function receives a data set, the variable content and name, analyzes their content and extract outliers information, showing a boxplot and a histogram.
#'@param data.to.work is a data frame containing the variables.
#'@param variable.content is a variable with all content of variable in the data set.
#'@param variable.name is the name of variable to be verified.
#'@return a list with number of outliers and the variable content
#'@author Elias Carvalho
#'@references GUJARATI, Damodar N. Basic econometrics. Tata McGraw-Hill Education, 2009.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Load data sets from package
#'data(dataQuantC)
#'# Set parameters to function
#'variable.content <- dataQuantC$A
#'variable.name <- "A"
#'# Preprocess information
#'preprocess.information <- preprocess.outliers(dataQuantC, variable.content, variable.name)
#'num.outliers <- preprocess.information[[1]]
#'variable.content <- preprocess.information[[2]]
#'mean.of.outliers <- preprocess.information[[3]]
#'@export

preprocess.outliers <- function(data.to.work, variable.content, variable.name)
{
  # Count the numbers of NAs into the variable
  initial.nas <- sum(is.na(variable.content))

  # Calculate the mean value of variable
  variable.mean <- mean(variable.content, na.rm = T)

  # Extract all outliers from the variable
  outliers.of.variable <- grDevices::boxplot.stats(variable.content)$out

  # start mean.of.outliers variable
  mean.of.outliers <- 0

  # If some outliers were found continues the process
  if (length(grDevices::boxplot.stats(variable.content)$out) > 0)
  {
    # Extract the mean from the outliers
    mean.of.outliers <- mean(outliers.of.variable)

    # Set 2 rows and 2 columns to the graphics
    graphics::par(mfrow=c(2, 2), oma=c(0,0,3,0))

    # Show a boxplot and histogram for this variable
    graphics::boxplot(variable.content, main=paste("With outliers - Mean: ",  round(mean.of.outliers,2), sep = ""))
    graphics::hist(variable.content, main="", xlab=NA, ylab=NA)

    # Show the title for graphics
    graphics::title(paste("\nOutlier Check of variable: ", variable.name, sep = ""), outer=TRUE)

    # Insert NA into the place of outliers
    variable.content <- ifelse(variable.content %in% outliers.of.variable, NA, variable.content)

    # Count the quantity of NAs in the variable
    nas.with.outliers.inside <- sum(is.na(variable.content))

    # Show then number of outliers doing numbers of NAs now where outliers were transforme in NA - numbers of NAs in the begining
    cat("\n\n     Outliers identified..................:", nas.with.outliers.inside - initial.nas)

    # Calculates and show the proportion of outliers
    cat(paste("\n     Propotion of outliers................:", round((nas.with.outliers.inside - initial.nas) / sum(!is.na(variable.content))*100, 1)),"%",sep = "")

    # Show the mean of outliers found
    cat("\n     Mean of the outliers..................:", round(mean.of.outliers, 2))

    # Calculates the general mean of variable without outliers
    general.mean.without.outliers <- mean(variable.content, na.rm = T)

    # Show the general mean without removing and removing outliers
    cat("\n     Mean of data without removing outliers:", round(variable.mean, 2))
    cat("\n     Mean of data if we remove outliers....:", round(general.mean.without.outliers, 2))

    # Show a boxplot and histogram again, but now without outliers (that are NAs)
    graphics::boxplot(variable.content, main=paste("Without outliers - Mean: ",  round(general.mean.without.outliers,2), sep = ""))
    graphics::hist(variable.content, main="", xlab=NA, ylab=NA)

    # Mount a list of two parameters to be returned to the caller function
    list.of.parameters <- list(nas.with.outliers.inside - initial.nas, variable.content, mean.of.outliers)

  } else # if (outliers.of.variable > 0)

  {
    # Show then number of outliers doing numbers of NAs now where outliers were transforme in NA - numbers of NAs in the begining
    cat("\n\n     Outliers identified: 0")

    # Mount a list of two parameters to be returned, in this case the number of outliers is zero
    list.of.parameters <- list(0, variable.content, mean.of.outliers)

  } # else # if (outliers.of.variable > 0)

  # return the parameters generateds
  return(list.of.parameters)
} # preprocess.outliers <- function(data.to.work, variable.content, variable.name)
