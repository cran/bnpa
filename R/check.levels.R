#'Check if factor need to be converted in numeric.
#'
#'Receives a dataset and the maximum number of levels a factor would have. It will
#'transform all factor variables exceeding this limit.
#'
#'@param data.to.work is a dataset to be checked / updated.
#'@param level.max is the maximum number of levels to be tolerated.
#'@param dataset.name is a name of a data frame containing the variables to be checked.
#'@details
#'This function receives a dataset, scan all variables of it, check if each variable is
#'a factor and if "yes", it will check if the number of factors of this variable exceeds
#'the maximum number of levels previously passed as parameter. If it exceeds then will convert
#'the variable to numeric type.
#'@return NULL
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Use working datasets from package
#'data.to.work <- dataQualiN
#'head(data.to.work)
#'###################################################################################
#'# check.levels.R - Check if factor need to be converted in numeric.
#'###################################################################################
#'# Adding dichotomic data to data.to.work, function will return TRUE
#'data.to.work$Z <- round(runif(500, min=0, max=1000),2)
#'# Converting the numeric variable into factor
#'data.to.work$Z <- factor(data.to.work$Z)
#'# Showing the data structure
#'str(data.to.work)
#'# Identify all variables with more than 2 factors and convert it to numeric (the 'Z' variable)
#'level.max <- 2
#'dataset.name <- "data.to.work"
#'check.levels(data.to.work, level.max, dataset.name)
#'# Showing the data structure
#'str(data.to.work)
#'@export

check.levels <- function (data.to.work, level.max, dataset.name) {

  for (x in 1:length(data.to.work))
  {
    # load the names of variables
    number.factors <- paste("nlevels(data.to.work$", names(data.to.work)[x], ")", sep="")
    number.factors <- eval(parse(text=number.factors))

    # check if variable has more factors then your level
    if (number.factors > 0 && number.factors > level.max)
    {
      # Convert factors to num
      convert.factors.num <- paste("data.to.work$", names(data.to.work)[x], "<- as.numeric(sub(',' , '.', data.to.work$", names(data.to.work)[x], "))", sep="")
      eval(parse(text=convert.factors.num))
    }# if (number.factors > factor.max)
  } # for (x in 1:length(data.to.work))

  # Mount a command to confirm changing on data frame
  command.Assign <- paste(dataset.name," <- data.to.work", sep="")
  eval(parse(text=command.Assign))

  # update dataset in Global environment
  command.Assign <- paste("assign('", dataset.name,"',", dataset.name,",.GlobalEnv)",sep="")
  eval(parse(text=command.Assign))

} # check.levels <- function
