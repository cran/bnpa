#'Creates sub datasets.
#'
#'Receives a dataset and build a new sub dataset.
#'
#'@param data.to.work is a dataset from which a new sub dataset will be extracted.
#'@param variables is the list of variables from the dataset to build the new one.
#'@param dataset.name is the name of new dataset.
#'@details
#'This function receives a dataset, a list with the name of some variables of this dataset, and a name to save the new dataset as parameter. Then the function builds a new dataset with these variables and return it.
#'@return A new sub dataset.
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# load datasets from package
#'data.to.work <-dataQualiN
#'# Set variables to create a new dataset
#'variables <- c( "A","S","L","E","D")
#'# Set the new dataset name
#'dataset.name <- "data.work.new"
#'# Creates the subdataset
#'create.subdatasets(data.to.work, variables, dataset.name)
#'@export

create.subdatasets <-function(data.to.work, variables, dataset.name) {

  for (x in 1:length(variables))
  {
    if (x==1)
      commandExec <- paste0(dataset.name, " <- subset(data.to.work,"," select = c(", variables[x], sep="")
    else commandExec <- paste(commandExec, ", ", variables[x], sep = "")
  } # for (x in 1:length(variables))
  commandExec <- paste(commandExec, "))", sep = "")
  commandExec <- eval(parse(text=commandExec))

  # Mount a command to confirm changing on data frame
  command.Assign <- paste("assign('", dataset.name,"',", dataset.name,",.GlobalEnv)",sep="")
  eval(parse(text=command.Assign))

} # create.subdatasets
