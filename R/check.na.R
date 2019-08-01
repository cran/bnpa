#'Verify variables with NA
#'
#'This function receives a data set and calculates the number of NAs to each variable, then
#'calculates the percentual of existing NAs and inform the variables, number/percent of NAs.
#'@param data.to.work is a data set containing the variables to check NAs.
#'@return the number and percent of NAs.
#'@author Elias Carvalho
#'@references LITTLE, R J A; RUBIN, D B. Statistical analysis with missing data. John Wiley & Sons, 2014.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Use working data sets from package
#'data(dataQuantC)
#'head(dataQuantC)
#'# Adding NAs to dataQuantC # credits for the random NA code for: https://goo.gl/Xj6caY
#'dataQuantC <- as.data.frame(lapply(dataQuantC, function(cc) cc[ sample(c(TRUE, NA),
#'                              prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
#'# Checking the Nas
#'check.na(dataQuantC)
#'@export

check.na <- function(data.to.work)
{
  # Calculates the number of rows of a data set
  number.rows <- nrow(data.to.work)
  # Extract the names of variables from data set
  var.names <- names(data.to.work)
  # Creates a list
  var.to.remove <- list()
  # Set variables to work
  list.pos <- 0
  total.na <- 0
  # Scan the variables
  for (x in 1:length(var.names))
  {
    # Calculates the number and percentage of NAs
    command.na <- paste("sum(is.na(data.to.work$",var.names[x],"))", sep="")
    number.na <- eval(parse(text= command.na))
    percent.na <- round((number.na / number.rows),5)*100
    # If there are NAs show the values
    if (number.na > 0)
    {
      var.to.remove[[length(var.to.remove) + 1]] <- paste(var.names[x],": ",  percent.na, sep="")
      print(paste(" variable: ", var.names[x], " has ", number.na,
                  " NAs representing ", percent.na,"% of total", sep=""))
      # Calculates the total of NAs
      total.na <- total.na + number.na
    } # if (number.na > 0)
  } # for (x in 1:length(var.names))
  # Return the total of NAs
  cat("\nThere is a total of ",  total.na, " NAs on this file")
  return(total.na)
} # check.na <- function(data.to.work)
