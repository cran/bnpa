#'Verify variables with NA.
#'
#'Receives a dataset and check the existence of NAs.
#'
#'@param data.to.work is a data frame containing the variables to check NAs.
#'
#'@details
#'This function receives a dataset and calculates the number of NAs to each variable, then
#'calculates the percentual of existing NAs and inform the variables, number/percent of NAs.
#'@return the number and percent of NAs.
#'@author Elias Carvalho
#'@examples
#'# Clean Environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'# Use working datasets from package
#'data.to.work <- dataQuantC
#'head(data.to.work)
#'###################################################################################
#'# check.NAs.R - Verify variables with NA..
#'###################################################################################
# Adding NAs to data.to.work # credits for the random NA code for: https://goo.gl/Xj6caY
#'data.to.work <- as.data.frame(lapply(data.to.work, function(cc) cc[ sample(c(TRUE, NA),
#'                prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
#'# Checking the Nas
#'check.na(data.to.work)
#'@export

check.na <- function(data.to.work) {
  number.rows <- nrow(data.to.work)
  var.names <- names(data.to.work)
  var.to.remove <- list()
  list.pos <- 0
  total.na <- 0

  for (x in 1:length(var.names))
  {
    command.na <- paste("sum(is.na(data.to.work$",var.names[x],"))", sep="")
    number.na <- eval(parse(text= command.na))
    percent.na <- round((number.na / number.rows),5)*100

    if (number.na > 0)
    {
      var.to.remove[[length(var.to.remove) + 1]] <- paste(var.names[x],": ",  percent.na, sep="")
      print(paste(" variable: ", var.names[x], " number of NAs: ", number.na,
                  " Percentual: ", percent.na,"%", sep=""))
      total.na <- total.na + number.na
    }

  } # for (x in 1:length(var.names))

  return(total.na)

} # check.na <- function(data.to.work)
