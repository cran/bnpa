% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.dummies.R
\name{create.dummies}
\alias{create.dummies}
\title{Creates dummy variables in the data set and remove master variables}
\usage{
create.dummies(data.to.work, dummy.vars)
}
\arguments{
\item{data.to.work}{is a data set containing the variables to tranform.}

\item{dummy.vars}{are the variables to be transformed.}
}
\value{
the new data set
}
\description{
This function receives a data set and the name of variables to be transformed into dummies. Then it create the dummy variables, transform it into numeric (to work with PA generation) and
remove the master variables that originates the dummies.
}
\examples{
# Clean environment
closeAllConnections()
rm(list=ls())
# Set enviroment
# setwd("to your working directory")
# Load packages
library(bnpa)
# Use working data sets from package
data(dataQualiN)
# Show the structure before
str(dataQualiN)
# Set possible dummy variables
dummy.vars <- c("A", "B")
# Create dummies
dataQualiN <- bnpa::create.dummies(dataQualiN, dummy.vars)
# Show the structure before
str(dataQualiN)
}
\references{
Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2),1-36.
}
\author{
Elias Carvalho
}
