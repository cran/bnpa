% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.types.R
\name{check.types}
\alias{check.types}
\title{Verify types of variable}
\usage{
check.types(data.to.work, show.message = 0)
}
\arguments{
\item{data.to.work}{is a data set containing the variables to be verified.}

\item{show.message}{is a parameter indicating if the function will or not show a message.}
}
\value{
A variable with the code indicating the type of variable and a message (or not)
}
\description{
This function receives a data set as parameter and check each type of variable returning a number indicating the type of variables in the whole data set.
The variables can be 1=integer, 2=numeric, 3=factor, 4=integer and numeric, 5=integer and  factor, 6=numeric and factor, 7=integer,
numeric and factor, 8=character.
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
data(dataQuantC)
# Show first lines of data set
head(dataQuantC)
# Check and return a numeric value
show.message <- 1
bnpa::check.types(dataQuantC, show.message)
# Adding random data to dataQuantC, function will return TRUE
dataQuantC$Z <- round(runif(500, min=0, max=1000),2)
# Converting the numeric variable into factor
dataQuantC$Z <- factor(dataQuantC$Z)
# Check and return a numeric value correspondig to: 1=integer, 2=numeric, 3=factor, 4=integer and
# numeric, 5=integer and  factor, 6=numeric and factor or 7=integer, numeric and factor.
show.message <- 1
bnpa::check.types(dataQuantC, show.message)
# Supressing the message
show.message <- 0
bnpa::check.types(dataQuantC, show.message)
}
\references{
GUJARATI, Damodar N. Basic econometrics. Tata McGraw-Hill Education, 2009.
}
\author{
Elias Carvalho
}
