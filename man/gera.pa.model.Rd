% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gera.pa.model.R
\name{gera.pa.model}
\alias{gera.pa.model}
\title{Generates PA input model}
\usage{
gera.pa.model(bn.structure, data.to.work)
}
\arguments{
\item{bn.structure}{is a BN structure learned from data.}

\item{data.to.work}{is a data set containing the variables of the BN.}
}
\value{
the PA input modeo string
}
\description{
This function is called from 'gera.pa' function. It receives a BN structure and a data set, build a PA input model string based on BN structure and return it.
}
\examples{
# Clean environment
closeAllConnections()
rm(list=ls())
# Set enviroment
# setwd("To your working directory")
# Load packages
library(bnpa)
library(bnlearn)
# load data sets from package
data(dataQualiN)
# Show first lines
head(dataQualiN)
# Learn BN structure
bn.structure <- hc(dataQualiN)
bnlearn::graphviz.plot(bn.structure)
# Set variables
# Generates the PA model from bn structure
pa.model <- gera.pa.model(bn.structure, dataQualiN)
pa.model
}
\references{
Yves Rosseel (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2),1-36.
}
\author{
Elias Carvalho
}
