% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.confusion.matrix.R
\name{convert.confusion.matrix}
\alias{convert.confusion.matrix}
\title{Converts the position of any element of confusion matrix to VP, FP, FN, VN}
\usage{
convert.confusion.matrix(confusion.matrix, cm.position)
}
\arguments{
\item{confusion.matrix}{is the confusion matrix to be converted.}

\item{cm.position}{is the position of your VP, FP, FN, VN at the confusion matrix.}
}
\value{
a new confusion matrix
}
\description{
This function receives a confusion matrix and the matrix values to keep the order  VP, FP, FN, VN.
}
\examples{
# Clean environment
closeAllConnections()
rm(list=ls())
# Set enviroment
# setwd("to your working directory")
# Load packages
library(bnpa)
# Creates a confusion matrix
confusion.matrix <-matrix(c(12395, 4, 377, 1), nrow=2, ncol=2, byrow=TRUE)
# Creates a vector with the position of VP, FP, FN, VN
cm.position <- c(4,3,2,1)
# Shows the original confusion matrix
confusion.matrix
# Converts the confusion matrix
confusion.matrix <- convert.confusion.matrix(confusion.matrix, cm.position)
# Shows the converted confusion matrix
confusion.matrix
}
\references{
STORY, Michael; CONGALTON, Russell G. Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, v. 52, n. 3, p. 397-399, 1986.
}
\author{
Elias Carvalho
}
