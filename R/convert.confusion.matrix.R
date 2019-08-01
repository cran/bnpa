#'Converts the position of any element of confusion matrix to VP, FP, FN, VN
#'
#'This function receives a confusion matrix and the matrix values to keep the order  VP, FP, FN, VN.
#'@param confusion.matrix is the confusion matrix to be converted.
#'@param cm.position is the position of your VP, FP, FN, VN at the confusion matrix.
#'@return a new confusion matrix
#'@author Elias Carvalho
#'@references STORY, Michael; CONGALTON, Russell G. Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, v. 52, n. 3, p. 397-399, 1986.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Creates a confusion matrix
#'confusion.matrix <-matrix(c(12395, 4, 377, 1), nrow=2, ncol=2, byrow=TRUE)
#'# Creates a vector with the position of VP, FP, FN, VN
#'cm.position <- c(4,3,2,1)
#'# Shows the original confusion matrix
#'confusion.matrix
#'# Converts the confusion matrix
#'confusion.matrix <- convert.confusion.matrix(confusion.matrix, cm.position)
#'# Shows the converted confusion matrix
#'confusion.matrix
#'@export

convert.confusion.matrix <-function(confusion.matrix, cm.position)
{
  # Check the length of confusion matrix
  if (length(confusion.matrix) != 4)
  {
    cat("\nThe confusion matrix must be 2x2")
    return ()
  }

  # Creates an empty confusion table 2x2
  confusion.table <-matrix(c(0, 0, 0, 0), nrow=2, ncol=2, byrow=T)

  # Creates a vector with the positions of a correct confusion matrix
  cm.position2 <- c("confusion.matrix[1,1]","confusion.matrix[1,2]","confusion.matrix[2,1]","confusion.matrix[2,2]")

  # Load confusion table line 1 column 1
  commandAssign <- paste("confusion.table [1,1] <-", cm.position2[cm.position[1]], sep = "")
  commandAssign
  eval(parse(text=commandAssign))

  # Load confusion table line 1 column 2
  commandAssign <- paste("confusion.table [1,2] <-", cm.position2[cm.position[2]], sep = "")
  commandAssign
  eval(parse(text=commandAssign))

  # Load confusion table line 2 column 1
  commandAssign <- paste("confusion.table [2,1] <-", cm.position2[cm.position[3]], sep = "")
  commandAssign
  eval(parse(text=commandAssign))

  # Load confusion table line 2 column 2
  commandAssign <- paste("confusion.table [2,2] <-", cm.position2[cm.position[4]], sep = "")
  commandAssign
  eval(parse(text=commandAssign))

  # Build a label to rows and columns of confusion table
  rownames(confusion.table) <- c("Modeled", "Modeled")
  colnames(confusion.table) <- c("Reality", "Reality")

  confusion.table

  # Calculates the indexes
  Sensibility <- round(confusion.table[1,1]/(confusion.table[1,1]+confusion.table[2,1])*100,2)
  Specificity <- round(confusion.table[2,2]/(confusion.table[1,2]+confusion.table[2,2])*100,2)
  Accuracy    <- round((confusion.table[1,1]+confusion.table[2,2])/(confusion.table[1,1]+confusion.table[1,2]+confusion.table[2,1]+confusion.table[2,2]),3) *100
  VPP         <- round(confusion.table[1,1]/(confusion.table[1,1]+confusion.table[1,2])*100,2)
  VPN         <- round(confusion.table[2,2]/(confusion.table[2,2]+confusion.table[2,1])*100,2)
  # Razão de Verossimilhança (likelihood ratio) - escrever o desempenho de um teste diagnóstico - http://www.misodor.com/VALIDTESTDIA.html
  RVP         <- round((confusion.table[1,1]/(confusion.table[1,1]+confusion.table[2,1])) /
                       (confusion.table[1,2]/(confusion.table[1,2]+confusion.table[2,2])),2)
  RVN         <- round((confusion.table[2,1]/(confusion.table[1,1]+confusion.table[2,1])) /
                       (confusion.table[2,2]/(confusion.table[2,2]+confusion.table[1,2])),2)

  # Show the indexes
  cat("\n\nConfusion Matrix of this model\n           R e a l i t y     \nModeled    VP ",  confusion.table[1,1], "   FP ", confusion.table[1,2],
      "\nModeled    FN ", confusion.table[2,1], "   VN ", confusion.table[2,2],
      "\n\nResults:",
      "\nSensibility: ", Sensibility," %",
      "\nSpecificity: ", Specificity," %",
      "\nPV(+)......: ", VPP," %",
      "\nPV(-)......: ", VPN," %",
      "\nAccuracy...: ", Accuracy," %",
      "\nLR(+)......: ", RVP," %",
      "\nLR(-)......: ", RVN," %",
      sep = "")

  # Return the converted confusion table
  return(confusion.table)
} # convert.confusion.matrix <-function
