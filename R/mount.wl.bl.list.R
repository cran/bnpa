#'Mounts a white or black list
#'
#'This function receives a simple list with one or more couple of variables and mount a new data frame in "bnlearn" syntax.
#'The final result must return an object similar to the result of bnlearn command "data.frame(from = c('B', 'F'), to = c('F', 'B'))" that is more complex syntax.
#'@return A new data frame with the 'from' and 'to' variables
#'@param black_or_white_list is a list of couple of variables.
#'@author Elias Carvalho
#'@references Scutari M (2017). Bayesian Network Constraint-Based Structure Learning Algorithms: Parallel and Optimized Implementations in the bnlearn R Package. Journal of Statistical Software, 77(2), 1-20.
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("To your working directory")
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# Load data sets from package
#'data(dataQuantC)
#'# Show the first lines of data
#'head(dataQuantC)
#'# Learn the BN structure without black and white list
#'bn.structure <- hc(dataQuantC)
#'# Split graph panel in 2 columns
#'par(mfrow=c(1,2))
#'# Show the BN structure
#'bnlearn::graphviz.plot(bn.structure)
#'# Mounting the black list
#'black.list <- ("A-C,D-F")
#'black.list <- mount.wl.bl.list(black.list)
#'black.list
#'white.list <- ("A-B,D-G")
#'white.list <- mount.wl.bl.list(white.list)
#'white.list
#'# Learn the BN structure with black and white list
#'bn.structure <- hc(dataQuantC, whitelist = white.list, blacklist = black.list)
#'# Show the BN structure
#'bnlearn::graphviz.plot(bn.structure)
#'@export

mount.wl.bl.list <-function(black_or_white_list)
{
  # Prepare variables 'from' and 'to'
  # black_or_white_list <-"v1-v2,v3-v4,v5-v6"
  black_or_white_list1 <- "data.frame(from = c("
  black_or_white_list2 <- ", to = c("
  # Create a variable to control the start of each string to e mounted
  nstart <- 1
  # scan all content of variable
  for (x in 1 : nchar(black_or_white_list))
  {
    # if '-' is found start mount 'from'
    if (substr(black_or_white_list,x,x)=="-")
    {
      black_or_white_list1 <- paste(black_or_white_list1, "'", substr(black_or_white_list,nstart,x-1),"'", sep="")
      # prepare to start scanning 'to' part of variable
      nstart <- x+1
      # start scaning
      for (y in nstart:nchar(black_or_white_list))
      {
        # if ',' is found means 'to' part finished
        if (substr(black_or_white_list,y,y) == "," || y == nchar(black_or_white_list))
        {
          # check if is the end of black_or_white_list and choose the right positon of 'y'
          if (y == nchar(black_or_white_list))
            black_or_white_list2 <- paste(black_or_white_list2,"'", substr(black_or_white_list,nstart,y),"'", sep="")
          else {
            # in this case found a ",' then 'to' part end with 'y-1' position
            black_or_white_list2 <- paste(black_or_white_list2,"'", substr(black_or_white_list,nstart,y-1),"'", sep="")
            x <- nstart <- y+1
          } # else
          # leave 'y' loop and restart 'x' loop in position 'y'+1
          break
        } # if (substr(black_or_white_list,y,y)...
      } # for (y in nstart:nchar(black_or_white_list))
    } # if (substr(black_or_white_list,x,x)=="-"
  } # for (x in 1 : nchar(black_or_white_list))
  # close 'from' and 'to' with parentesis
  black_or_white_list1 <- paste(black_or_white_list1,")", sep = "")
  black_or_white_list2 <- paste(black_or_white_list2,"))", sep = "")
  # mount list and at the same time replace "' '" inside var with "','"
  cmdMontaVar <- gsub("''","','",(paste(black_or_white_list1,black_or_white_list2, sep="")))
  #return variable as a command
  return (eval(parse(text=cmdMontaVar)))
} # mount.wl.bl.list <-function(black_or_white_list)
