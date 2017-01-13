#'Mounts a white or black list
#'
#'Using a list of variables passed as parameter this function mounts a white list or a black list in bnlearn package sintax.
#'
#'@param vlist is a list of couple of variables.
#'@details
#'This function receives a list with one or more couple of variables and mount a new data frame in "bnlearn"
#'syntax. The final result must return an object similar to "data.frame(from = c('B', 'F'), to = c('F', 'B'))"
#'@return A new data frame with the 'from' and to 'to' variables.
#'
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Load packages
#'library(bnpa)
#'library(bnlearn)
#'# Load data sets from package
#'data.to.work<-dataQuantC
#'head(data.to.work)
#'###################################################################################
#'# mount.wl.bl.list.R - Mounts a white or black list.
#'###################################################################################
#'# Mounting the black list
#'vlist <- ("A-C,D-F")
#'bl <- mount.wl.bl.list(vlist)
#'# Mounting the white list
#'vlist <- ("A-B,D-G")
#'wl <- mount.wl.bl.list(vlist)
#'# Learn the Bayesian Networks structure without black and white list
#'bn.structure <- hc(data.to.work, whitelist = wl, blacklist = bl)
#'# Set the name of a text to save the Bayesian Networks parameters
#'param.name <- "docbnparamHC.txt"
#'# Generates a Bayesian Networks parameters from the Bayesian Networks structure and 
#'# write a text file with this
#'bn.param <- gera.bn.param(bn.structure, data.to.work, param.name)
#'# Set the graph name
#'graph.name <- "imgBNHC"
#'# Save the graph
#'mount.graph(bn.structure, bn.param, graph.name, data.to.work)
#'@export

mount.wl.bl.list <-function(vlist) {
  # Prepare variables 'from' and 'to'
  # vlist <-"v1-v2,v3-v4,v5-v6"
  vlist1 <- "data.frame(from = c("
  vlist2 <- ", to = c("

  # Create a variable to control the start of each string to e mounted
  nstart <- 1

  # scan all content of variable
  for (x in 1 : nchar(vlist))
  {
    # if '-' is found start mount 'from'
    if (substr(vlist,x,x)=="-")
    {
      vlist1 <- paste(vlist1, "'", substr(vlist,nstart,x-1),"'", sep="")

      # prepare to start scanning 'to' part of variable
      nstart <- x+1

      # start scaning
      for (y in nstart:nchar(vlist))
      {
        # if ',' is found means 'to' part finished
        if (substr(vlist,y,y) == "," || y == nchar(vlist))
        {
          # check if is the end of vlist and choose the right positon of 'y'
          if (y == nchar(vlist))
            vlist2 <- paste(vlist2,"'", substr(vlist,nstart,y),"'", sep="")
          else {
            # in this case found a ",' then 'to' part end with 'y-1' position
            vlist2 <- paste(vlist2,"'", substr(vlist,nstart,y-1),"'", sep="")
            x <- nstart <- y+1
          } # else
          # leave 'y' loop and restart 'x' loop in position 'y'+1
          break
        } # if (substr(vlist,y,y)...
      } # for (y in nstart:nchar(vlist))
    } # if (substr(vlist,x,x)=="-"
  } # for (x in 1 : nchar(vlist))

  # close 'from' and 'to' with parentesis
  vlist1 <- paste(vlist1,")", sep = "")
  vlist2 <- paste(vlist2,"))", sep = "")

  # mount list and at the same time replace "' '" inside var with "','"
  cmdMontaVar <- gsub("''","','",(paste(vlist1,vlist2, sep="")))

  #return variable as a command
  return (eval(parse(text=cmdMontaVar)))

} # mount.wl.bl.list <-function(vlist)
