#'Indentifies and gives an option to remove outliers
#'
#'This function receives a data set, scan all variables e for each one, verifies if there are outliers and ask if we wish to remove them. We can pass a parameter where we set if the function remove it automatically or will ask before.
#'@param data.to.work is a data set with variables to be checked.
#'@param ask.before control if the process will ask for confirmation or not.
#'@return NULL
#'@author Elias Carvalho
#'@examples
#'# Clean environment
#'closeAllConnections()
#'rm(list=ls())
#'# Set enviroment
#'# setwd("to your working directory")
#'# Load packages
#'library(bnpa)
#'# Load the data set
#'data(dataQuantC) # Pre-Loaded
#'# Set a variable to ask before remove outlier or not
#'ask.before = "Y" # or ask.before = "N"
#'# Call the procedure to check if there are outliers
#'dataQuantC <- check.outliers(dataQuantC, ask.before)
#'@export

check.outliers <- function(data.to.work, ask.before) {

  # scan all variables from the data set and check and remove outliers for each one
  for (x in 1:length(names(data.to.work)))
  {

    # Mount a variable to load each variable and pass to the check.outlier function
    commandAssign <- paste("variable.content <- data.to.work$", names(data.to.work)[x], sep = "")
    eval(parse(text=commandAssign))

    # set the currently variable name
    variable.name <- names(data.to.work)[x]

    # Check the type of variable
    type.variable <- bnpa::check.type.one.var(data.to.work, show.message=0, variable.name)

    if (type.variable == 1)
    {
      variable.type <- "integer"
    }  else if (type.variable == 2) # if (type.variable == 3)
    {
      variable.type <- "numeric"
    } else if (type.variable == 3) # if (type.variable == 3)
    {
      variable.type <- "factor"
    } else if (type.variable == 8) # if (type.variable == 3)
    {
      variable.type <- "character"
    } else if (type.variable == 9) # else if type.variable == 8)
    {
      variable.type <- "dichotomic"
    } # else if (type.variable == 9)

    # Only 1=integer, 2=numeric and 4=numeric variables need to check outliers
    if (type.variable == 1 ||
        type.variable == 2 ||
        type.variable == 4)
    {
      # Show the variable being evaluated
      cat(paste("\n\n===================================================================", sep = ""))
      cat(paste("\nVerifying the outliers of variable: ", variable.name," (type ",variable.type, ")" , sep = ""))

      # Verify and extract information about the outliers
      preprocess.information <- preprocess.outliers(data.to.work, variable.content, variable.name)

      # Take the number of outliers and variable cotent
      num.outliers <- preprocess.information[[1]]
      variable.content <- preprocess.information[[2]]
      mean.of.outliers <- preprocess.information[[3]]

      # Set the response variable as 'no'
      response <- "no"

      # If there are some outlier
      if (num.outliers > 0)
      {
        # Ask what to do
        if (ask.before == "Y")
        {
          response <- readline(prompt=paste("\n\nRemove outliers of ", variable.name," and replace them with NA ? [Y/N] or [ENTER] to exit: ", sep = ))
        } else # if (ask.before == "yes")
        {
          response <- "yes"
        } # else # if (ask.before == "yes")

        # If nothing was answered the process finishes
        if (response=="")
          return(data.to.work)

        # The answer is "YES"
        if(response == "y" | response == "Y" | response == "yes" | response == "YES")
        {
          # Creates a counter
          nu.iteraction <- 0

          # Executes the pre processing to extract information about outliers
          while (num.outliers > 0)
          {

            # Show the number of steps to remove outliers
            nu.iteraction <- nu.iteraction + 1
            cat(paste("\n\nResult of iteracton ", nu.iteraction, sep = ""))

            # Load the new data into variable
            commandAssign <- paste("data.to.work$", variable.name, " <- invisible(variable.content)", sep = "")
            eval(parse(text=commandAssign))

            # Pre process again to identify new outliers
            preprocess.information <- preprocess.outliers(data.to.work, variable.content, variable.name)

            # Take the number of outliers and variable cotent
            num.outliers <- preprocess.information[[1]]
            variable.content <- preprocess.information[[2]]

          } # while (num.outliers > 0)

        } else # if(response == "y" | response == "yes")
        {
          cat("\n     Nothing changed")
        } # else{ # if(response == "y" | response == "yes")

        if (num.outliers == 0)
        {

          # Show a message
          cat("\n\n          All outliers successfully removed")

          # Show a boxplot and histogram for this variable
          # graphics::boxplot(variable.content, main= "Final result")
          # graphics::hist(variable.content, main="", xlab=NA, ylab=NA)

          # Mount a command to confirm changing on data frame
          command.Assign <- paste("assign('data.to.work', data.to.work,.GlobalEnv)", sep = "")
          eval(parse(text=command.Assign))

        } # if (num.outliers == 0)

      } # if (num.outliers > 0)

    } # if (type.variable == 1 || type.variable == 2 || type.variable == 4)

  } # for (x in 1:length(names(data.to.work)))

  # return the data set
  return(data.to.work)

} # check.outliers <- function(data.to.work)
