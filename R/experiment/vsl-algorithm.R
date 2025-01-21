# include required source files
source("./R/VSLearning.R")
source("./R/ScoreComputation.R")
source("./R/ScoreHash.R")
source("./R/VSEvolutionInfo.R")
source("./R/QMatrix.R")
source("./R/fileUtils.R")
library(dplyr)

# process arguments
args <- commandArgs(trailingOnly = TRUE)

# stop the script if no command line argument
if(length(args) != 4){
  cat("Please include the following arguments: \n")
  cat("     name of the data set to process\n")
  cat("     number of parent sets\n")
  cat("     init method\n")
  cat("     number of partitions to consider!\n")
  stop("Required command line argument.")
}

# sets the name
name <- args[1]
nparentsets <- as.integer(args[2])
initmethod <- as.integer(args[3])
variants <- as.integer(args[4])

cat("arguments ------------------------------------\n")
cat("name: ", name, "\n")
cat("number of parent sets: ", nparentsets, "\n")
cat("init method: ", initmethod, "\n")
cat("number of datasets: ", variants)

# initializes the random number generation for being able to reproduce
# the results
set.seed(0)

# sets the number of decimals
options(digits = 4)
ndecimals <- 4

#' execute vsl learning algorithm on the target dataset and the
#' required number of partitions
#' @param name target dataset
#' @param variants number of partitions to analyze
#' @param initmethod initialization method
#' @param nparentsets number of parent sets to sample
#' @param ndecimals number of decimal numbers to consider for files
learn <- function(name, variants, initmethod, nparentsets, ndecimals){
  # for storing the final results
  results <- matrix(nrow = variants, ncol = 6)

  # sets the test and train dataset names
  testdbname <- paste("./data/", name, "/", name, "-test", sep = "")
  traindbname <- paste("./data/", name, "/",name, "-train", sep = "")

  # uses each of the train datasets
  for(index in 1:variants){
    # composes the base name
    dbname <- paste(traindbname, index, ".csv", sep = "")
    tdbname <- paste(testdbname, index, ".csv", sep = "")

    # creates VSLearning object: allows debug information
    vsl <- VSLearning$new(dbname, tdbname, debug=TRUE, initmethod, nparentsets)

    # calls learn method
    iterResults <- vsl$learn()

    # strore results
    results[index, ] <- iterResults
  }

  # convert results into a data frame
  df <- as.data.frame(results)

  # sets col-names
  colnames(df) <- c("map-tr", "map-ts", "bma-tr", "bma-ts", "bay-tr", "bay-ts")

  # limits the number of digits to 4
  df <- df %>%
    mutate_if(is.numeric, round, digits = ndecimals)

  # return df
  df
}

#' saves the results of learning into a csv file
saveLearnResults <- function(name, resultsdf){
  # compose the name of results file
  filename <- paste("./results/",name,"/", "vsl.csv", sep="")

  # saves the results
  write.csv(resultsdf, file = filename, row.names = FALSE)
}

# execute the sequence of actions for getting and storing the results
df <- learn(name, variants, initmethod, nparentsets, ndecimals)

# saves the data frame into a file
saveLearnResults(name, df)


