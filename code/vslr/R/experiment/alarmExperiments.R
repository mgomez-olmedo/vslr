# include required source files
source("./R/VSLearning.R")
source("./R/ScoreComputation.R")
source("./R/ScoreHash.R")
source("./R/VSEvolutionInfo.R")
source("./R/QMatrix.R")
source("./R/fileUtils.R")

# initializes the random number generation for being able to reproduce
# the results
set.seed(0)

# sets the number of decimals
options(digits = 4)

# sets the number of parents to sample
nparentsets <- 100

# sets the test dataset
testdbname <- "./data/alarm/10000/alarm-10000-1.csv"

# sets the initialization method: model learnt with bnlearn hc
# algorithm
initmethod <- 4

# defines the number of different datasets to consider
variants <- 5

# for storing the final results
results <- matrix(nrow = variants, ncol = 6)

# uses each of the train datasets
for(index in 1:5){
  # composes the base name
  dbname <- "./data/alarm/600/alarm-600-"
  dbname <- paste0(dbname, index)
  dbname <- paste0(dbname, ".csv")

  # creates VSLearning object
  vsl <- VSLearning$new(dbname, testdbname, initmethod, nparentsets)

  # calls learn method
  iterResults <- vsl$learn()
  results[index, ] <- iterResults
}

# print all the results
cat("---------------- global results -----------------\n")
print(results)


