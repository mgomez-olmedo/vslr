library(tools)
library(bnlearn)
library(stringr)

#' composes a text adding all the strings passed as
#' argument
#' @param objects data to convert to text
#' @param sep separator to use
#' @return string with the information
composeString <- function(objects, sep=NULL){
  output <- ""
  for(i in 1:length(objects)){
    output <- paste0(output, objects[i])
    if(!is.null(sep)){
      output <- paste0(output, sep)
    }
  }

  # return output
  output
}

#' composes a text adding all the strings passed as
#' argument
#' @param collection strings to compose into a single string
#' @param sep separator to use
#' @return composed string
composeStringForList <- function(collection, sep=NULL){
  output <- ""
  for(i in 1:length(collection)){
    output <- paste0(output, collection[i])
    if(!is.null(sep)){
      output <- paste0(output, sep)
    }
  }

  # return output
  output
}

#' composes a text adding all the strings passed as
#' argument as values with format
#' @param objects objects to convert to string
#' @param sep separator to use
#' @return string with the composed information
composeStringForValues <- function(objects, sep=NULL){
  output <- ""
  for(i in 1:length(objects)){
    output <- paste0(output, sprintf(": %2.4f", objects[i]))
    if(!is.null(sep)){
      output <- paste0(output, sep)
    }
  }

  # return output
  output
}

#' creates a new folder (if needed) for storing debug
#' information about the execution with a given file
#' @param dbname name of data to consider
#' @param maxIter number of iterations of the experiment to trace
#' @param nParentSets number of parent sets of the experiment
#' @return path to the created folder
createTraceFolder <- function(dbname, maxIter, nParentSets){
  # check if it is needed to create the general folder
  # for trace information
  tracePath <- "./traceFiles"

  if(!file.exists(tracePath)){
    # creates the folder
    cat("Creating trace folder: ", tracePath, "\n")
    dir.create(tracePath)
  }

  # new gets the base name for the file with the data
  # removing the extension and the first part with
  # data folder
  basename <- gsub(pattern = ".csv", "", dbname)
  basename <- gsub(pattern = "./data/", "", basename)
  basename <- paste(basename, maxIter, sep = "-")
  basename <- paste0(basename, "-")
  basename <- paste0(basename, nParentSets)

  # compose the path to this new folder in order to
  # create it if required
  folder <- paste(tracePath, basename, sep="/")
  if(!file.exists(folder)){
    # creates the folder
    cat("Creating specific trace folder: ", folder, "\n")
    dir.create(folder, recursive = TRUE)
  }

  # the method return the complete path
  folder
}

#' return the name of the net from dbname
#' @param dbname name of the data set
#' @return name of the net related to the data file
extractsNetName <- function(dbname){
  # divides dbname in parts (separated by -)
  res <- strsplit(basename(dbname), "-")

  # return the first part only
  res[[1]][1]
}

#' gets the numeric part of a file name
#' @param name name of the file to consider
#' @return numeric characters a string
getIndex <- function(name){
  # gets all numbers
  numbers <- stringr::str_extract_all(name, "\\d+")[[1]]

  # return the last one
  numbers[length(numbers)]
}

#' generate databases from a net file
#' @param netname name of net
#' @param nsamples number of samples to generate
#' @param nfiles number of files to generate
#' @export
generateDataBases <- function(netname, nsamples, nfiles){
  # compose the name of the file to read
  filename <- paste0("./data/net/", netname)
  filename <- paste0(filename, ".net")
  cat("reading file: ", filename, "\n")

  # read the net info
  net <- bnlearn::read.net(filename)

  # creates the folder if required
  storefolder <- paste0("./data/", netname)
  cat("analyzing file: ", storefolder, "\n")

  if(!file.exists(storefolder)){
    cat("   creating folder: ", storefolder)
    dir.create(storefolder)
  }

  # creates as well the folder for the number of
  # samples
  samplesfolder <- paste(storefolder, nsamples, sep="/")
  cat("analyzing file: ", samplesfolder, "\n")

  if(!file.exists(samplesfolder)){
    cat("   creating folder: ", samplesfolder)
    dir.create(samplesfolder)
  }

  # composes path
  pathname <- paste(samplesfolder, netname, sep="/")
  pathname <- paste(pathname, nsamples, sep="-")

  # generates the database
  for(i in 1:nfiles){
    # actual way of getting samples
    db <- bnlearn::rbn(net, n=nsamples)

    # composes the database name
    samplesfile <- paste(pathname, i, sep="-")
    samplesfile <- paste(samplesfile, ".csv", sep="")
    cat("         saving file ",samplesfile, "\n")

    # writes cvs file
    utils::write.csv(db, file=samplesfile, row.names=FALSE)
  }
}

#' generates a string with the content of a matrix
#' @param mat matrix to convert into string
#' @return string with matrix information
matrixToString <- function(mat){
  # gets the number of rows and columns
  rows <- nrow(mat)
  cols <- ncol(mat)

  # initializes the output
  output <- ""

  # gets values one by one
  for(i in 1:rows){
    for(j in 1:cols){
      output <- paste0(output, sprintf("%d  ", mat[i,j]))
    }

    # add a new line ar the end of the row
    output <- paste0(output, "\n")
  }

  # return output
  output
}

#' function for reading a database
#' @param filedb file with the data to load
#' @return data frame
readCSVDataBase <- function(filedb){
  # read the data
  db <- utils::read.csv(filedb,header=TRUE)

  # set column names to the data frame
  colnames <- colnames(db)

  # converts everything into factor
  db[,colnames] <- lapply(db[,colnames] , factor)

  # return db
  return(db)
}

#' function for reading a database
#' @param dbname file with the data to load
#' @return data frame
readDataBase <- function(dbname){
  # gets basename and path name
  base <- basename(dbname)

  # check the extension: it could be csv, dat or arff
  ext <- tools::file_ext(base)

  # considers each case
  switch(ext,
         # csv files
         "csv"={
           # read a csv file
           dat <- readCSVDataBase(dbname)
         })
  # return dat
  dat
}

