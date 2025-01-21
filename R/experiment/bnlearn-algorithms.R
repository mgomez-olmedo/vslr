# include required source files
library(bnlearn)
library(dplyr)
library(reshape2)
library(ggplot2)
source("./R/fileUtils.R")

# initializes the random number generation for being able to reproduce
# the results
set.seed(0)

# sets the number of decimals
ndecimals <- 4

# sets the list of algorithms to use
algorithms <- c("hc", "fast.iamb", "inter.iamb", "iamb.fdr", "mmpc",
                "si.hiton.pc", "hpc", "mmhc", "rsmax2", "h2pc",
                "chow.liu", "aracne")

# sets thelist of parameters to be used for the algorithms
list1 <- list(maxp = 4, optimized = TRUE)
list2 <- list(max.sx = 4, undirected = FALSE)
list3 <- list(max.sx = 4, undirected = TRUE)
list4 <- list()

# relates parameter lists with algorithms
parameters <- list(list1, list2, list2, list2, list2, list2, list3,
                   list4, list4, list4,list4, list4)

# process arguments: name and number of variants
args <- commandArgs(trailingOnly = TRUE)

# stop the script if no command line argument
if(length(args) != 2){
  cat("Please include the following arguments: \n")
  cat("      name of the data set to process\n")
  cat("      number of partitions to consider\n")
  stop("Required command line argument.")
}

# sets the name
name <- args[1]
variants <- as.integer(args[2])
cat("arguments - name: ", name, " variants: ", variants, "\n")

#' execute bnlearn learning algorithms on the target datasets
#' return the dataframe of results
learn <- function(algorithms, parameters, name, variants){
  # compose train and test dataset names
  dbtrainname <- paste("./data/", name, "/", name, "-train", sep="")
  dbtestname <- paste("./data/", name, "/", name, "-test", sep = "")

  # for storing the final results: as much columns as the number of algorithms
  # multiplied by two (a dataset of train and another for test)
  results <- matrix(nrow = variants, ncol = 2*length(algorithms))

  #considers each algorithm
  for(algindex in 1:length(algorithms)){
    cat("Algorihtm : ", algorithms[algindex], "\n")
    params <- parameters[[algindex]]

   # uses each of the train datasets
   for(index in 1:variants){
     cat("   iteration: ", index, "\n")
     # composes the base name
     dbtrainname_index <- paste(dbtrainname, index, ".csv", sep="")
     dbtestname_index <- paste(dbtestname, index, ".csv", sep="")
     cat("train data set: ", dbtrainname_index, "\n")
     cat("test data set: ", dbtestname_index, "\n")

     # read the train dataset
     datatr <- readCSVDataBase(dbtrainname_index)

     # read the test dataset
     datatt <- readCSVDataBase(dbtestname_index)

     # computes the ratio between both data sets
     ratio = nrow(datatr) / nrow(datatt)

     # learn with the method
     paramsf <- append(params, list(x = datatr))
     model <- do.call(algorithms[algindex], paramsf)
     model <- bnlearn::cextend(model, strict = FALSE)

     # gets the undirected arcs
     undirected <- bnlearn::undirected.arcs(model)

     # remove undirected arcs if needed
     if(nrow(undirected) != 0){
       for(j in 1:nrow(undirected)){
         arc <- undirected[j,]
         model <- bnlearn::drop.arc(model, from = arc[1], to = arc[2])
       }
     }

     # compute the score
     sctr <- bnlearn::score(model, datatr, type = "bde", iss = 1, by.node = FALSE)

     # determine the column for the results
     column <- (algindex - 1)*2 + 1

     # calls learn method
     results[index, column] <- sctr

     # clean data variable
     rm(datatr)

     # compute the score on test
     sctt <- bnlearn::score(model, datatt, type = "bde", iss = 1, by.node = FALSE) * ratio

     # clean data variable
     rm(datatt)

     # store the results
     results[index, column+1] <- sctt
   }
  }

  # save the results as a file in the corresponding folder
  resultsdf <- as.data.frame(results)

  # compose the list of column names
  dfnames <- c()
  for(algindex in 1:length(algorithms)){
    dfnames <- c(dfnames, paste0(algorithms[algindex],"-tr"),
                 paste0(algorithms[algindex],"-ts"))
  }

  # sets colnames
  colnames(resultsdf) <- dfnames

  # limits the number of digits to 4
  resultsdf <- resultsdf %>%
    mutate_if(is.numeric, round, digits = ndecimals)

  # return results data frame
  resultsdf
}

#' generates the boxplots of the required algorithms and datasets
#' and saves the corresponding pdf file
#' @param name name of the target dataset
#' @param algorithms vector of employed algorithms
#' @param train boolean flag to show if results on train datasets
#'              must be included or not
#' @param columns indices of the algorithms to consider. If this
#'              parameter is not set, then all the algorithms are
#'              plotted
generateBoxplots <- function(name, algorithms, train, columns = c()){
  # read data
  filename <- paste("./results/",name,"/bnlearn.csv", sep="")
  pdffilename <- paste("./results/", name, "/bnlearn-boxplots.pdf", sep="")
  data <- read.csv(filename)

  # generate the pdf with boxplot
  pdf(pdffilename)

  # if there are no columns specified, then all the algorithms will
  # be considered
  if(length(columns) == 0){
    columns = 1:length(algorithms)
  }

  # compose the vector of columns to consider
  if(train == FALSE){
    # just consider indices multiplied by two (only consider odd columns
    # of data)
    targetcols <- 2*columns
  }
  else{
    # all the columns will be plotted
    targetcols <- 1:(length(algorithms)*2)
  }

  # process data for preparing boxplots generation
  datas <- reshape2::melt(data[,targetcols])
  colnames(datas) <- c("score", "value")

  # generate plot
  plot <- ggplot2::ggplot(data = melt(datas), aes(x=score, y=value, fill = score)) +
    geom_boxplot() +
    theme(legend.position="none", axis.text.x = element_text(angle = 90,
                                                vjust = 0.5, hjust=1))

  # print the plot
  print(plot)

  # close the dev
  dev.off()
}

#' saves the results of learning into a csv file
saveLearnResults <- function(name, resultsdf){
  # compose the name of results file
  filename <- paste("./results/",name,"/", "bnlearn.csv", sep="")

  # saves the results
  write.csv(resultsdf, file = filename, row.names = FALSE)
}

# execute the sequence of actions for getting and storing the results
df <- learn(algorithms, parameters, name, variants)

# saves the data frame into a file
saveLearnResults(name, df)

# generates boxplots for test datasets
generateBoxplots(name, algorithms, FALSE)

