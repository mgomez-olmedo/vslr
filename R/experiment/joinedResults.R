library(reshape2)
library(ggplot2)

#' the names of the columns on the joined df (all.csv) are the following
#' ones:
#' 1 - map.tr
#' 2 - map.ts
#' 3 - bma.tr
#' 4 - bma.ts
#' 5 - bay.tr
#' 6 - bay.ts
#' 7 - hc.tr
#' 8 - hc.ts
#' 9 - fast.iamb.tr
#' 10 -fast.iamb.ts
#' 11 - inter.iamb.tr
#' 12 - inter.iamb.ts
#' 13 - iamb.fdr.tr
#' 14 - iamb.fdr.ts
#' 15 - mmpc.tr
#' 16 - mmpc.ts
#' 17 - si.hiton.pc.tr
#' 18 - si.hiton.pc.ts
#' 19 - hpc.tr
#' 20 - hpc.ts
#' 21 - mmhc.tr
#' 22 - mmhc.ts
#' 23 - rsmax2.tr
#' 24 - rsmax2.ts
#' 25 - h2pc.tr
#' 26 - h2pc.ts
#' 27 - chow.liu.tr
#' 28 - chow.liu.ts
#' 29 - aracne.tr
#' 30 - aracne.ts

# process arguments
args <- commandArgs(trailingOnly = TRUE)

# stop the script if no command line argument
if(length(args) != 1){
  cat("Please include the following arguments: \n")
  cat("     name of the data set to process\n")
  stop("Required command line argument.")
}

# sets the name
name <- args[1]

#' by default all the columns will be plotted, but we can change the final
#' boxplot changing this vector
columns <- seq(1:30)

#' generates the boxplots of the required algorithms and datasets
#' and saves the corresponding pdf file
#' @param name name of the target dataset
#' @param train boolean flag to show if results on train datasets
#'              must be included or not
#' @param test boolean flag to show if results on test datasets
#'              must be included or not
#' @param map boolean flag for setting if map measure is considered
#' @param bma boolean flag for setting if bma measure is considered
#' @param bay boolean flag for setting if bay measure is considered
generateBoxplots <- function(name, train = FALSE, test = TRUE,
                             map = FALSE, bma = TRUE, bay = FALSE){
  # read data
  filename <- paste("./results/",name,"/all.csv", sep="")
  data <- read.csv(filename)

  # sets initial value for columns
  bnlearn <- c()
  seed <- c()

  # add columns according to parameters
  # add map if required
  if(map){
    seed <- c(1, seed)
  }

  # add bma if required
  if(bma){
    seed <- c(3, seed)
  }

  # add bayes if required
  if(bay){
    seed <- c(5, seed)
  }

  # sets bnlearn and vsl measures to consider
  if(train){
    bnlearn <- seq(from = 7, to = 30, by = 2)

    # sets vsl measures
    all <- sort(c(seed, bnlearn))
  }
  if(test){
    bnlearn <- c(bnlearn, seq(8, to = 30, by = 2))
    # sets vsl measures
    seedt <- seed+1
    if(train){
      seedt <- c(seed, seedt)
    }
    all <- sort(c(seedt, bnlearn))
  }

  # compose the vector of columns to consider
  basename <- paste("./results/", name, "/all", sep = "")
  if(train == TRUE){
    # add train to basename
    basename <- paste(basename, "-train", sep="")
  }
  if(test == TRUE){
    basename <- paste(basename, "-test", sep="")
  }

  # add the extension to basename
  pdffilename <- paste(basename, "-boxplots.pdf", sep="")

  # generate the pdf with boxplot
  pdf(pdffilename)

  # process data for preparing boxplots generation
  datas <- reshape2::melt(data[, all])
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

#' joing vsl and bnlearn results into a single dataframe for
#' generating posterior plots or analysis with it
#' @param name target dataset to consider
joinVslBnlearnResults <- function(name){
  # compose names of partial results
  pathname <- paste("./results/", name, "/", sep = "")
  vslfile <- paste(pathname, "vsl.csv", sep = "")
  bnlearnfile <- paste(pathname, "bnlearn.csv", sep = "")

  # compose name of joined results
  joinedfile <- paste(pathname, "all.csv", sep = "")

  # read data
  vsldf <- read.csv(vslfile)
  bnlearndf <- read.csv(bnlearnfile)

  # now join both data sets
  df <- cbind(vsldf, bnlearndf)

  # finally save the complete df
  write.csv(df, file = joinedfile, row.names = FALSE)
}

#' the process of generation consists of generating the complete
#' dataframe joining vsl and bnlearn results
df <- joinVslBnlearnResults(name)

#' generate the boxplots (by default only test data will be plotted and only
#' bma for vsl algorithms)
generateBoxplots(name, train = FALSE, test = TRUE, map = FALSE,
                 bma = TRUE, bay = FALSE)

#' plotting also train and test
# generateBoxplots(name, train = TRUE, test = TRUE, map = FALSE,
#                 bma = TRUE, bay = FALSE)


