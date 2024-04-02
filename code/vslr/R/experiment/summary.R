library(ggplot2)
library(reshape2)

#' read summary data
readSummaryData <- function(filename){
  data <- read.csv(filename)
}

#' gets average and deviation of a column
#' @param data data frame with data
#' @param column target column
getStatistics <- function(data, column){
  datac <- data[, column]

  # computes the average
  avg <- mean(datac)

  # computes the standard deviation
  st <- sd(datac)

  # compose a vector with results
  c(avg, st)
}

#' generates data to include in latex table about
#' asia datasets
generateAsiaData <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/asia-summary.csv")

  # computes average and standard deviation for map
  maptrain <- getStatistics(data, 1)
  maptest <- getStatistics(data, 4)

  # computes average and standard deviation for bma
  bmatrain <- getStatistics(data, 2)
  bmatest <- getStatistics(data, 5)

  # computes average and standard deviation for bnlearn
  bntrain <- getStatistics(data, 7)
  bntest <- getStatistics(data, 8)

  # produces the output as latex text
  cat("map train: ", sprintf("$%.2f %s %.2f$\n", maptrain[1], " \\pm ", maptrain[2]))
  cat("map test: ", sprintf("$%.2f %s %.2f$\n", maptest[1], " \\pm ", maptest[2]))
  cat("bma train: ", sprintf("$%.2f %s %.2f$\n", bmatrain[1], " \\pm ", bmatrain[2]))
  cat("bma test: ", sprintf("$%.2f %s %.2f$\n", bmatest[1], " \\pm ", bmatest[2]))
  cat("bnlearn train: ", sprintf("$%.2f %s %.2f$\n", bntrain[1], " \\pm ", bntrain[2]))
  cat("bnlearn test: ", sprintf("$%.2f %s %.2f$\n", bntest[1], " \\pm ", bntest[2]))
}

#' generates boxplots for asia datasets
generateAsiaBoxplots <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/asia-summary.csv")

  # generate the pdf with boxplot
  pdf("asia-boxplots.pdf")

  # process data for preparing boxplots generation
  datas <- melt(data[,c(1,4,2,5,7,8)])
  colnames(datas) <- c("score", "value")

  # generate plot
  plot <- ggplot2::ggplot(data = melt(datas), aes(x=score, y=value, fill = score)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
    theme(legend.position="none")

  # print the plot
  print(plot)

  # close the dev
  dev.off()
}

#' generates data to include in latex table about
#' asia datasets
generateAlarmData <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/alarm-summary.csv")

  # computes average and standard deviation for map
  maptrain <- getStatistics(data, 1)
  maptest <- getStatistics(data, 4)

  # computes average and standard deviation for bma
  bmatrain <- getStatistics(data, 2)
  bmatest <- getStatistics(data, 5)

  # computes average and standard deviation for bnlearn
  bntrain <- getStatistics(data, 7)
  bntest <- getStatistics(data, 8)

  # produces the output as latex text
  cat("map train: ", sprintf("$%.2f %s %.2f$\n", maptrain[1], " \\pm ", maptrain[2]))
  cat("map test: ", sprintf("$%.2f %s %.2f$\n", maptest[1], " \\pm ", maptest[2]))
  cat("bma train: ", sprintf("$%.2f %s %.2f$\n", bmatrain[1], " \\pm ", bmatrain[2]))
  cat("bma test: ", sprintf("$%.2f %s %.2f$\n", bmatest[1], " \\pm ", bmatest[2]))
  cat("bnlearn train: ", sprintf("$%.2f %s %.2f$\n", bntrain[1], " \\pm ", bntrain[2]))
  cat("bnlearn test: ", sprintf("$%.2f %s %.2f$\n", bntest[1], " \\pm ", bntest[2]))
}

#' generates boxplots for asia datasets
generateAlarmBoxplots <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/alarm-summary.csv")

  # generate the pdf with boxplot
  pdf("alarm-boxplots.pdf")

  # process data for preparing boxplots generation
  datas <- melt(data[,c(1,4,2,5,7,8)])
  colnames(datas) <- c("score", "value")

  # generate plot
  plot <- ggplot2::ggplot(data = melt(datas), aes(x=score, y=value, fill = score)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
    theme(legend.position="none")

  # print the plot
  print(plot)

  # close the dev
  dev.off()
}

#' generates data to include in latex table about
#' asia datasets
generateHepar2Data <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/hepar2-summary.csv")

  # computes average and standard deviation for map
  maptrain <- getStatistics(data, 1)
  maptest <- getStatistics(data, 4)

  # computes average and standard deviation for bma
  bmatrain <- getStatistics(data, 2)
  bmatest <- getStatistics(data, 5)

  # computes average and standard deviation for bnlearn
  bntrain <- getStatistics(data, 7)
  bntest <- getStatistics(data, 8)

  # produces the output as latex text
  cat("map train: ", sprintf("$%.2f %s %.2f$\n", maptrain[1], " \\pm ", maptrain[2]))
  cat("map test: ", sprintf("$%.2f %s %.2f$\n", maptest[1], " \\pm ", maptest[2]))
  cat("bma train: ", sprintf("$%.2f %s %.2f$\n", bmatrain[1], " \\pm ", bmatrain[2]))
  cat("bma test: ", sprintf("$%.2f %s %.2f$\n", bmatest[1], " \\pm ", bmatest[2]))
  cat("bnlearn train: ", sprintf("$%.2f %s %.2f$\n", bntrain[1], " \\pm ", bntrain[2]))
  cat("bnlearn test: ", sprintf("$%.2f %s %.2f$\n", bntest[1], " \\pm ", bntest[2]))
}

#' generates boxplots for asia datasets
generateHepar2Boxplots <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/hepar2-summary.csv")

  # generate the pdf with boxplot
  pdf("hepar2-boxplots.pdf")

  # process data for preparing boxplots generation
  datas <- melt(data[,c(1,4,2,5,7,8)])
  colnames(datas) <- c("score", "value")

  # generate plot
  plot <- ggplot2::ggplot(data = melt(datas), aes(x=score, y=value, fill = score)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
    theme(legend.position="none")

  # print the plot
  print(plot)

  # close the dev
  dev.off()
}

#' generates data to include in latex table about
#' asia datasets
generateWin95ptsData <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/win95pts-summary.csv")

  # computes average and standard deviation for map
  maptrain <- getStatistics(data, 1)
  maptest <- getStatistics(data, 4)

  # computes average and standard deviation for bma
  bmatrain <- getStatistics(data, 2)
  bmatest <- getStatistics(data, 5)

  # computes average and standard deviation for bnlearn
  bntrain <- getStatistics(data, 7)
  bntest <- getStatistics(data, 8)

  # produces the output as latex text
  cat("map train: ", sprintf("$%.2f %s %.2f$\n", maptrain[1], " \\pm ", maptrain[2]))
  cat("map test: ", sprintf("$%.2f %s %.2f$\n", maptest[1], " \\pm ", maptest[2]))
  cat("bma train: ", sprintf("$%.2f %s %.2f$\n", bmatrain[1], " \\pm ", bmatrain[2]))
  cat("bma test: ", sprintf("$%.2f %s %.2f$\n", bmatest[1], " \\pm ", bmatest[2]))
  cat("bnlearn train: ", sprintf("$%.2f %s %.2f$\n", bntrain[1], " \\pm ", bntrain[2]))
  cat("bnlearn test: ", sprintf("$%.2f %s %.2f$\n", bntest[1], " \\pm ", bntest[2]))
}

#' generates boxplots for asia datasets
generateWin95ptsBoxplots <- function(){
  # read data for asia
  data <- readSummaryData("./R/experiment/win95pts-summary.csv")

  # generate the pdf with boxplot
  pdf("win95pts-boxplots.pdf")

  # process data for preparing boxplots generation
  datas <- melt(data[,c(1,4,2,5,7,8)])
  colnames(datas) <- c("score", "value")

  # generate plot
  plot <- ggplot2::ggplot(data = melt(datas), aes(x=score, y=value, fill = score)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#999999", "#999999", "#E69F00", "#E69F00", "#56B4E9", "#56B4E9")) +
    theme(legend.position="none")

  # print the plot
  print(plot)

  # close the dev
  dev.off()
}
