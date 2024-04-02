library(ggplot2)

readHeader <- function(filepath){
  # read the 5 first lines
  data <- read.table(filepath)

  # gets the first lines with the global measures
  bntrain <- data[1,]
  bntest <- data[2,]
  maptest <- data[3,]
  bmatest <- data[4,]
  baytest <- data[5,]

  # return the tuple
  c(bntrain, bntest, maptest, bmatest, baytest)
}

readIterData <- function(filepath){
  # discard the first 5 lines
  df <- read.csv(filepath, skip = 5)

  # sets names to variables
  names(df) <- c("map", "bma", "bay")

  # return the data frame
  df
}

generateGraphic <- function(filepath){
  # gets header data
  base <- as.numeric(readHeader(filepath))

  # gets the rest of the data
  df <- readIterData(filepath)

  # sets limits for axis according to bound values
  ymin = min(base[1], df$map)
  ymax = max(base[1], df$map)

  # compose the graph
  pdf("kk.pdf")
  graph <- ggplot2::ggplot(data = df) +
    scale_colour_manual(name = "", values  = c("darkred", "darkblue", "green", "brown")) +
    xlim(1, length(df$map)) +
    ylim(ymin, ymax) +
    xlab("Iterations") +
    ylab("Scores") +
    geom_smooth(aes(x = 1:length(map), y = map, colour = "map"), size = 1.2) +
    # geom_line(aes(x = 1:length(map), y = bma, colour = "bma"), size = 1.2) +
    # geom_line(aes(x = 1:length(map), y = bay, colour = "bay"), size = 1.2) +
    geom_hline(aes(yintercept = base[1], colour = "bnlearn"),
               size = 1.2, linetype = "dashed") +
    theme(legend.position = c(0.1,0.95))
  # prints the graph
  print(graph)

  # close the device
  dev.off()

}
