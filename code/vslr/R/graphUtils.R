library(igraph)
library(bnlearn)

#' creates a string describing the structure of the net
#' example ([A|B:C:F][B][C][D][E][F])
#' @param variables lits of variables involved
#' @param parents list of parents for each variable
createBnlearnString <- function(variables, parents) {
  # initiaizes description to ""
  description <- ""

  # considers each variable
  numberVariables <- length(variables)
  for (i in 1:numberVariables) {
    X <- variables[i]
    variableDescription <- paste0("[", X)

    # considers its parents
    xParents <- parents[[X]]

    # adds the description
    if (length(xParents) == 0) {
      variableDescription <- paste0(variableDescription, "]")
    }
    else{
      # add | to the descripcion
      variableDescription <- paste0(variableDescription, "|")

      # adds the description of parents
      variableDescription <- paste0(variableDescription,
                                    paste(parents[[X]], collapse = ":"))

      # adds ]
      variableDescription <- paste0(variableDescription, "]")
    }

    # adds the variable description to the global description
    description <- paste0(description, variableDescription)
  }

  # return the description
  description
}

#' creates a test matrix for testing purposes
#' @param names variable names for printing the
#' graph
createTestMatrix <- function(names){
  # creates a specific qmatrix
  mat <- QMatrix$new(5, names)

  # sets the values
  mat$setValue(1, 2, c(0.75, 0, 0.25))
  mat$setValue(1, 3, c(0.15, 0.85, 0))
  mat$setValue(1, 4, c(0.01, 0.98, 0.01))
  mat$setValue(1, 5, c(0, 1, 0))
  mat$setValue(2, 3, c(0.15, 0.05, 0.8))
  mat$setValue(2, 4, c(0, 1, 0))
  mat$setValue(2, 5, c(0, 1, 0))
  mat$setValue(3, 4, c(0.6, 0.2, 0.2))
  mat$setValue(3, 5, c(0.1, 0.1, 0.8))
  mat$setValue(4, 5, c(0, 1, 0))

  # return mat
  mat
}

#' generates a graph from a net with bnlearn format
#' @param bnet object of bnet class
generateGraphFromBnet <- function(bnet){
  # gets the adjacency matrix from bnet
  netMat <- bnlearn::amat(bnet)

  # generate a graph with this matrix
  rownames(netMat) <- bnlearn::nodes(bnet)
  colnames(netMat) <- bnlearn::nodes(bnet)
  g <- igraph::graph_from_adjacency_matrix(netMat, weighted=TRUE,
                                           diag=FALSE)
}

#' generates a graph from an adjacency matrix
#' @param amat adjacency matrix
#' @param variables vector of variables
generateGraphFromMatrix <- function(amat, variables){
  # generate a graph with this matrix
  rownames(amat) <- variables
  colnames(amat) <- variables
  g <- igraph::graph_from_adjacency_matrix(amat, weighted=TRUE,
                                           diag=FALSE)
}

#' generates the predominant graph reading the highest values from
#' the matrix of probabilities passed as argument
#' @param probMatrix matrix with probabilities
generatePredominantGraph <- function(probMatrix){
  # gets the matrix with max values
  adjMatrix <- probMatrix$getAdjacencyMatrix()

  # gets the graph for this net
  generateGraphFromMatrix(adjMatrix, probMatrix$getVariables())
}

#' generates the predominant graph reading the highest values from
#' the matrix of probabilities passed as argument
#' @param probMatrix matrix with probabilities
generatePredominantGraphWithBnlearn <- function(probMatrix){
  # gets the parents
  parents <- probMatrix$generateParentsList()

  # generate a bnlearn string with the description of the net
  bnetString <- createBnlearnString(probMatrix$getVariables(),
                                    parents)

  # creates an empty graph and sets the description to the
  # structure
  net <- bnlearn::empty.graph(probMatrix$getVariables())
  bnlearn::modelstring(net, debug=FALSE) <- bnetString

  # gets the graph for this net
  generateGraphFromBnet(net)
}

#' generates a probability graph
#' @param probMatrix matrix with the probabilistic matrix
#' generated with the learning algorithm
#' @param threshold min value of prob for showing an edge
generateProbGraph <- function(probMatrix, threshold){
  # gets the dimensions of the matrix
  n <- probMatrix$getDimension()

  # to generate a graph considers positive values for the
  # upper matrix and negative values for lower matrix
  amat <- matrix(ncol=n, nrow=n, 0)
  cat("generate prob graph ...... ")

  # get weight values
  for(i in 1:n){
    for(j in 1:n){
      if(j > i){
        # stores in i,j the value in positive matrix
        cat("triplet for ",i, " -> ", j, " ")
        print(probMatrix$getValuesFromIndices(i,j))
        probPos <- probMatrix$getValuesFromIndices(i,j)[3]
        probNeg <- probMatrix$getValuesFromIndices(i,j)[1]
        if(probPos > threshold){
          amat[i,j] <- probPos
        }

        # stores in j,i the value in negative matrix
        if(probNeg > threshold){
          amat[j,i] <- probNeg
        }
      }
    }
  }
  amat <- round(amat, digits = 2)

  # the values of the matrix are converted to a vector. In order
  # to make the conversion by row we use the transpose of amat
  matVals <- as.vector(t(amat))
  matValsNoZeros <- matVals[which(matVals != 0)]

  # generate a graph with this matrix
  rownames(amat) <- probMatrix$getVariables()
  colnames(amat) <- probMatrix$getVariables()
  g <- igraph::graph_from_adjacency_matrix(amat, weighted=TRUE,
                                           diag=FALSE)

  # sets the weights of the edges
  igraph::E(g)$width <- 1+igraph::E(g)$weight*5

  # sets the labels of the edges
  igraph::E(g)$label <- matValsNoZeros

  # return g
  g
}

#' general method for generating a graph with the
#' real net
#' @param name name of the bnet to graph
generateRealNetGraph <- function(name){
  # read the real net
  net <- readBnet(name)

  # gets the graph for this net
  generateGraphFromBnet(net)
}

#' plot in a single graph the predominant model and the probabilistic
#' one
#' @param name name of the network of interest
#' @param probModel probabilistic matrix generated with
#' the learning algorithm
#' @param threshold probability threshold; only values over
#' threshold will be printed
plotEvolutionNets <- function(name, probModel, threshold){

  # first at all read the file with the coordinates
  # of the model
  positions <- readPositions(name)

  # generate the predominant graph
  predG <- generatePredominantGraph(probModel)

  # generate the graph for the probabilistic model
  probG <- generateProbGraph(probModel, threshold)

  # generate a general panel for the graphs
  graphics::par(cex=0.7, mai=c(0.001, 0.001, 0.001, 0.001))
  graphics::par(mfrow=c(1,2))

  # sets the plot for the real graph
  #par(fig=c(0, 0.49, 0, 1))
  graphics::par(fig=c(0, 0.5, 0, 1))
  plot(predG, vertex.size=40, edge.arrow.size=0.8, edge.curved=.2,
       layout=positions, edge.color="darkblue", vertex.color = "yellow")
  #par(fig=c(0.51, 1, 0, 1), new = TRUE)
  graphics::par(fig=c(0.5, 1, 0, 1), new = TRUE)
  plot(probG, vertex.size=40, edge.arrow.size=0.8, edge.curved=.5,
       layout=positions, edge.color="lightblue", edge.label.color="darkblue")

  # reset par configuration
  graphics::par(mfrow=c(1,1))
}

#' plot in a single graph the real model, the predominant
#' model and the probabilistic and complete model
#' @param name name of the network of interest
#' @param bnlearnModel model  generated with the initialization
#' process
#' @param probModel probabilistic matrix generated with
#' the learning algorithm
#' @param threshold probability threshold; only values over
#' threshold will be printed
#' @param dataPath path for data folder
plotNets <- function(name, bnlearnModel, probModel, threshold, dataPath){
  cat("executing plot nets funcion\n")
  # first at all read the file with the coordinates
  # of the model
  positions <- readPositions(name, dataPath)
  cat("obtained positions of nodes\n")

  # generate the real net graph
  realG <- generateRealNetGraph(name)
  cat("obtained real net graph\n")

  # generate the model generated from bnlearn
  bnetG <- generateGraphFromBnet(bnlearnModel)
  cat("obtained bnlearn model\n")

  # generate the predominant graph
  predG <- generatePredominantGraph(probModel)
  cat("obtained predominan graph\n")

  # generate the graph for the probabilistic model
  probG <- generateProbGraph(probModel, threshold)
  cat("obtained probability graph\n")

  # generate a general panel for the graphs
  graphics::par(cex.main=1.5, mai=c(0.2, 0.2, 0.2, 0.2))
  graphics::par(mfrow=c(2,2))

  # sets the plot for the real graph
  #par(fig=c(0, 0.49, 0.51, 1))
  graphics::par(fig=c(0, 0.50, 0.50, 1))
  plot(realG, vertex.size=60, edge.arrow.size=1.2, edge.curved=.0,
       layout=positions, edge.color="black", vertex.color = "lightblue",
       main = "true network", vertex.label.cex = 1.5,
       vertex.label.font = 2)

  #par(fig=c(0.51, 1, 0.51, 1), new = TRUE)
  graphics::par(fig=c(0.5, 1, 0.5, 1), new = TRUE)
  plot(bnetG, vertex.size=60, edge.arrow.size=1.2, edge.curved=0,
       layout=positions, edge.color="darkblue", vertex.color = "lightblue",
       main = "bnlearn hc network", vertex.label.cex = 1.5,
       vertex.label.font = 2)

  #par(fig=c(0, 0.49, 0, 0.49), new=TRUE)
  #par(fig=c(0, 0.50, 0, 0.50), new=TRUE)
  #plot(predG, vertex.size=40, edge.arrow.size=0.8, edge.curved=.2,
  #     layout=positions, edge.color="darkblue", vertex.color = "yellow")

  #par(fig=c(0.51, 1, 0, 0.49), new=TRUE)
  # par(fig=c(0.50, 1, 0, 0.5), new=TRUE)
  graphics::par(fig=c(0.25, 0.75, 0, 0.5), new=TRUE)
  plot(probG, vertex.size=60, edge.arrow.size=1.2, edge.curved=.5,
       layout=positions, edge.color="lightblue", edge.label.color="darkred",
       main = "vsl network", vertex.label.cex = 1.5,
       edge.label.cex = 1.5, edge.label.font = 2, vertex.label.font = 2)
  graphics::par(mfrow=c(1,1))
}

#' reads a bnet description from a file
#' @param name name of the file to read
readBnet <- function(name){
  netfile <- composeString(c("./data/bnet/", name, ".net"))
  net <- bnlearn::read.net(netfile)
}

#' read the positions of the nodes of a graph
#' @param name name of the file with positions
#' @param path where file with positions is stored
readPositions <- function(name, path = "./data/bnet/"){
  # TODO: change path for normal use: only ./data/bnet
  # for vignettes use ../data/bnet/...
  filename <- composeString(c(path, name, ".pos"))
  cat("filename in read positions: ", filename, "\n")
  positions <- utils::read.csv(filename, header = FALSE)
  matpos <- as.matrix(positions)
}
