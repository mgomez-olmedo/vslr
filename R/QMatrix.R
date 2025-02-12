library(R6)

#' class for storing the values required for learning
#' @param name class name
#' @param public attributes
#' @param private attributes
#' @importFrom R6 R6Class
QMatrix <- R6Class("QMatrix",
  public=list(
    #' @description
    #' method to create a new object
    #' @param n number of variables
    #' @param names names of variables
    #' @param debug debug flag
    initialize=function(n, names, debug=FALSE){
      # initializes data member
      private$dimension <- n

      # initializes debug flag
      private$debug <- debug

      # stores the names of the variables
      private$variables <- names

      # creates the matrix of triplets
      private$matrix <- private$createTripletsMatrix()
    },

    #' @description
    #' copies the values from the matrix passed as argument
    #' @param toCopy matrix to be copied
    copy=function(toCopy){
      # copy the values from matrix
      private$dimension <- toCopy$getDimension()
      private$debug <- toCopy$getDebug()
      private$matrix <- rlang::duplicate(toCopy$matrix)
    },

    #' @description
    #' gets the probability for the presence of a link from
    #' a variable to another
    #' @param from head of link
    #' @param to tail of link
    #' @return probability value
    getProbFromTo=function(from, to){
      # gets the index of from and to
      indexFrom = which(private$variables == from)
      indexTo = which(private$variables == to)

      # gets the required triplet
      triplet <- self$getValuesFromIndices(indexFrom, indexTo)

      # makes access with order
      if(indexFrom < indexTo){
        # gets the prob with right direction to the query
        prob <- triplet[3]
      }
      else{
        # gets the prob from negative component (indexFrom <- indexTo)
        prob <- triplet[1]
      }

      # return prob
      prob
    },

    #' @description
    #' gets the list of parents analyzing the dominance
    #' matrix formed from probabilities
    generateParentsList = function() {
      # gets the matrix of dominance
      dominanceMatrix <- private$getDominanceMatrix()

      # determine the parents of each variable
      parents <- list()

      for (i in 1:private$dimension) {
        indCol <- (1:private$dimension)
        indCol <- indCol[indCol > i]

        # gets the name of the variable used as Xi
        Xi <- private$variables[i]

        # considers position above main diagonal
        for (j in indCol) {
          # gets the name of the variable used as Xj
          Xj <- private$variables[j]

          # gets the symbol in the dominance matrix
          relation <- dominanceMatrix[i, j]

          # considers the relation
          if (relation == '+') {
            # Xi is parent of Xj
            previousParents <- parents[[Xj]]
            if (length(previousParents) == 0) {
              parents[[Xj]] <- Xi
            }
            else{
              # concatenates with previous parents
              parents[[Xj]] <- c(parents[[Xj]], Xi)
            }
          }
          else{
            if (relation == "-") {
              # Xj is parent of Xi
              previousParents <- parents[[Xi]]
              if (length(previousParents) == 0) {
                parents[[Xi]] <- Xj
              }
              else{
                # concatenates with previous parents
                parents[[Xi]] <- c(parents[[Xi]], Xj)
              }
            }
          }
        }
      }

      # return the list of parents
      parents
    },

    #' @description
    #' composes the adjacency matrix
    getAdjacencyMatrix=function(){
      # creates the matrix to return
      amat <- matrix(0, nrow=private$dimension,
                     ncol=private$dimension)

      # fill the matrices according to the values in the
      # matrices
      columns <- c(1:private$dimension)
      for(i in 1:private$dimension){
        indices <- columns[columns > i]
        for(j in indices){
          # gets the values from negative, zero and positive
          triplet <- self$getValuesFromIndices(i,j)

          # gets the index of the max value
          indMax <- which(max(triplet) == triplet)[1]

          # translate into symbol
          if(indMax == 1){
            # link from Xj to Xi
            amat[j,i] <- 1
          }
          else{
            if(indMax == 3){
              # link from Xi to Xj
              amat[i,j] <- 1
            }
          }
        }
      }

      # return adjacency matrix
      amat
    },

    #' @description
    #' gets debug flag
    getDebug = function(){
      private$debug
    },

    #' @description
    #' gets the dimension of the matrices
    getDimension=function(){
      private$dimension
    },

    #' @description
    #' gets the probability for the absence of a link from
    #' a variable to another
    #' @param first variable of pair
    #' @param second second variable of pair
    #' @return probability value
    getIndependence=function(first, second){
      # gets the index from variable names
      indexFirst = which(private$variables == first)
      indexSecond = which(private$variables == second)

      # gets the values for these indices
      triplet <- self$getValuesFromIndices(indexFirst, indexSecond)

      # just return the second value
      triplet[2]
    },

    #' @description
    #' gets the state corresponding to a given position and
    #' a certain random number
    #' @param i row of the position of interest
    #' @param j column of the position of interest
    #' @param random random value to get the state corresponding
    #' to this random number
    #' @return selected state (-1, 0, 1)
    getState=function(i, j, random){
      # gets the corresponding triplet
      triplet <- self$getValuesFromIndices(i,j)

      sum <- 0
      val <- 0
      # if the random number is less than the probability
      # for negative state, return -1
      if(random < triplet[1]){
        val <- -1
      }
      else{
        # accumulates the values for negative and zero
        # states. If the random number is over the sum,
        # return 1. In any other case, return 0
        sum <- triplet[1]+triplet[2]
        if(random > sum){
          val <- 1
        }
      }

      # return val
      val
    },

    #' @description
    #' gets the values for a given a position given by
    #' variable names
    #' @param source name of the first variable
    #' @param dest name of the second variable
    #' @return corresponding triplet
    getValuesFromNames=function(source, dest){
      # gets the index due to i and j
      index <- private$getIndexFromVariableNames(source, dest)

      # just access to the corresponding element
      private$matrix[[index]]
    },

    #' @description
    #' gets the values for a given a position
    #' @param i row
    #' @param j column
    #' @return vector of negative, zero and positive values
    getValuesFromIndices = function(i,j){
      # gets the index due to i and j
      index <- private$getIndexFromVariableIndices(i,j)

      # just access to the corresponding element
      private$matrix[[index]]
    },

    #' @description
    #' gets the variables of the matrix
    getVariables = function(){
      private$variables
    },

    #' @description
    #' initializes matrices with an adjacencies matrix
    #' @param matrix an adjacency matrix obtained from bnlearn
    modelInitialization = function(matrix){
      # gets the number of nodes
      nnodes <- dim(matrix)[1]

      # considers all the values of the matrix
      for(i in 1:nnodes){
        for(j in 1:nnodes){
          # acts on 1 values
          if(matrix[i,j] == 1){
            # sets the values
            if(i < j){
              # assigns 0.8 to positive position
              self$setValue(i, j, c(0.1, 0.1, 0.8))
            }
            else{
              # assigns 0.8 to negative position
              self$setValue(j, i, c(0.8, 0.1, 0.1))
            }
          }
          else{
            if(i < j){
              # assigns 0.8 to independence position
              self$setValue(i, j, c(0.1, 0.8, 0.1))
            }
          }
        }
      }
    },

    #' @description
    #' method for getting a new set of probability values
    #' compute from a score matrix
    #' @param scoreMatrix matrix used for setting new
    #' probability values
    normalizeWithScores=function(scoreMatrix){
      # transform the score matrix applying exponential and
      # substracting the maximum value
      # gets max values for each position
      maxValues <- private$getMaxValues(private$matrix)

      # substract each max value of the corresponding triplet
      substracted <- private$substract(private$matrix, maxValues)

      # computes exponential on substracted
      exponential <- private$exponential(substracted)

      # gets the sums for all the triples
      sumMatrix <- private$getSumms(exponential)

      # changes the values of triplets stored into
      # exponential by the summs in order to normalize
      private$matrix <- private$divide(exponential, sumMatrix)
    },

    #' @description
    #' method for producing a random initialization
    randomInitialization=function(){
      # gets random numbers for the probabilities of each
      # alternative: ->, ind, <-
      for(i in 1:length(private$matrix)){
        private$matrix[[i]] <- runif(3)
      }

      # normalize
      private$normalize()
    },

    #' @description
    #' function for saving the data of the matrix in RDS format
    #' @param filename name of the file to use
    save=function(filename){
      # gets file without path and extension
      base <- basename(filename)
      base <- gsub(pattern = "\\.$*", "", base)

      # add Rres to the extension
      base <- paste0(base,".Rres")

      # adds the path to data/res
      pathBase <- paste0("./data/res/",base)
      saveRDS(self, file=pathBase)
    },

    #' @description
    #' sets the value corresponding to a given position and
    #' @param i index of the row
    #' @param j index of the col
    #' @param values vector of values (negative, zero, positive)
    #' @param normalize boolean flag (TRUE: the values must be
    #'        normalized before storing them)
    setValue=function(i, j, values, normalize=FALSE){
      # gets the index of the triplet from i and j
      index <- private$getIndexFromVariableIndices(i,j)

      if (!normalize){
        private$matrix[[index]] <- values
      }
      else{
        # get the maximum and minimum values
        maxVal <- max(values)
        # minVal <- min(values)

        # computes the difference of each value respect
        # to min value
        # difValues <- (values - minVal)/(maxVal - minVal)

        # translate difValues to 0-5 and gets exp value
        # difnew <-  difValues*10

        # makes exponentiation
        # expValues <- exp(difnew)
        expValues <- exp(values - maxVal)

        # gets the sum
        sumVal <- sum(expValues)

        # normalizes
        normValues <- expValues/sumVal

        # stores the resultant values
        private$matrix[[index]] <- normValues
      }
    },

    #' @description
    #' method for producing a string with the content of the
    #' matrix
    show=function(){
      # initialize output string
      output <- ""

      # gets the indices for columns and rows
      columns <- c(1:length(private$variables))

      # considers each row
      for(i in 1:length(private$variables)){

        # for each row gets valid columns (over main diagonal)
        indices <- columns[columns > i]

        # consider each valid column
        for(j in indices){
          index <- private$getIndexFromVariableIndices(i,j)
          values <- private$matrix[[index]]
          output <- paste0(output, sprintf("%s - %s ( %1.4f %1.4f %1.4f ) \n",
                                           private$variables[i], private$variables[j],
                                           values[1], values[2], values[3]))
        }
      }

      # finally return output string
      output
    },


    #' @description
    #' method producing a initialization with uniform values
    uniformInitialization=function(){
      # sets 0.3333 for the probabilities of each
      # alternative: ->, ind, <-
      for(i in 1:length(private$matrix)){
        private$matrix[[i]] <- c(0.3333, 0.3333, 0.3333)
      }
    }
  ),

  private=list(
    #' dimension number of rows and cols
    dimension="numeric",

    #' debug debug flag
    debug="boolean",

    #' variables names of variables
    variables="character",

    #' matrix of triplets
    matrix="list",

    #' gets the matrix with -1, 0, 1 according to the
    #' probability values
    getDominanceMatrix=function(){
      # creates the matrix to return
      dominance <- matrix(0, nrow=private$dimension,
                          ncol=private$dimension)

      # fill the matrices according to the values in the
      # matrices
      columns <- c(1:private$dimension)
      for(i in 1:private$dimension){
        indices <- columns[columns > i]
        for(j in indices){
          # gets the values from negative, zero and positive
          valNeg <- private$negative[i,j]
          valZero <- private$zero[i,j]
          valPos <- private$positive[i,j]
          vals <- c(valNeg, valZero, valPos)

          # gets the index of the max value
          indMax <- which(max(vals) == vals)[1]

          # translate into symbol
          symbol <- '+'
          if(indMax == 1){
            symbol <- '-'
          }
          else{
            if(indMax == 2){
              symbol='0'
            }
          }

          # writes the symbol into dominance
          dominance[i,j] <- symbol
        }
      }

      # return dominance matrix
      dominance
    },

    #' creates a list of triplets with the
    #' required dimension for the problem
    createTripletsMatrix = function(){
      matrix <- list()
      counter = 1
      for(i in 1:(private$dimension-1)){
        for(j in (i+1):private$dimension){
          matrix[[counter]] <- c(0,0,0)
          counter <- counter+1
        }
      }

      # return the matrix
      matrix
    },

    #' creates a list of values with the
    #' required dimension for the problem
    createValuesMatrix = function(){
      matrix <- list()
      counter = 1
      for(i in 1:(private$dimension-1)){
        for(j in (i+1):private$dimension){
          matrix[[counter]] <- 0
          counter <- counter+1
        }
      }

      # return the matrix
      matrix
    },

    #' gets the max value for all the triplets
    #' param triplets - matrix of triplets to consider
    getMaxValues = function(triplets){
      # creates a matrix of values
      maxMat <- private$createValuesMatrix()

      # loop over matrix triplets
      for(i in 1:length(triplets)){
        # gets the corresponding triplet
        triplet <- triplets[[i]]
        maxMat[i] <- max(triplet)
      }

      # return maxMat
      maxMat
    },

    #' gets the sum for all the triplets
    #' param triplets - matrix of triplets to consider
    getSumms = function(triplets){
      # creates a matrix of values
      summs <- private$createValuesMatrix()

      # loop over matrix triplets
      for(i in 1:length(triplets)){
        # gets the corresponding triplet
        triplet <- triplets[[i]]
        summs[i] <- sum(triplet)
      }

      # return summs
      summs
    },

    #' param triplets - triplet matrix of triplets
    #' param values - list of values to substract
    #' return - the matrix of new triplets
    substract = function(triplets, values){
      # create the result matrix
      result <- private$createTripletsMatrix()

      # makes the substractions
      for(i in 1:length(values)){
        result[[i]] <- triplets[[i]] - values[i]
      }

      # return the final result
      result
    },

    #' param - triplet matrix of triplets
    #' param - values list of values to bivide by
    #' return - the matrix of new triplets
    divide = function(triplets, values){
      # create the result matrix
      result <- private$createTripletsMatrix()

      # makes the substractions
      for(i in 1:length(values)){
        result[[i]] <- triplets[[i]]/values[[i]]
      }

      # return the final result
      result
    },

    #' apply exponential to each triplet of the matrix
    #' passed as argument
    #' return - the matrix of new triplets
    exponential = function(triplets){
      # create the result matrix
      result <- private$createTripletsMatrix()

      # makes the exponentiation operation
      for(i in 1:length(values)){
        result[[i]] <- exp(triplets[[i]])
      }

      # return the final result
      result
    },

    #' computes the index for a given pair
    #' of variable names
    #' return - required index
    getIndexFromVariableNames = function(source ,dest){
      # gets the index of from and to
      indexFrom = which(private$variables == source)
      indexTo = which(private$variables == dest)

      # computes and return the index of the triplet
      # into matrix
      private$getIndexFromVariableIndices(indexFrom, indexTo)
    },

    #' computes the index of the triplet related
    #' to i row and j column into matrix
    #' return - required index
    getIndexFromVariableIndices = function(i, j){
      # order indexes
      row = min(i, j)
      col = max(i, j)

      # determine the number of elements for the row.
      # This number will be used to compute the inferior
      # limit of the summation of elements below the row
      inf = private$dimension - row + 1

      # the superior number is always the dimension of the
      # matrix minus one
      sup = private$dimension - 1

      # computes the offset until the first element for
      # the row
      desp = ((sup*(sup+1)) - (inf*(inf-1)))/2

      # now adds the offset due to the column
      desp <- desp + (col - row)

      # return the desp
      desp
    },

    #' method for normalizing the values
    normalize=function(){
      # Gets the matrices addition
      sumMatrix <- private$getSumms(private$matrix)

      # divides all the triplets by the corresponding sum
      private$matrix <- private$divide(private$matrix, sumMatrix)
    }
  )
)

