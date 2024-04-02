library(R6)
library(hash)
library(reshape2)
library(dplyr)

# TODO: remove
library(bnlearn)

#' class for computing scores for a given data file
ScoreComputation <- R6Class("ScoreComputation",
  public = list(
    #' @description
    #' class constructor
    #' @param filename name of the file containing the data
    initialize=function(filename){
      # stores filename
      private$filename <- filename

      # reads the data file and store it into data
      private$data <- readDataBase(filename)

      # gets the variables if the dataframe
      private$variables <- colnames(private$data)

      # gets the number of states
      private$states <- sapply(private$data, nlevels)

      # creates the hash for the scores
      private$scores <- ScoreHash$new(private$debug, private$debugInfo)

      # computes the weights
      private$weights <- private$computeWeights()
    },

    #' @description
    #' gets the name of the variable with a given index
    #' @param index index of interest
    #' @return name of the variable
    getVariableName=function(index){
      private$variables[index]
    },

    #' @description
    #' gets the index of a variable given its name
    #' @param name of interest
    #' @return index of the variable
    getVariableIndex=function(name){
      which(private$variables == name)
    },

    #' @description
    #' return the data used for learning
    getData=function(){
      private$data
    },

    #' @description
    #' gets the number of variables
    getNumberVariables=function(){
      # return the number of variables
      length(private$variables)
    },

    #' @description
    #' gets the names of variables
    getVariableNames=function(){
      private$variables
    },

    #' @description
    #' sets the object used for storing debug info
    #' @param debugInfo object to use for storing debug information
    setDebugInfoObject=function(debugInfo){
      private$debugInfo  <- debugInfo

      # set if for hash as well
      private$scores$setDebugInfoObject(private$debugInfo)
    },

    #' @description
    #' computes bnlearn score for comparison reasons
    #' @param indexV index of target variable
    #' @param indexesP indexes of parent variables
    bnlearnScore=function(indexV, indexesP){
      variable <- private$variables[indexV]
      parents <- private$variables[indexesP == 1]
      nodes <- c(variable)
      if(length(parents) != 0){
        nodes <- c(variable, parents)
      }

      # creates an empty graph with the nodes of interest
      graph <- bnlearn::empty.graph(nodes, 1)

      # sets arcs for parents
      if(length(parents) != 0){
        for(i in 1:length(parents)){
          graph <- bnlearn::set.arc(graph, parents[i], variable)
        }
      }

      # limit data to selected variables
      variables <- c(variable, parents)
      dataScore <- as.data.frame(dplyr::select(private$data,
                                               tidyselect::all_of(variables)))

      # compute score and return its value
      score <- bnlearn::score(graph, dataScore, by.node = TRUE,
                              type="bde", iss=1)

      # return score
      indexInBnlearn <- which(bnlearn::nodes(graph) == variable)
      score[indexInBnlearn]
    },

    #' @description
    #' computes bnlearn score for comparison reasons
    #' @param indexV index of target variable
    #' @param parents vector of parent variables
    bnlearnScoreWithParents=function(indexV, parents){
      variable <- private$variables[indexV]
      nodes <- c(variable)
      if(length(parents) != 0){
        nodes <- c(variable, parents)
      }

      # creates an empty graph with the nodes of interest
      graph <- bnlearn::empty.graph(nodes, 1)

      # sets arcs for parents
      if(length(parents) != 0){
        for(i in 1:length(parents)){
          graph <- bnlearn::set.arc(graph, parents[i], variable)
        }
      }

      # limit data to selected variables
      variables <- c(variable, parents)
      dataScore <- as.data.frame(dplyr::select(private$data,
                                               tidyselect::all_of(variables)))

      # compute score and return its value
      score <- bnlearn::score(graph, dataScore, by.node = TRUE,
                              type="bde", iss=1)

      # return score
      score
    },

    #' @description
    #' method for computing the score of a variable given a
    #' certain set of parents
    #' @param indexV index of the variable of interest
    #' @param indexesP indexes of the variables in the set of parents
    #' @return computed score
    score=function(indexV, indexesP){
      # gets the variable of interest
      variable <- private$variables[indexV]

      # just considers those variables with a 1 in the corresponding
      # position
      parents <- private$variables[indexesP == 1]

      # retrieve the score or compute it
      if(private$scores$check(variable, parents) == TRUE){
        # just recover the result
        val=private$scores$get(variable, parents)
      }
      else{
         # gets the number of states of variable
         ri <- private$states[variable]

         # gets the number of configurations for parents
         qi <- private$getNumberConfigurations(parents)

         # gets the data related to the variables
         # data <- private$getDataForVariables(variable, parents)

         # now compute the score
         # val <- private$computeScore(ri, qi, data)
         val <- self$bnlearnScore(indexV, indexesP)

         # gets the number of parent in pi[s,]
         numberParents <- length(parents)

         # adds the penalty term
         # penalty <- log(choose(length(private$variables), numberParents))

         # compose val and penalty
	       # TODO: removed penalty
         # val <- val-penalty

         # stores the score
         private$scores$store(variable, parents, val)
      }

       # shows debug info
      #if(private$debug){
         # private$debugInfo$addEndText("ScoreComputation$score")
      #}

      # return val
      val
    },

    #' @description
    #' method for computing the score of a variable given a
    #' certain set of parents
    #' @param target target variable
    #' @param parents vector of parent variables
    #' @return computed score
    scoreWithParentSet=function(target, parents){

      # convert parents from set to vector for getting access to
      # the hash of scores
      parents <- private$variables[private$variables %in% parents]

      # retrieve the score or compute it
      if(private$scores$check(target, parents) == TRUE){
        # just recover the result
        val=private$scores$get(target, parents)
      }
      else{
        # gets the number of states of variable
        ri <- private$states[target]

        # gets the number of configurations for parents
        qi <- private$getNumberConfigurations(parents)

        # gets the data related to the variables
        data <- private$getDataForVariables(target, parents)

        # now compute the score
        val <- private$computeScore(ri, qi, data)

        # gets the number of parents
        numberParents <- length(parents)

        # adds the penalty term
        # penalty <- log(choose(length(private$variables), numberParents))

        # compose val and penalty
	# TODO: removed penalty
        # val <- val-penalty

        # removes data
        rm(data)

        # stores the score
        private$scores$store(target, parents, val)
      }

      # return val
      val
    },

    #' @description
    #' shows information about the object
    show = function(){
      # compose a string with info
      message <- composeString(c("Info about hash of scores....................\n",
                      private$scores$show(),
                      ".............................................\n"))

      # return the message
      message
    }
  ),

  # private part of the class
  private = list(
    #' file name of the csv data file to analyze
    filename="character",

    #' data frame with the data in filename
    data="data.frame",

    #' variables
    variables="character",

    #' number of states for the variables
    states="integer",

    #' weights of the variables.
    #' TODO: remove it?
    weights="numeric",

    #' scores hash for storing scores
    scores="ScoreHash",

    # debug flag
    debug="logical",

    #' object of class debugInfo
    debugInfo="VSLearningInfo",

    #' method for computing the number of configurations of a certain
    #' set of variables
    getNumberConfigurations=function(variables){
      # gets the indexes of the variables if the data frame variables
      indexes <- match(variables, private$variables)

      # multiply the number of states
      numberConfigurations <- prod(private$states[indexes])
    },

    #' gets the part of the data frame for a given set
    #' of variables (variable + parents)
    getDataForVariables=function(variable, parents){
      # just remove unused variables
      data <- private$data[, c(variable, parents)]
      if(length(parents) != 0){
          data <- as.data.frame(data %>% group_by_all(.drop = FALSE) %>% count)
      }
      else{
          data <- as.data.frame(table(data))
      }

      # adds all the variables into a single vector adding
      # separator for composing a formula. At the end adds
      # "~."
      # formula <- paste(c(variable, parents), collapse="+")
      # formula <- paste0(formula," ~ .")
      # cat("formula :  ", formula, "\n")

      # gets the data: this function produces a table
      # considering all the combinations of the main
      # variable and its parents. Each row (combination)
      # of values will have as last column the counter
      # with the number of occurrences of such combination
      # data <- reshape2::dcast(data, formula,
      #                         value.var = variable,
      #                         drop=FALSE, length)

      # gives name to the last column (counters)
      names(data) <- c(variable, parents, "counters")

      # return data
      data
      #data <- dplyr::select(private$data, c(variable, parents))
    },

    #' computes the score for a particular subset of data
    computeScore=function(ri, qi, data){
      # shows debug info
      # if(private$debug){
      #    private$debugInfo$addStartText("ScoreComputation$computeScore",
      #                                   c("ri", "qi"), c(ri, qi))
      # }

      # initializes nij
      nij <- rep(0, qi)

      # gets the index of the last column in data
      index <- ncol(data)

      # use the equivalent sample size s=1
      s <- 1.0

      # gets the counters of interest
      for(j in 1:qi){
        for(k in 1:ri){
          nij[j]=nij[j]+data[qi*(k-1)+j,index]
        }
      }

      # now compute the values of the score
      sum <- 0
      for(j in 1:qi){
        val1 <- lgamma(s/qi)-lgamma(nij[j]+(s/qi))
        val2 <- 0
        for(k in 1:ri){
          nijk=data[qi*(k-1)+j,index]
          val2jk <- lgamma(nijk + (s/(ri*qi))) - lgamma(s/(ri*qi))

          # shows debug info
          #if(private$debug){
          #  message <- composeString(c("j: ", j, " nij: ", nij[j],
          #                             " k: ", k, " nijk: ", nijk,
          #                             " val1: ", sprintf("%8.4f ", val1),
          #                             " val2jk: ", sprintf("%3.4f ", val2jk)))
          #  private$debugInfo$addText(message)
          #}

          # increments val2
          val2 <- val2 + val2jk
        }

        # compose the final value
        sum <- sum+val1+val2
      }

      # removes nij
      remove(nij)

      # shows debug info
      # if(private$debug){
      #   message <- composeString(c("value to return: ", round(sum,4)))
      #   private$debugInfo$addText(message)
      #   private$debugInfo$addEndText("ScoreComputation$computeScore")
      # }

      # return sum as a single number (not numeric with name)
      sum <- unname(sum)
    },

    #' computes the weights for each variable
    computeWeights=function(){
      # initializes the weights to 1
      private$weights <- rep(1,length(private$variables))

      # compute the weights
      sapply(1:(length(private$variables)-1), function(i) {

        # computes the product
        weight <- prod(private$states[(i+1):length(private$variables)])

        # stores it
        private$weights[i] <- weight
      })
    }
  )
)
