library(R6)
library(knitr)
library(ggplot2)
library(bnlearn)
library(sets)
library(matrixStats)
library(doParallel)
library(parallel)

#' Class for variational structure learning
#' @param name class name
#' @param public attributes
#' @param private attributes
#' @importFrom R6 R6Class
#' @export
VSLearning <- R6Class("VSLearning",
  public = list(
    #' @description
    #' class constructor
    #' @param filenameTrain name of the file with the train data
    #' @param filenameTest name of the file with the test data
    #' @param initializationMethod method to employ for
    #' initializing probability matrices
    #' possible values: 1 (random), 2 (uniform), 3 (pc algorithm),
    #' 4 (hill climbing), 5 (tabu search)
    #' @param nParentSets number of parents to sample
    #' @param maxIter maximum number of iterations of the
    #' cycle of improvement
    #' @param dataPath path to net files
    initialize = function(filenameTrain,
                          filenameTest,
                          debug = FALSE,
                          initializationMethod,
                          nParentSets,
                          maxIter = 1,
                          dataPath = "./data/net/") {
      # sets data members
      private$filenameTrain <- filenameTrain
      private$filenameTest <- filenameTest
      private$debug <- debug
      private$initializationMethod <- initializationMethod
      private$maxIter <- maxIter
      private$nParentSets <- nParentSets
      cat("\nnumber of parent sets to sample: ", nParentSets, "\n")
      private$dataPath <- dataPath

      # creates the scorer passing as arguments the name of the
      # train file and debugFlag. The last argument is
      # VSLearningInfo object, initially null
      private$scorer <- ScoreComputation$new(filenameTrain)

      # creates the scorer for making computations on the test
      #dataset
      private$scorerTest <- ScoreComputation$new(filenameTest)

      # gives value to ratio
      private$ratio <-
        nrow(private$scorer$getData())/nrow(private$scorerTest$getData())

      # gets the names of variables
      private$variables <- private$scorer$getVariableNames()

      # gets the number of variables
      private$numberVariables <- length(private$variables)

      # if the number of max. iterations is not defined as parameter,
      # computes its value for considering 5 times each parameter
      if(private$maxIter == 1){
        private$maxIter  <- (private$numberVariables * (private$numberVariables-1))/2 * 5
        cat("number of iterations: ", private$maxIter, "\n")
      }

      # initializes the current iteration counter
      private$currentIteration <- 1

      # PARALLEL
      private$ncores = parallel::detectCores()/2

      # creates the object responsible for storing debug and
      # evolution info
      private$evolInfo <- EvolutionInfo$new(filenameTrain,
                                                initializationMethod,
                                                private$maxIter, nParentSets,
                                                private$variables)

      # initialization of probQ matrix
      private$initializeMatrices()

      # computes the initial lower bound: the param shows that
      # computed values must be used for updating scores
      if(private$initializationMethod == 3 ||
         private$initializationMethod == 4 ||
         private$initializationMethod == 5){
        private$computeBounds(bnlearn = TRUE, test = FALSE, update = TRUE)
      }
      else{
        private$computeBounds(bnlearn = FALSE, test = FALSE, update = TRUE)
      }
    },

    #' @description
    #'  main method for learning
    learn = function() {
      # compose a vector with all the pairs of variables to
      # consider
      pairs <- utils::combn(1:private$numberVariables, 2, simplify = FALSE)

      # sets pair to the first pair of variables
      pairIndex <- 1

      # increments iteration counter (1 is starting point with
      # initialization and 2 is the first one)
      private$currentIteration <- private$currentIteration + 1

      # loop of optimization (adds 1 to the limit because the
      # initial iteration is 1)
      while (private$currentIteration <= private$maxIter+1){
        # selects the pair
        pair <- pairs[[pairIndex]]
        private$updateQValue(pair[1], pair[2])

        # computes the lower bound for this iteration (regular
        # computation; this updates current bound and best bound)
        bounds <- private$updateScore(pair[1], pair[2])

        # update values and store evolution info for the model
        # measures are: entropy - normal - averaging and bayesian
        private$updateBounds(bounds[1], bounds[2], bounds[3], bounds[4])
        private$finalMapTrain <- bounds[1] + bounds[2]
        private$finalBmaTrain <- bounds[3]
        private$finalBayTrain <- bounds[4]

        # prepare the next iteration
        pairIndex <- pairIndex+1
        if (pairIndex > length(pairs)){
          pairIndex <- 1
        }

        # increment the number of iterations
        private$currentIteration <- private$currentIteration + 1
      }

      # computes the bounds with respect to the test data set
      # at this point it is not needed to update bounds
      bounds <- private$computeBounds(bnlearn = FALSE, test = TRUE, update = FALSE)
      private$finalMapTest <- bounds[1] + bounds[2]
      private$finalBmaTest <- bounds[3]
      private$finalBayTest <- bounds[4]
      cat("final measures on test dataset: \n")
      cat("entropy: ", bounds[1], "\n");
      cat("sum scores: ", bounds[2], "\n");
      cat("map: ", private$finalMapTest, "\n");
      cat("bma: ", private$finalBmaTest, "\n");
      cat("bay: ", private$finalBayTest, "\n");

      # generates the file with the data of the process
      private$evolInfo$generateBoundsFile(private$maxIter,
                                         private$nParentSets,
                                         private$initialBnlearnScore,
                                         private$initialBnlearnScoreTest,
                                         private$finalMapTest,
                                         private$finalBmaTest,
                                         private$finalBayTest)

      # generate graphic for MAP measure
      private$evolInfo$generateMapsGraphic(private$currentIteration,
                                             private$initialBnlearnScore,
                                             private$initialBnlearnScoreTest,
                                             private$finalMapTest)


      # return the measures grouped by train-test pairs
      c(private$finalMapTrain, private$finalMapTest,
        private$finalBmaTrain, private$finalBmaTest,
        private$finalBayTrain, private$finalBayTest)
    },

    #' gets the probabilistic model computed by the method
    #' @return probQ object
    getModel = function(){
      private$probQ
    },

    #' gets the relevant measures about the execution of the
    #' algorithm
    #' @return vector with relevant measures
    getMeasures = function(){
      # return a vector with all the important measures
      # the measures are: bnlearn score on train, bnlearn score
      # on test, regular score, averaging and bayesian
      measures <- c(private$initialBnlearnScore,
                    private$initialBnlearnScoreTest,
                    private$finalTrain, private$finalTest,
                    private$finalAveraging, private$finalBayesian)
    },

    #' returns bnlearn scores on training and test datasets
    getBnlearnScores = function(){
      c(private$initialBnlearnScore, private$initialBnlearnScoreTest)
    }
  ),

  #'#################################################################
  #' private part of the class
  #'#################################################################

  #' private part of the class
  private = list(
    # @field filenameTrain filename with the train data
    filenameTrain = "character",

    # @field filenameTest filename with the test data
    filenameTest = "character",

    # @field debug flag for showing debug information during
    # the process
    debug = "boolean",

    # @field ratio ratio between train and test data
    ratio = "numeric",

    # @field path for data
    dataPath = "character",

    # @field initializationMethod identifier of the
    #' initialization method
    initializationMethod = "numeric",

    # @field maxIter maximun number of iterations
    maxIter = "numeric",

    # @field ncores number of cores for parallel operations
    ncores = "numeric",

    # @field nParentSets number of parents to sample
    nParentSets = "numeric",

    # @field level required level of parallelism
    level = "numeric",

    # @field numberVariables number of variables
    numberVariables = "numeric",

    # @field variables names of variables
    variables = "character",

    # @field logQ matrix with logarithms
    logQ = "QMatrix",

    # @field  matrix with probabilities
    probQ = "QMatrix",

    # @field scorer object for making scores computation
    scorer = "ScoreComputation",

    # @field scorer object for making scores computation on
    # the test dataset
    scorerTest = "ScoreComputation",

    # @field matrices of probabilities
    neg = "matrix",
    ind = "matrix",
    pos = "matrix",

    # @field currentIteration counter for the current iteration
    currentIteration = "numeric",

    # @field currentMAP bound of current iteration
    currentMAP = "numeric",

    # @field currentBMA averaging bound of current iteration
    currentBMA = "numeric",

    # @field currentBAY Bayesian bount of current interation
    currentBAY = "numeric",

    # @field testBound final bound on test data
    testBound = "numeric",

    # @field testAveragingBound averaging bound on test data
    testAveragingBound = "numeric",

    # @field testBayesianBound Bayesian bound on test data
    testBayesianBound = "numeric",

    # @field bnlearnModel data member for storing the initial model
    #' produced by bnlearn
    bnlearnModel = "bnlearn::bn",

    # @field evolInfo object for storing debung info if required
    evolInfo = "EvolutionInfo",

    # @field initialBnlearnScore initial value for bnlearn score (when
    #' the model is initialized with a bnlearn method) using train data
    initialBnlearnScore = "numeric",

    # @field initialBnlearnScoreTest initial value for bnlearn score (when
    #' the model is initialized with a bnlearn method) using test data
    initialBnlearnScoreTest = "numeric",

    # @field finalMapTrain final regular score on train dataset
    finalMapTrain = "numeric",

    # @field finalTest final regular score on test dataset
    finalMapTest = "numeric",

    # @field finalBmaTrain final averaging score on train dataset
    finalBmaTrain = "numeric",

    # @field finalBmaTest final averaging score on test dataset
    finalBmaTest = "numeric",

    # @field finalBayTrain final bayesian score on train dataset
    finalBayTrain = "numeric",

    # @field finalBayTest final bayesian score on train dataset
    finalBayTest = "numeric",

    #' initialize method for matrices depending on
    initializeMatrices = function() {
      # creates the matrices
      private$logQ <- QMatrix$new(private$numberVariables,
                                  private$variables,
                                  private$debug)
      private$probQ <- QMatrix$new(private$numberVariables,
                                   private$variables,
                                   private$debug)

      # the action to take will depend on the method to use
      switch (
        private$initializationMethod,
        '1' = {
          # random initialization of probQ matrix
          private$probQ$randomInitialization()
        },
        '2' = {
          # uniform initialization
          private$probQ$uniformInitialization()
        },
        '3' = {
          # computes bnlearn model
          private$computeBnlearnPCStable()
        },
        '4' = {
          # initialization with the result of hc algorithm
          private$computeBnlearnHC()
        },
        '5' = {
          private$computeBnlearnTabu()
        }
      )
    },

    #' computes the bound of the current situation
    #' param - bnlearn flag to show if bound on bnlearn model is
    #'                required
    #' param - test flag to show if the score for the model must
    #'                be computed ob test dataset
    #' param - update flag to show if bounds must be updated
    computeBounds = function(bnlearn, test = FALSE, update){
      # initialize the initial value for bnlearn score if required
      if(bnlearn == TRUE){
        private$initialBnlearnScore <-
            bnlearn::score(private$bnlearnModel,by.node = TRUE,
                           private$scorer$getData(), type = "bde", iss = 1)
        private$initialBnlearnScoreTest <-
            bnlearn::score(private$bnlearnModel, by.node = TRUE,
                           private$scorerTest$getData(), type = "bde", iss = 1)*private$ratio

        private$initialBnlearnScore <- sum(private$initialBnlearnScore)
        cat("initial bnlearn score: ", private$initialBnlearnScore, "\n")
        private$initialBnlearnScoreTest <- sum(private$initialBnlearnScoreTest)
      }


      # computes the global score for the model
      bounds <- private$computeVSLBound(test, update)

      # update values and store evolution info for the model
      # except for the last iteration
      if(update){
        private$updateBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      }

      # bounds computed are entropy, normal, averaging and bayesian
      c(bounds[1], bounds[2], bounds[3], bounds[4])
    },

    #' method for generating parent sets. The method returns
    #' a matrix with a certain number of rows (samples). The
    #' values equals to 1 in a variable column states that Xj
    #' must be considered as parent for such sample
    generateParentSets = function(varIndex) {
      # sets a vector with the indexes of columns
      columns <- c(1:private$numberVariables)

      # gets indexes posterior and previous to target
      posterior <- columns[columns > varIndex]
      previous <- columns[columns < varIndex]

      # makes the generation according to the parallelism level
      samples <- private$generateSets(varIndex, posterior, previous)
      t(samples)

      # return the matrix of parents
      samples
    },

    #' gets the different patterns for each database of
    #' sampled parents
    getParentPatterns = function(parents){
      # gets the patterns in parents data frame
      patterns <- unique(parents)

      # gets the counters for each pattern
      numberPatterns <- nrow(patterns)

      # get counters for each pattern
      counters <- rep(0, numberPatterns)
      for(i in 1:numberPatterns){
        counters[i] <- length(
          which(apply(parents, 1, function(x) identical(x, patterns[i, ]))))
      }

      # return the patterns and number of counters
      list("patterns" = patterns, "counters"=counters)
    },

    #' computes lower bound in order to check the evolution
    #' of the system
    #' param test - flag to show if score for normal model must
    #' be computed on test dataset
    #' param update - to show if bounds must be updated for
    #' evolution info
    computeVSLBound = function(test, update) {
      # initializes sumGlobal
      sumMAP <- 0
      sumBMA <- 0
      sumBAY <- 0

      # compute the score for all the variables using
      # test dataset. It is required to pass test argument
      # to the method
      scores <- private$computeScores(test)

      # store score for variables if required
      if(update == TRUE){
        for(i in 1:private$numberVariables){
          private$evolInfo$storeVariableMAP(i, scores[i, 1])
          private$evolInfo$storeVariableBMA(i, scores[i, 2])
          private$evolInfo$storeVariableBAY(i, scores[i, 3])
        }
      }

      # gets the summs for all the variables
      sumMAP <- sum(scores[, 1 ])
      sumBMA <- sum(scores[, 2])
      sumBAY <- sum(scores[, 3])

      # computes the global entropy
      entropy <- private$computeGlobalEntropy()

      # deletes the vector of scores
      rm(scores)

      # shows debug information if required
      if(private$debug == TRUE){
        if(private$currentIteration %% 100 == 0){
          cat("iter ", private$currentIteration, " on test: ", test,
               " entropy: ", entropy,
               " glb. score: ", sumMAP,  " (score + entropy): ",
              (sumGlobal + entropy),
              " avg: ", sumBMA,
              " bay: ", sumBAY, "\n")
          cat("entropy + sumGlobal: ", (entropy+sumGlobal), "\n")
        }
      }

      # return entropy, sumMAP, sumBMA and sumBAY
      c(entropy, sumMAP, sumBMA, sumBAY)
    },

    #' computes lower bound in order to check the evolution
    #' of the system but considering just a pair of variables
    #' because the score for the rest of variables is not
    #' modified
    #' param i - index of the first variable
    #' param j - index of the second variable
    updateScore = function(i, j) {
      # initializes result as the score of the previous iteration
      newMap <- private$evolInfo$getLastMAP()
      newBma <- private$evolInfo$getLastBMA()
      newBay <- private$evolInfo$getLastBAY()

      # gets the scores for i and j
      mapI <- private$evolInfo$getLastVariableMAP(i)
      mapJ <- private$evolInfo$getLastVariableMAP(j)
      bmaI <- private$evolInfo$getLastVariableBMA(i)
      bmaJ <- private$evolInfo$getLastVariableBMA(j)
      bayI <- private$evolInfo$getLastVariableBAY(i)
      bayJ <- private$evolInfo$getLastVariableBAY(j)
      prevEntropy <- private$evolInfo$getLastEntropy()

      newMap <- newMap - mapI - mapJ - prevEntropy
      newBma <- newBma - bmaI - bmaJ
      newBay <- newBay - bayI - bayJ

      # sets the targets for the computation
      targets <- c(i, j)

      # this vector will store the scores for variables i and j
      scores <- c()
      averagingScores <- c()
      bayesianScores <- c()

      # considers each variable
      scoresForTargets <- sapply(targets, function(target) {

        # generate parent set for Xi
        parents <- private$generateParentSets(target)

        # compute the score for the given set of parents. The
        # last parameter states that scores must be computed on
        # train datasets
        measures <- private$computeScore(target, parents, test = FALSE)

        # remove parents
        rm(parents)

        # return measures
        measures
      })

      # adds sum to sumGlobal
      newMap <- newMap + sum(scoresForTargets[1, ])
      newBma <- newBma + sum(scoresForTargets[2, ])
      newBay <- newBay + sum(scoresForTargets[3, ])

      # stores the score for the variable
      private$evolInfo$storeVariablesMAP(i, j, scoresForTargets[1, ])
      private$evolInfo$storeVariablesBMA(i, j, scoresForTargets[2, ])
      private$evolInfo$storeVariablesBAY(i, j, scoresForTargets[3, ])

      # computes the global entropy
      entropy <- private$computeGlobalEntropy()

      # shows debug information if required
      if(private$debug == TRUE){
        if(private$currentIteration %% 100 == 0){
           cat("iter ", private$currentIteration,
               " global score (1): ", (newMap+entropy),  " averaging: ",
               newBma, " bayesian: ", newBay, "bnlearn: ",
               private$initialBnlearnScore, "\n")
        }
      }

      # return sumGlobal plus entropy
      c(entropy, newMap, newBma, newBay)
    },

    #' method for updating the distribution of a given variable
	  #' param i - index of the first variable
	  #' param j - index of the second variable
    updateQValue = function(i, j) {
      # gets the parent sets for Xi
      paxi <- private$generateParentSets(i)

      # gets the parent sets for Xj
      paxj <- private$generateParentSets(j)

      # NOTE: adds both matrixes
      # pa <- private$summMatrixes(paxi, paxj)

      # complete the set of parents used for updating
      # the probabilities
      # pi1 forces j as parent: Xi <- Xj
      pi1 <- paxi
      # pi1 <- pa
      pi1[, j] <- 1

      # NOTE: remove Xi as parent
      # pi1[, i] <- 0

      # pi0 assumes j is not parent of i
      pi0 <- paxi
      # pi0 <- pa
      pi0[, j] <- 0

      # NOTE: remove Xi as parent
      # pi0[, i] <- 0

      # pj0 assumes i is not parent of j
      pj0 <- paxj
      # pj0 <- pa
      pj0[, i] <- 0

      # NOTE: remove Xj as parent
      # pj0[, j] <- 0

      # pj1 forces i as parent: Xi -> Xj
      pj1 <- paxj
      # pj1 <- pa
      pj1[, i] <- 1

      # NOTE: remove Xj as parent
      # pj1[, j] <- 0

      # computes the scores of interest
      # score for Xi <- Xj
      scorexij <- private$computeScore(i, pi1, test = FALSE)[1]
      rm(pi1)

      # score for Xj
      scorexj <- private$computeScore(j, pj0, test = FALSE)[1]
      rm(pj0)

      # score for Xi
      scorexi <- private$computeScore(i, pi0, test = FALSE)[1]
      rm(pi0)

      # score for Xj <- Xi
      scorexji <- private$computeScore(j, pj1, test = FALSE)[1]
      rm(pj1)

      # deletes the parent sets
      rm(paxi)
      rm(paxj)

      # computes the value for each orientation (Xi - Xj)
      # value for (Xi <- Xj)
      # as the sum of the score for Xi having j as parent and
      # Xj without Xi as parent
      vneg <- (scorexij + scorexj)

      # computes the score for independence (Xi ind Xj)
      # as the sum of the scores of the variables without
      # having the other as parent
      vzero <- (scorexi + scorexj)

      # computes the value for Xi -> Xj as the sum of the score
      # for Xj having i as parent and Xi without j as parent
      vpos <- (scorexji + scorexi)

      # update the values into the matrix of logarithms
      private$logQ$setValue(i, j, c(vneg, vzero, vpos), FALSE)

      # sets these values into the matrix of probabilities
      # after normalizing
      private$probQ$setValue(i, j, c(vneg, vzero, vpos), TRUE)

      # computes final entropy
      private$computeEntropy(i,j)
    },

    #' computes the score of a variable given a sample set
    #' of parents
    #' param i - index of target variable
    #' param pa - configuration of parents to consider
    #' param test - flag to show if the score for the normal
    #' model must be computed on test dataset
    computeScore = function(i, pa, test) {
      # determines the patterns and counters for this set of
      # parents
      patterns <- private$getParentPatterns(pa)
      scores <- private$computePatternsScore(i, patterns, test)

      # delete patterns ans scores info
      rm(patterns)

      # return computes scores
      scores
    },

    # @description
    #' computes the global entropy
    computeGlobalEntropy = function(){
      # composes a vector with the indexes of the variables
      variableindexes <- c(1:private$numberVariables)

      # parallel computation of entropy for each pair of variables
      entropies <- sapply(1:private$numberVariables, function(i){
        # gets the vector of valid indexes
        indexes <- variableindexes[variableindexes > i]
        sum <- 0
        for(j in indexes){
          sum <- sum + private$computeEntropy(i, j)
        }

        # return the sum
        sum
      })

      # computes the sum of all the entropies
      entropy <- sum(entropies)

      # remove entropies
      rm(entropies)

      # return the sum of all the entropies
      entropy
    },

    # @description
    #' computes the entropy for a given triplet of values
    #' representing <-, ind, ->
    # param i - index of the first variable
    # param j - index of the second variable
    # return - entropy value
    computeEntropy = function(i, j){
      # gets the values of prob for Xi-Xj
      values <- private$probQ$getValuesFromIndices(i,j)

      # computes entropy
      sum <- 0
      for(i in 1:length(values)){
        if(values[i] != 0){
          sum <- sum + values[i]*log(values[i])
        }
      }

      # return sum value
      -sum
    },

    # @description
    #' computes a model learning with bnlearn::pc.stable algorithm
    computeBnlearnPCStable = function(){
      # read data base
      data <- readDataBase(private$filenameTrain)

      # avoid logical variables
      data <- data %>% mutate_all(as.character)
      data <- data %>% mutate_all(as.factor)

      # learn using bnlearn::pc.stable
      private$bnlearnModel <- bnlearn::pc.stable(data, undirected = FALSE)
      private$initialBnlearnScore <-
            bnlearn::score(private$bnlearnModel, data, type = "bde", iss = 1)

      # gets the matrix from model
      matrix <- bnlearn::amat(private$bnlearnModel)

      # use if for initialization
      private$probQ$modelInitialization(matrix)
    },

	  # @description
	  #' computes a model learning with bnlearn::hc algorithm
	  computeBnlearnHC = function(){
	    # learn using bnlearn::hc
	    # added maxp = 4 for UCI experiments
	    private$bnlearnModel <- bnlearn::hc(private$scorer$getData(), maxp = 4,
	                                        optimized = TRUE)
	    private$initialBnlearnScore <-
	      bnlearn::score(private$bnlearnModel, private$scorer$getData(),
	                     type = "bde", iss = 1)

	    # gets the matrix from model
	    matrix <- bnlearn::amat(private$bnlearnModel)

	    # use it for initialization
	    private$probQ$modelInitialization(matrix)
	},

	# @description
	#' computes a model learning with bnlearn::tabu algorithm
	computeBnlearnTabu = function(){
	  # initialization with the result of tabu algorithm
	  # gets the data from scorer
	  data <- private$scorer$getData()

	  # avoids logical variables
	  data <- data %>% mutate_all(as.character)
	  data <- data %>% mutate_all(as.factor)

	  # learn using bnlearn::tabu
	  private$bnlearnModel <- bnlearn::tabu(data, optimized = FALSE)
	  private$initialBnlearnScore <- bnlearn::score(private$bnlearnModel, data,
	                                              type = "bde", iss = 1)

	  # gets the matrix from model
	  matrix <- bnlearn::amat(private$bnlearnModel)

	  # use if for initialization
	  private$probQ$modelInitialization(matrix)
  },

  # @description
  #' updates bounds if needed
  #' param - entropy entropy for the edge distribution
  #' param - map regular bound
	#' param - bma averaging bound
	#' param - bay bayesian bound
  updateBounds = function(entropy, map, bma, bay){
    private$currentMAP <- map + entropy
    private$currentBMA <- bma
    private$currentBAY <- bay

    # stores info of entropy and score for generating the corresponding
    # graphic
    private$evolInfo$storeEvolutionInfo(entropy, private$currentMAP,
                                           private$currentBMA,
                                           private$currentBAY)
  },

	#' makes a sequential evaluation for the scores of the variables
	#' param - test flag to show if the score for the normal model
	#' must be computed on test dataset
	computeScores = function(test){
	  # sequential loop for computing score for variable
	  scores <- lapply(1:private$numberVariables, function(i){

	    # generate parent set for Xi
	    parents <- private$generateParentSets(i)

	    # compute the score for the given set of parents
	    scoresForVar <- private$computeScore(i, parents, test)

	    # removes parents
	    rm(parents)

	    # return scores
	    scoresForVar
	  })

	  # stores scores as a matrix
	  matrix(unlist(scores), byrow = TRUE, ncol = 3)
	},

	#' generation of parent sets using sequential loop
	#' param varIndex - index of target variable
	#' param posterior - indices posterior to i
	#' param previous - indices previous to i
	generateSets = function(varIndex, posterior, previous){
	  # apply the generation of a parent set for all the rows
	  samples <- lapply(1:private$nParentSets, function(i) {
	    sample <- rep(0, private$numberVariables)
	    #  considers variables posterior to target. Xj will be
	    # parent of Xi if the random number produces the state
	    # representing <- (Xi <- Xj)
	    indexes <- posterior

	    # consider the positions of interest in the same row
	    for (j in indexes) {
	      # generates a random number
	      random <- runif(1)

	      # gets the state related to this value and this
	      # position (varIndex row and j column)
	      state <- private$probQ$getState(varIndex, j, random)

	      # adds Xj as parent if the state is -1 (the link is
	      # taken considering Xi <- Xj)
	      if (state == -1) {
	        sample[j] = 1
	      }
	    }

	    # considers the positions in the same row: those with
	    # values between 1 and varIndex-1. The vector of columns
	    # is selected just because only values over main diagonal
	    # are considered. For checking parents for variable 2, for
	    # example, the consideration of 1 as parent requires to
	    # consider position (1, 2) in the matrix. Then Xj will be
	    # added as Xi parent if the random number lead to state
	    # -> (Xj -> Xi)
	    indexes <- previous
	    for (j in indexes) {
	      # generates a random number
	      random <- runif(1)

	      # gets the state related to this value and this
	      # position (varIndex column and j row)
	      state <- private$probQ$getState(j, varIndex, random)

	      # adds Xj as parent if the state is 1 (Xj -> Xi)
	      if (state == 1) {
	        sample[j] = 1
	      }
	    }

	    # limit the number of parents if required
	    # TODO: ????? mantener o quitar?????
	    # changed limit to 4 for isolet experiments
            np <- 10
	    if(sum(sample) > np){
	       parents <- which(sample == 1)
	       parents <- sample(parents, np)
	       sample <- rep(0, private$numberVariables)
	       for(k in 1:length(parents)){
	         sample[parents[k]] = 1
	       }
	    }

	    # return the sample
	    sample
	  })

	  # return the matrix of parents
	  samples <- matrix(unlist(samples), byrow = TRUE,
	                    ncol = private$numberVariables)
	  colnames(samples) <- private$variables
	  samples
	},

	#' compute score for a variable and a given set of patterns with a
	#' sequential approach
	#' param i - index of target variable
	#' param patterns - patterns obtained from parent sets
	#' param test - flag to set if computation for the normal score
	computePatternsScore = function(i, patterns, test){
	  # iterates on the number of patterns
    # PARALLEL: change to mclapply - lapply
	  scores <- mclapply(1:length(patterns$counters), function(k) {
	    # compute the score
	    if(test == FALSE){
	      # for regular iterations
	      map <- private$scorer$score(i, patterns$patterns[k,])
	      bma <- private$scorerTest$score(i, patterns$patterns[k,])*private$ratio
	    }
	    else{
	      # just for the last computation
	      map <- private$scorerTest$score(i, patterns$patterns[k,])*private$ratio
	      bma <- map
	    }
	    bay <- bma
	    c(map, bma)
	  }, mc.set.seed = TRUE, mc.cores = private$ncores)

	  # gets results
	  scores <- matrix(unlist(scores), byrow = TRUE, ncol = 2)

	  # computes the weights for the patterns
	  weights <- patterns$counters/private$nParentSets

	  # acum <- 0
	  # for(i in 1:length(weights)){
	  #    cat(scores[i,1], " ", weights[i], " -> " , scores[i,1]*weights[i], "\n")
	  #  acum <- acum + (scores[i,1]*weights[i])
	  #    cat(" acum: ", acum, "\n")
	  # }

	  # compose the global value
	  map <- sum(weights * scores[, 1])

	  # computes the result of Bayesian model averaging
	  terms <- scores[, 2] + log(weights)
	  bma <- matrixStats::logSumExp(terms)

	  # starting point for bayesian averaging is the same value
	  # used for averaging
	  bay <- sum(weights*scores[, 2])

	  # delete scores
	  rm(scores)
	  rm(weights)
	  rm(terms)

	  # return sum and the values of averaging and bayesian
	  c(map, bma, bay)
	},

	#' gets the index of a given variable
	#' param varName - name of the target variable
	getVariableIndex = function(varName){
	  which(private$variables == varName)
	},

	#' summs two matrixes of the same dimension treating
	#' the values as booleans. The method is used for
	#' aggregating datasets of parents
	#' param mat1 - first matrix
	#' param mat2 - second matrix
	#' return - sum matrix
	summMatrixes = function(mat1, mat2){
	  # gets the matrix
	  dimr <- dim(mat1)

	  # build the final result
	  result <- matrix(0, nrow = dimr[1], ncol = dimr[2])

	  # loop over values
	  for(i in 1:dimr[1]){
	    for(j in 1:dimr[2]){
	      if(mat1[i,j] == 1 || mat2[i,j] == 1){
	        result[i,j] <- 1
	      }
	    }
	  }

          # considers each row of the matrix to check the number
          # of 1's
	  np <- 10
          for(i in 1:nr){
             row <- result[i,]
             ones <- sum(row)
             if(ones > 10){
                result[i,] <- private$limitOnes(row, np)
                cat("  prev: ", ones, " final: ", sum(result[i,]), "\n")
             }
          }

	  # return result
	  result
	},

         #' limits the number of ones with a random selection
        #' of ones
        limitOnes = function(row, limit){
           newOnes <- rep(0, length(row))
           ones <- which(row == 1)
           keep <- sample(ones, limit)
           for(k in 1:length(keep)){
              newOnes[keep[k]] = 1
           }

           # return newOnes
           newOnes
        }
 )
)
