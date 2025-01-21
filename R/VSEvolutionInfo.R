library(R6)
library(stringr)
library(bnlearn)
library(ggplot2)
library(hash)

#' Class for storing debug info about the execution
#' @param name class name
#' @param public attributes
#' @param private attributes
#' @importFrom R6 R6Class
EvolutionInfo <- R6Class("EvolutionInfo",
  public = list(
    #' @description
    #' class constructor
    #' @param filename name of file to generate
    #' @param initializationMethod id of the method used for initialization
    #' @param maxIter maximum number of iterations
    #' @param nParentSets number of configurations for parent sets
    #' @param variables variables of the graph under analysis
    initialize = function(filename, initializationMethod, maxIter,
                          nParentSets, variables){
      # sets values for the data members specifying the
      # parameters for the learning process
      private$filename <- filename
      private$initializationMethod <- initializationMethod
      private$maxIter <- maxIter
      private$nParentSets <- nParentSets

      # sets basename
      private$basename <- extractsNetName(filename)

      # sets partition name
      private$partition <- getIndex(filename)

      # sets the variables and numberVariables
      private$variables <- variables
      private$numberVariables <- length(variables)

      # gets the name of the folder where the files will be
      # placed
      private$folder <- createTraceFolder(private$filename, maxIter, nParentSets)

      # initializes text
      private$text <- ""

      # initialize stackLevel to 1
      private$stackLevel <- 1

      # initialize collections for bounds
      private$maps <- c()
      private$bmas <- c()
      private$bays <- c()
      private$entropy <- c()
      private$hashmaps <- hash::hash()
      private$hashbmas <- hash::hash()
      private$hashbays <- hash::hash()
      for(i in 1:private$numberVariables){
        # stores an empty vector as starting point
        private$hashmaps[[private$variables[[i]]]] <- c()
        private$hashbmas[[private$variables[[i]]]] <- c()
        private$hashbays[[private$variables[[i]]]] <- c()
      }

      # creates file
      private$createFile()
    },

    #' @description
    #' adds text with debug info about begining a method
    #' @param methodName name of the method
    #' @param argumentNames a vector with argument names
    #' @param argumentValues vector with argument values
    addStartText = function(methodName, argumentNames, argumentValues){
        # increments the stack level
        private$incrementStackLevel()

        # composes the nunber of spaces according to stack level
        spaces <- strrep(" ", private$stackLevel*2)

        # add info with the method
        text <- composeString(c(spaces, "------ ", methodName,
                                " - ", "start", " ------\n"))

        if(!is.null(argumentNames)){
          # add information for each argument
          for(i in 1:length(argumentNames)){
            text <- paste0(text, spaces)
            text <- paste0(text, argumentNames[i], ": ", argumentValues[i], "\n")
          }
        }

        # adds a final line separating the text
        text <- paste0(text, spaces)
        text <- paste0(text, "......................................\n")

        # now adds all this information to the data member
        private$text <- paste0(private$text, text)

        # write the actual content
        private$write()
    },

    #' @description
    #' adds text with debug info about exiting a method
    #' @param methodName name of the method just finishing
    addEndText = function(methodName){
        # composes the number of spaces according to stack level
        spaces <- strrep(" ", private$stackLevel*2)

        # add info about exiting the method wich name
        # is passed as argument
        text <- composeString(c(spaces, "------ ", methodName,
                                " - ", "end", " ------\n"))

        # now adds all this information to the data member
        private$text <- paste0(private$text, text)

        # decrements the stack level
        private$decrementStackLevel()

        # write the actual content
        private$write()
    },

    #' @description
    #' add regular text to trace
    #' @param message text to add to the information to be shown
    addText = function(message){
      # composes the number of spaces according to stack level
      spaces <- strrep(" ", private$stackLevel*2)

      # add spaces to text
      private$text <- paste0(private$text, spaces)

      # ensure the correct number of spaces for each
      # line in message
      message <- stringr::str_replace_all(message, "\n", paste0("\n", spaces))

      # add the message
      private$text <- paste0(private$text, message)

      # add a new line
      private$text < paste0(private$text, "\n")

      # write the actual content
      private$write()
    },

    #' @description
    #' generates a file with the data of bounds
    #' @param maxIter iterations of the experiment
    #' @param nParentSets number of parent sets employed
    #' @param bnlearnTrain score of bnlearn model on train dataset
    #' @param bnlearnTest score of bnlearn model on test dataset
    #' @param mapTest score of our model on test dataset
    #' @param finalbma final average score on test
    #' @param finalbay final bayesian score on test
    generateBoundsFile = function(maxIter, nParentSets,
                                  bnlearnTrain, bnlearnTest, mapTest,
                                  finalbma, finalbay){
      # compose the name of the file with the data
      basename <- paste("./results/", private$basename, "/", private$basename,
                        "-", private$partition, ".evol", sep = "")
      cat("basename with evolution data: ", basename, "\n")

      # writes initial scores
      write(bnlearnTrain, ncolumns = 1, file = basename)
      write(bnlearnTest, ncolumns = 1, file = basename, append = TRUE)
      write(mapTest, ncolumns = 1, file = basename, append = TRUE)
      write(finalbma, ncolumns = 1, file = basename, append = TRUE)
      write(finalbay, ncolumns = 1, file = basename, append = TRUE)

      # writes all the vector
      for(i in 1:length(private$maps)){
        write(c(private$maps[i], private$bmas[i], private$bays[i]),
            ncolumns = 3, file = basename, sep =",", append = TRUE)
      }
    },

    #' @description
    #' generates a graphic with the evolution of the bounds obtained
    #' during the search process
    #' @param iteration number of iteration where the graphics is generated
    #' @param bnlearnTrain bnlearn score on train data
    #' @param bnlearnTest bnlearn score on test data
    #' @param mapTest final value of bound on test data
    generateMapsGraphic = function(iteration,
                                     bnlearnTrain = -Inf,
                                     bnlearnTest = -Inf,
                                     mapTest = -Inf){
      # compose the name for the graph
      filename <- paste("./results/", private$basename, "/",
                        private$basename, "-", private$partition,
                        "-map-", iteration, "-evol.pdf", sep = "")

      #sets values for y-axis depending of initial score
      if(bnlearnTrain == -Inf){
        bnlearnTrain <- mean(private$maps)
      }
      if(bnlearnTest == -Inf){
        bnlearnTest <- mean(private$maps)
      }
      if(mapTest == -Inf){
        mapTest <- mean(private$maps)
      }

      # sets limits for axis according to bound values
      # ymin = min(private$bounds, bnlearnTrain, bnlearnTest, finalTestScore)
      ymin = min(private$maps, bnlearnTrain)
      # ymax = max(private$maps, bnlearnTrain, bnlearnTest, mapTest)
      ymax = max(private$maps, bnlearnTrain)
      diff = ymax-ymin
      ymin = ymin - (0.1*diff)
      ymax = ymax + (0.1*diff)
      step = diff/1000
      cat("ymin: ", ymin, " ymax: ", ymax, "\n")

      # compose a data frame with the values of iteration bounds
      maps <- data.frame(Iterations = 1:length(private$maps), elbo = private$maps)

      # determine the positions for labels: xmax - 20 and finalScoreText - 10
      # for y position
      elbox = length(private$maps) - 20
      difference <- 30
      elboy = max(private$maps) + (step*difference)
      bnlearnx = elbox
      bnlearny = bnlearnTrain + (step*difference)
      cat("elbox: ", elbox, " elboy: ", elboy, " bnlearnx: ", bnlearnx,
          " bnlearny: ", bnlearny, "\n")

      # compose the ggplot graph
      pdf(filename)
      graph <- ggplot2::ggplot(data = maps) +
               scale_colour_manual(name = "", values  = c("darkred", "darkblue")) +
               xlim(1, length(private$maps)) +
               ylim(ymin, ymax) +
               xlab("Iterations") +
               ylab("Scores") +
               geom_smooth(aes(Iterations, elbo, colour = "elbo"), size = 1.2) +
               # annotate(geom="text", x = elbox, y = elboy, label="ELBO",
               #           colour ="darkblue", size = 3, fontface = 1) +
               # geom_line(aes(y = v2), color = "steelblue") +
               geom_hline(aes(yintercept = bnlearnTrain, colour = "bnlearn"),
               size = 1.2, linetype = "dashed") +
               theme(legend.position = c(0.8,0.95))
               # annotate(geom = "text", x = bnlearnx, y = bnlearny, label="bnlearn",
               #         colour = "darkred", size = 3, fontface = 1)
               # geom_hline(yintercept = bnlearnTest, linetype = "dotted" ,
               #   color = "red", size = 1) +
               # geom_hline(yintercept = finalTestScore, linetype = "dotted" ,
               #   color = "green", size = 1)

      # prints the graph
      print(graph)

      # close the device
      dev.off()
    },

    #' @description
    #' generates a graphic with the evolution of the averaging bounds obtained
    #' during the search process
    #' @param iteration number of iteration where the graphics is generated
    #' @param bnlearnTrain bnlearn score on train data
    #' @param bnlearnTest bnlearn score on test data
    #' @param mapTest final value of averaging on test data
    generateBMAsGraphic = function(iteration,
                                        bnlearnTrain = -Inf,
                                        bnlearnTest = -Inf,
                                        mapTest = -Inf){
      # compose the name for the graph
      filename <- paste("./results/", private$basename, "/",
                        private$basename, "-", private$partition,
                        "-bma-", iteration, "-evol.pdf", sep = "")

      #sets values for y-axis depending of initial score
      if(bnlearnTrain == -Inf){
        bnlearnTrain <- mean(private$bmas)
      }
      if(bnlearnTest == -Inf){
        bnlearnTest <- mean(private$bmas)
      }
      if(finalTestScore == -Inf){
        finalTestScore <- mean(private$bmas)
      }

      # sets limits for axis according to bound values
      ymin = min(private$avgBounds, bnlearnTrain, bnlearnTest, mapTest)
      ymax = max(private$avgBounds, bnlearnTrain, bnlearnTest, mapTest)
      diff = ymax-ymin
      ymin = ymin - (0.2*diff)
      ymax = ymax + (0.2*diff)

      # compose a data frame with the values iteration bounds
      bounds <- data.frame(v1 = private$bmas)

      # compose the ggplot graph
      pdf(filename)
      graph <- ggplot2::ggplot(bounds, aes(x = 1:length(private$bmas))) +
               xlim(1, length(private$bmas)) +
               ylim(ymin, ymax) +
               xlab("Iterations") +
               ylab("Averaging scores") +
               geom_line(aes(y = v1), color = "darkred") +
               # geom_line(aes(y = v2), color = "steelblue") +
               geom_hline(yintercept = bnlearnTrain, linetype = "dashed" ,
               color = "red", size = 1) +
               geom_hline(yintercept = bnlearnTest, linetype = "dotted" ,
                  color = "red", size = 1)+
               geom_hline(yintercept = mapTest, linetype = "dotted" ,
                  color = "green", size = 1)

      # prints the graph
      print(graph)

      # close the device
      dev.off()
    },

    #' @description
    #' generates a graphic with the evolution of the Bayesian bounds obtained
    #' during the search process
    #' @param iteration number of iteration where the graphics is generated
    #' @param bnlearnTrain bnlearn score on train data
    #' @param bnlearnTest bnlearn score on test data
    #' @param mapTest final value of averaging on test data
    generateBAYsGraphic = function(iteration,
                                        bnlearnTrain = -Inf,
                                        bnlearnTest = -Inf,
                                        mapTest = -Inf){
      # compose the name for the graph
      filename <- paste("./results/", private$basename, "/",
                        private$basename, "-", private$partition,
                        "-bay-", iteration, "-evol.pdf", sep = "")

      #sets values for y-axis depending of initial score
      if(bnlearnTrain == -Inf){
        bnlearnTrain <- mean(private$bays)
      }
      if(bnlearnTest == -Inf){
        bnlearnTest <- mean(private$bays)
      }
      if(finalTestScore == -Inf){
        finalTestScore <- mean(private$bays)
      }

      # sets limits for axis according to bound values
      ymin = min(private$bayBounds, bnlearnTrain, bnlearnTest, mapTest)
      ymax = max(private$bayBounds, bnlearnTrain, bnlearnTest, mapTest)
      diff = ymax-ymin
      ymin = ymin - (0.2*diff)
      ymax = ymax + (0.2*diff)

      # compose a data frame with the values of iteration bounds
      bounds <- data.frame(v1 = private$bays)

      # compose the ggplot graph
      pdf(filename)
      graph <- ggplot2::ggplot(bounds, aes(x = 1:length(private$bays))) +
        xlim(1, length(private$bays)) +
        ylim(ymin, ymax) +
        xlab("Iterations") +
        ylab("Bayesian scores") +
        geom_line(aes(y = v1), color = "darkred") +
        # geom_line(aes(y = v2), color = "steelblue") +
        geom_hline(yintercept = bnlearnTrain, linetype = "dashed" ,
                   color = "red", size = 1) +
        geom_hline(yintercept = bnlearnTest, linetype = "dotted" ,
                  color = "red", size = 1) +
        geom_hline(yintercept = finalTestScore, linetype = "dotted" ,
                  color = "green", size = 1)

      # prints the graph
      print(graph)

      # close the device
      dev.off()
    },

    #' @description
    #' generates a graphic with the evolution of the bounds obtained
    #' during the search process
    generateEntropyGraphic = function(){
      # compose the name for the graph
      filename <- paste("./result/", private$basename, "/",
                        private$basename, "-", private$partition,
                        "-ent-", iteration, "-evol.pdf", sep = "")

      # sets limits for axis according to bound values
      ymin = min(private$entropy)
      ymax = max(private$entropy)
      diff = ymax-ymin
      ymin = ymin - (0.2*diff)
      ymax = ymax + (0.2*diff)

      # compose a data frame with the values of entropy
      bounds <- data.frame(v1 = private$entropy)

      # compose the ggplot graph
      pdf(filename)
      graph <- ggplot2::ggplot(bounds, aes(x = 1:length(private$entropy))) +
               xlim(1, length(private$entropy)) +
               ylim(ymin, ymax) +
               xlab("Iterations") +
               ylab("Entropy") +
               geom_line(aes(y = v1), color = "darkred")

      # prints the graph
      print(graph)

      # close the device
      dev.off()
    },

    #' @description
    #' return the text stored with debg information
    getText = function(){
      private$text
    },

    #' @description
    #' stores initial information about bounds
    #' @param map initial value for bound and bestBound
    #' @param bma initial value for averagingBound
    #' @param bay initial value for bayesianBound
    storeInitialBounds = function(map, bma, bay) {
      # stores the data into private data members
      private$maps <- c(private$maps, map)
      private$bmas <- c(private$bmas, bma)
      private$bays <- c(private$bays, bay)
    },

    #' @description
    #' stores a new measure of score for a certain variable
    #' @param varIndex index of the variable of interest
    #' @param score value of the score to store
    storeVariableMAP = function(varIndex, score) {
      varname <- private$variables[[varIndex]]
      # stores the corresponding value into the hash entry
      private$hashmaps[[varname]] <- c(private$hashmaps[[varname]], score)
    },

    #' @description
    #' stores a new measure of score for a pair of variables
    #' @param varIndex1 index of the first variable of interest
    #' @param varIndex2 index of the second target variable
    #' @param scores values of the scores to store
    storeVariablesMAP = function(varIndex1, varIndex2, scores) {
      varname1 <- private$variables[[varIndex1]]
      varname2 <- private$variables[[varIndex2]]
      # stores the corresponding values into the hash entry
      private$hashmaps[[varname1]] <- c(private$hashmaps[[varname1]], scores[1])
      private$hashmaps[[varname2]] <- c(private$hashmaps[[varname2]], scores[2])
    },

    #' @description
    #' stores a new measure of averaging score for a certain variable
    #' @param varIndex index of the variable of interest
    #' @param score value of the score to store
    storeVariableBMA = function(varIndex, score) {
      varname <- private$variables[[varIndex]]
      # stores the corresponding value into the hash entry
      private$hashbmas[[varname]] <- c(private$hashbmas[[varname]], score)
    },

    #' @description
    #' stores a new measure of averaging score for a pair of variables
    #' @param varIndex1 index of the first variable of interest
    #' @param varIndex2 index of the second target variable
    #' @param scores values of the scores to store
    storeVariablesBMA = function(varIndex1, varIndex2, scores) {
      varname1 <- private$variables[[varIndex1]]
      varname2 <- private$variables[[varIndex2]]
      # stores the corresponding values into the hash entry
      private$hashbmas[[varname1]] <-
        c(private$hashbmas[[varname1]], scores[1])
      private$hashbmas[[varname2]] <-
        c(private$hashbmas[[varname2]], scores[2])
    },

    #' @description
    #' stores a new measure of bayesian score for a certain variable
    #' @param varIndex index of the variable of interest
    #' @param score value of the score to store
    storeVariableBAY = function(varIndex, score) {
      varname <- private$variables[[varIndex]]
      # stores the corresponding value into the hash entry
      private$hashbays[[varname]] <- c(private$hashbays[[varname]], score)
    },

    #' @description
    #' stores a new measure of bayesian score for a pair of variables
    #' @param varIndex1 index of the first variable of interest
    #' @param varIndex2 index of the second target variable
    #' @param scores values of the scores to store
    storeVariablesBAY = function(varIndex1, varIndex2, scores) {
      varname1 <- private$variables[[varIndex1]]
      varname2 <- private$variables[[varIndex2]]
      # stores the corresponding values into the hash entry
      private$hashbays[[varname1]] <-
        c(private$hashbays[[varname1]], scores[1])
      private$hashbays[[varname2]] <-
        c(private$hashbays[[varname2]], scores[2])
    },


    #' @description
    #' stores information about the evolution of the system in order
    #' to analyze its performance
    #' @param entropy entropy value
    #' @param map bound for current iteration
    #' @param bma averaging bound for the current iteration
    #' @param bay Bayesian bound for the current iteration
    storeEvolutionInfo = function(entropy, map, bma, bay) {
      # stores the bounds
      private$entropy <- c(private$entropy, entropy)
      private$maps <- c(private$maps, map)
      private$bmas <- c(private$bmas, bma)
      private$bays <- c(private$bays, bay)
    },

    #' @description
    #' gets the score for the last iteration
    getLastMAP = function(){
      private$maps[length(private$maps)]
    },

    #' @description
    #' gets the averaging score for the last iteration
    getLastBMA = function(){
      private$bmas[length(private$bmas)]
    },

    #' @description
    #' gets the bayesian score for the last iteration
    getLastBAY = function(){
      private$bays[length(private$bays)]
    },

    #' @description
    #' gets the last score for a given variable and a certain
    #' iteration
    #' @param index index of the target variable
    #' @param iteration target iteration
    getLastVariableMAP = function(index){
      # return the score for the required iteration (the
      # first value belongs to the score computed just
      # after initialization and before improving parameters)
      scoresForVariable <- private$hashmaps[[private$variables[index]]]
      scoresForVariable[length(scoresForVariable)]
    },

    #' @description
    #' gets the last averaging score for a given variable and a certain
    #' iteration
    #' @param index index of the target variable
    #' @param iteration target iteration
    getLastVariableBMA = function(index){
      # return the score for the required iteration (the
      # first value belongs to the score computed just
      # after initialization and before improving parameters)
      scoresForVariable <- private$hashbmas[[private$variables[index]]]
      scoresForVariable[length(scoresForVariable)]
    },

    #' @description
    #' gets the last bayesian score for a given variable and a certain
    #' iteration
    #' @param index index of the target variable
    #' @param iteration target iteration
    getLastVariableBAY = function(index){
      # return the score for the required iteration (the
      # first value belongs to the score computed just
      # after initialization and before improving parameters)
      scoresForVariable <- private$hashbays[[private$variables[index]]]
      scoresForVariable[length(scoresForVariable)]
    },

    #' @description
    #' gets the scores for a given variable
    #' @param index index of the target variable
    getVariableMAPs = function(index){
      # return the scores for the required variable
      private$hashmaps[[private$variables[index]]]
    },

    #' @description
    #' gets the last value of entropy stored
    getLastEntropy = function(){
      private$entropy[length(private$entropy)]
    },

    #' @description
    #' close file
    closeTrace = function(){
      close(private$traceFile)
    }
  ),

  #' #################################################################
  #' private part of the class
  #' #################################################################

  #' data members description and private methods
  private = list(
    # @field filename filename with the data
    filename = "character",

    # @field basename name of the dataset
    basename = "character",

    # @field name of the partition used for learning
    partition = "character",

    # @field initializationMethod initialization method
    initializationMethod = "numeric",

    # @field maxIter maximun number of iterations
    maxIter = "numeric",

    # @field nParentSets number of parents to sample
    nParentSets = "numeric",

    # @field numberVariables number of variables of the graph under analysis
    numberVariables = "numeric",

    # @field variables names of the variables
    variables = "character",

    # @field text text for adding info about the execution
    text = "character",

    # @field bounds vector for storing information about map obtained
    #' for each iteration
    maps = "numeric",

    # @field averaging bounds vector for storing information about
    #' bma obtained for each iteration
    bmas = "numeric",

    # @field averaging bounds vector for storing information about
    #' bouds obtained for each iteration
    bays = "numeric",

    # @field entropy entropy for each iteration
    entropy = "numeric",

    # @field hashmaps scores for each variable
    hashmaps = "hash",

    # @field hashbmas scores computes as Bayesian model
    #        averaging for each variable
    hashbmas = "hash",

    # @field hashbays scores computes as Bayesian model
    # for each variable
    hashbays = "hash",

    #' folder where trace files will be stored
    folder = "character",

    # @field traceFile file used for saving trace info
    traceFile = "file",

    # @field stackLevel number for controlling the level of the method
    #' under execution (entries in stack) in order to
    #' tabulate the text
    stackLevel = "numeric",

    # @description
    #' creates a string describing the structure of the net
    # @param parents list of parents for each node
    createBnlearnString = function(parents){
      # initializes description
      description <- ""

      # considers each variable
      for(i in 1:private$numberVariables){
        # gets the name of the variable
        x <- private$variables[i]

        # adds variable name to description
        varDescription <- paste0("[", x)

        # gets x parents
        xparents <- parents[[x]]

        # adds data to description
        if(length(xparents) != 0){
          # adds separator for parents
          varDescription <- paste0(varDescription,"|")

          # add parent names
          varDescription <- paste0(varDescription,
                                   paste(xparents, collapse = ":"))
        }
        # finally adds "]"
        varDescription <- paste0(varDescription,"]")

        # add the description of the var to description
        description <- paste0(description, varDescription)
      }

      # return description
      description
    },

    # @description
    #' creates the file for the text
    createFile = function(){
      # remove the path for filename
      filename <- basename(private$filename)

      # compose with the folder
      filename <- paste(private$folder, filename, sep = "/")

      # creates the name of the file using the data members
      filename <- paste(filename, private$initializationMethod, sep="-")
      filename <- paste(filename, private$maxIter, sep="-")
      filename <- paste(filename, private$nParentSets, sep="-")
      filename <- paste(filename, "txt", sep=".")

      # creates the file
      private$traceFile <- file(filename, open = "w")
    },

    # @description
    #' decrements the counter storing stack level
    decrementStackLevel = function(){
      private$stackLevel = private$stackLevel -1
    },

    # @description
    #' increments the counter storing stack level
    incrementStackLevel = function(){
      private$stackLevel <- private$stackLevel + 1
    },

    # @description
    #' writes the current content of text and clear its content
    write = function(){
      # write the content of text
      write(private$text, file=private$traceFile, append=TRUE)

      # reset text content
      private$text <- ""
    }
  )
)
