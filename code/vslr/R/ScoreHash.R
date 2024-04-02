library(R6)
library(hash)

#' class definition. Data members
#' debug: flag for printing debug information
#' info: info about edges. This is needed to store
#' the required decoration for both sides of every edge
#' graph: graph with the structure of the model. Used for
#' making easier the computation of adjacents
ScoreHash <- R6Class("ScoreTestHash",
  public = list(
    #' @description
    #' class constructor
    #' @param debug debug flag
    #' @param debugInfo object for storing debug info
    initialize=function(debug=FALSE, debugInfo){
      # stores debug flag
      private$debug <- debug

      # stores debugInfo
      private$debugInfo <- debugInfo

      # initializes the hash structure
      private$scores <- hash::hash()

      # initializes the counters
      private$checkCalls <- 0
      private$getCalls <- 0
    },

    #' @description
    #' method to store tests information
    #' @param variable variable of interest
    #' @param parents set of parents
    #' @param value value of the score
    store=function(variable, parents, value){
      # if (private$debug){
      #   private$debugInfo$addStartText("ScoreHash$store", NULL, NULL)
      #   message <- composeString(c("Variable: ", variable,
      #                              "parents: ",
      #                              composeString(parents)))
      # }

      # form a key with variable and parents
      key <- private$getKey(variable, parents)

      # shows debug info
      # if(private$debug){
      #   message <- composeString(c("Key", key, " value: ", value))
      #   private$debugInfo$addText(message)
      # }

      # stores the info
      private$scores[[key]] <- value

      # shows debug info
      # if (private$debug){
      #   private$debugInfo$addEndText("ScoreHash$store")
      # }
    },

    #' @description
    #' method to check the existance of a score information
    #' @param variable variable of interest
    #' @param parents set of parents
    #' @return boolean value
    check=function(variable, parents){
      # shows debug info
      # if (private$debug){
      #   private$debugInfo$addStartText("ScoreHash$check", NULL, NULL)
      #   message <- composeString(c("Variable: ", variable,
      #                              "parents: ",
      #                              composeString(parents)))
      # }

      # increments the counter
      private$checkCalls <- private$checkCalls+1

      # gets the key
      key <- private$getKey(variable, parents)

      # checks the key
      testResult <- (!is.null(private$scores[[key]]))

      # if (private$debug){
      #   message <- composeString(c("Result of check: ", testResult))
      #   private$debugInfo$addText(message)
      # }

      # shows debug info
      # if(private$debug){
      #   private$debugInfo$addEndText("ScoreHash$check")
      # }

      # return testResult
      testResult
    },

    #' @description
    #' method to get the result of a score
    #' @param variable variable of interest
    #' @param parents set of parents
    #' @return score value
    get=function(variable, parents){
      # shows debug info
      # if (private$debug){
      #   private$debugInfo$addStartText("ScoreHash$get", NULL, NULL)
      #   message <- composeString(c("Variable: ", variable,
      #                              "parents: ",
      #                              composeString(parents)))
      # }

      # increments the counter
      private$getCalls <- private$getCalls+1

      # gets the key
      key <- private$getKey(variable, parents)

      # gets the result
      scoreResult <- (private$scores[[key]])

      # shows debug info
      # if(private$debug){
      #   message <- composeString(c("Score retrieved: ", scoreResult))
      #   private$debugInfo$addText(message)
      #   private$debugInfo$addEndText("ScoreHash$get")
      # }

      # return scoreResult
      scoreResult
    },

    #' @description
    #' show method for printing information
    show=function(){
      message <-
        composeString(c("---------------- ScoreHash object --------------\n",
                        "Scores in hash: ",
                        length(private$scores),
                        "\n",
                        "Check calls: ",
                        private$checkCalls, ",  Get calls: ",
                        private$getCalls,
                        "\n",
                        "------------------------------------------------\n"))

      # return message
      message
    },

    #' @description
    #' sets the object used for storing debug information
    #' @param debugInfo object for storing debug information
    setDebugInfoObject = function(debugInfo){
      private$debugInfo = debugInfo
    }
  ),

  #' class private part
  private = list(
      #' hash of scores
      scores="hash",

      #' counter of calls to check
      checkCalls="numeric",

      #' counter of calls to get
      getCalls="numeric",

      #' boolean flag
      debug="logical",

      #' object of class VSLearningInfo
      debugInfo="VSLearningInfo",

      #' composes a key from the variables involved in the
      #' score
      getKey=function(variable, parents){
        # form the key with variable and parents (once ordered)
        key <- c(variable,sort(parents))

        # now paste all of them into a single string
        key <- paste0(key, collapse = "")
      }
  )
)

