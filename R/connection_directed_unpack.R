#' @title Directed Unpack Connection
#' @name DirectedUnpackConnection
#' @description Implementation of \code{ConnectionInterface} which represents a
#'              directed flow of info between a head module to a tail module.
#'              This is an extension of \code{DirectedConnection} designed to
#'              unpack a specific named value out of a list outputted by the
#'              head module as input into the tail module's execution rather
#'              than using the direct output of the head module as input into
#'              the tail module.
#' @importFrom R6 R6Class
#' @section Class Constructor:
#' \describe{
#'     \item{\code{new(name, headModule, tailModule, inputArgument)}}{
#'         \itemize{
#'             \item{\bold{\code{name}}: unique character string that identifies
#'                    this connection.}
#'             \item{\bold{\code{headModule}}: obj that implements
#'                    \code{ModuleInterface} or a character string that
#'                     represents the head module of this connection.}
#'             \item{\bold{\code{tailModule}}: obj that implements
#'                    \code{ModuleInterface} or a character string that
#'                     represents thetail module of this connection.}
#'             \item{\bold{\code{inputArgument}}: name of the inputArgument
#'                   the output of the head module is used for in the tail
#'                   module's execution. Should be named with what value should
#'                   be unpacked out of the head module's return list and used
#'                   for the tail module's execution.}
#'         }
#'     }
#' }
#' @section Inheritance From:
#' \describe{
#'     \item{\link[updraft]{DirectedConnection}}{}
#' }
#' 
#' @section Public:
#' \describe{
#'     \item{\code{filterOutputValue(outputValue)}}{
#'         \itemize{
#'             \item{Filters the output of the \code{headModule}, \code{outputValue}, to what is expected by the \code{tailModule}. With this implementation of a directed connection, assumes \code{outputValue} is a list and the associated name of \code{inputArgument} is used to filter down \code{outputValue}.}
#'             \item{\bold{Returns}: filtered \code{outputValue}}
#'         }
#'     }
#' }
#' @section Static Class Methods:
#' \describe{
#'     \item{\link[updraft]{DirectedUnpackConnection-cash-initFromSaveData}}{}
#' } 
#' @export
DirectedUnpackConnection <- R6::R6Class("DirectedUnpackConnection"
    , inherit = DirectedConnection
    
    , public = list(
        initialize = function(name
                              , headModule
                              , tailModule
                              , inputArgument
        ) {
            super$initialize(name = name
                             , headModule = headModule
                             , tailModule = tailModule
                             , inputArgument = inputArgument)
            
            self$errorCheck()
        }
        
        , errorCheck = function(executionCheck = FALSE
                                , ...
        ) {
            if(length(names(private$inputArgument)) != 1
               && names(private$inputArgument)[1] != "") {
                UpDraftSettings$errorLogger("inputArgument must be named with the value to unpack from the output of the associated head module.")
            }
        }
        
        , filterOutputValue = function(outputValue) {
            if (!is.list(outputValue)
                || length(names(outputValue)) != length(outputValue)) {
                UpDraftSettings$errorLogger("outputValue is not a named list - DirectedUnpackConnection is intended for unpacking a value out of a returned named list from a head module and pass the unpacked value as an argument into a tail module's execution")
            }

            if (!all(names(private$inputArgument) %in% names(outputValue))){
                UpDraftSettings$errorLogger(paste("the provided inputArgument",paste0(private$inputArgument, collapse = ",")
                                                  ,"are not fully contained within the outputValue:",paste0(names(outputValue),collapse = ",")))
            }
            
            unpackedValue <- outputValue[[names(private$inputArgument)]]
            return(unpackedValue)
        }
        
        , getSaveInfo = function() {
            internalState <- super$getSaveInfo()
            
            internalState[['unpackValue']] <- names(private$inputArgument)
            
            return(internalState)
        }
    )
)

#' @title DirectedUnpackConnection Init From Save Data
#' @name DirectedUnpackConnection$initFromSaveData
#' @description Static factory method that generates a DirectedUnpackConnection
#'              from json save data.
#' @param saveData json saved data generated through the method \code{getSaveInfo}.
DirectedUnpackConnection$initFromSaveData <- function(saveData) {
    inputArgument <- saveData$inputArgument
    names(inputArgument) <- saveData$unpackValue
    
    connection <- DirectedUnpackConnection$new(name = saveData$name
                                         , headModule = saveData$headModule
                                         , tailModule = saveData$tailModule
                                         , inputArgument = inputArgument)
    return(connection)
}
