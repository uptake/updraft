#' @title Directed Connection Connection
#' @name DirectedConnection
#' @description Implementation of \code{ConnectionInterface} which represents a directed flow of info
#'              between a head module to a tail module. 
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
#'             \item{\bold{\code{inputArgument}}: Name of the inputArgument
#'                   the output of the head module is used for in the tail
#'                   module's execution.}
#'          }
#'     }
#' }
#' @section Public:
#' \describe{
#'     \item{\code{errorCheck(executionCheck = FALSE, ...)}}{
#'         \itemize{
#'             \item{Runs error checking on the internal state of this object for erroneous values.}
#'             \item{\code{executionCheck}: when set to \code{TRUE}, runs additional checks to determine if ready for execution.}
#'             \item{\code{...}: not used by this component.}
#'             \item{\bold{Returns}: \code{NULL}, will raise a fatal error if an error is found.}
#'         }
#'     }
#'     
#'     \item{\code{filterOutputValue(outputValue)}}{
#'         \itemize{
#'             \item{Filters the output of the \code{headModule}, \code{outputValue}, to what is expected by the \code{tailModule}. With this implementation of a connection, just returns \code{outputValue}.}
#'             \item{\bold{\code{outputValue}}: output of \code{headModule}.}
#'             \item{\bold{Returns}: \code{outputValue}}
#'         }
#'     }
#'     
#'     \item{\code{getHeadModuleName()}}{
#'         \itemize{
#'             \item{Gets the name of the head module associated with this connection.}
#'             \item{\bold{Returns}: a character string}
#'         }
#'     }
#'     
#'     \item{\code{getName()}}{
#'         \itemize{
#'             \item{Gets the character string that identifies this connection.}
#'             \item{\bold{Returns}: a character string}
#'         }
#'     }
#'     
#'     \item{\code{getInputArgument()}}{
#'         \itemize{
#'             \item{Gets the argument the output of the head module is used for in the tail module's execution.}
#'             \item{\bold{Returns}: a character vector}
#'         }
#'     }
#'     
#'     \item{\code{getSaveInfo()}}{
#'         \itemize{
#'             \item{Gets a named list of internal states of this object which can be used to save this object on disk.}
#'             \item{\bold{Returns}: a named list}
#'         }
#'     }
#'     
#'     \item{\code{getTailModuleName()}}{
#'         \itemize{
#'             \item{Gets the name of the tail module associated with this connection.}
#'             \item{\bold{Returns}: a character string}
#'         }
#'     }
#'     
#' }
#' @section Private:
#' \describe{
#'     \item{\code{inputArgument}}{
#'         \itemize{
#'             \item{Name of the argument the output of the head module is used for in the tail module's execution.}
#'         }
#'     }
#'     
#'     \item{\code{headModuleName}}{
#'         \itemize{
#'             \item{Stores name of the head module.}
#'         }
#'     }
#'     
#'     \item{\code{tailModuleName}}{
#'         \itemize{
#'             \item{Stores name of the tail module.}
#'         }
#'     }
#'     
#'     \item{\code{extractNameFromModule(module)}}{
#'         \itemize{
#'             \item{Extracts name of \code{module}.}
#'             \item{\bold{\code{module}}: obj that implements \code{ModuleInterface} or a character string name of a module.}
#'             \item{\bold{Returns}: Character string of \code{module}. Will raise a fatal error if module is
#'                   not a character vector or a valid implementation of \code{ModuleInterface}.}
#'         }
#'     }
#' }
#' @section Static Class Methods:
#' \describe{
#'     \item{\link[updraft]{DirectedConnection-cash-initFromSaveData}}{}
#' } 
#' @export
DirectedConnection <- R6::R6Class("DirectedConnection"
    , inherit = ConnectionInterface
    
    , public =  list(
        initialize = function(name
                              , headModule
                              , tailModule
                              , inputArgument
        ) {
            if(is.character(name) 
               && length(name) == 1
               && name != "") {
                private$name = name
            } else {
                UpDraftSettings$errorLogger("name parameter must be a non-empty character string")   
            }
                
            private$headModuleName <- private$extractNameFromModule(headModule)
            private$tailModuleName <- private$extractNameFromModule(tailModule)
            
            if (is.character(inputArgument) && length(inputArgument) == 1) {
                private$inputArgument <- inputArgument
            } else {
                UpDraftSettings$errorLogger("inputArgument parameter must be a single character string")
            }
        }
        
        , errorCheck =  function(executionCheck = FALSE
                                 , ...
        ) {
            # field values are checked in the constructor 
            return(NULL)
        }
        
        , filterOutputValue = function(outputValue) {
            return(outputValue)
        }
        
        , getHeadModuleName = function() {
            return(private$headModuleName)    
        }
        
        , getName = function() {
            return(private$name)    
        }
        
        , getInputArgument = function() {
            return(private$inputArgument)
        }

        , getSaveInfo = function() {
            internalState = list()
        
            internalState[['class']] <- class(self)[1]
            internalState[['name']] <- private$name
            internalState[['headModule']] <- private$headModuleName
            internalState[['tailModule']] <- private$tailModuleName
            internalState[['inputArgument']] <- private$inputArgument
    
            return(internalState)
        }
        
        , getTailModuleName = function() {
            return(private$tailModuleName)
        }
        
    )
    
    , private = list(
        headModuleName = ""
        , inputArgument = ""
        , name = ""
        , tailModuleName = ""

        , extractNameFromModule = function(module, descriptionOfModule) {
            if(is.character(module) && length(module) == 1) {
                return(module)
            } else if("ModuleInterface" %in% class(module)) {
                return(module$getName())
            }
            
            UpDraftSettings$errorLogger(descriptionOfModule, "is not a valid implementation of ModuleInterface")
        }
    ) 
)

#' @title DirectedConnection Init From Save Data
#' @name DirectedConnection$initFromSaveData
#' @description Static factory method that generates a DirectedConnection
#'              from json save data.
#' @param saveData json saved data generated through the method \code{getSaveInfo}.
DirectedConnection$initFromSaveData <- function(saveData) {
    connection <- DirectedConnection$new(name = saveData$name
                                         , headModule = saveData$headModule
                                         , tailModule = saveData$tailModule
                                         , inputArgument = saveData$inputArgument)
    return(connection)
}

#' @title is Directed Connection
#' @name is.DirectedConnection
#' @description Returns a boolean indicating if \code{obj} is a valid
#'              \code{DirectedConnection}.
#' @importFrom R6 is.R6
#' @param obj object instance
#' @return boolean where \code{TRUE} indicates \code{obj} is a \code{DirectedConnection}
#'         and \code{FALSE} otherwise.
#' @export
is.DirectedConnection <- function(obj) {
    if ("DirectedConnection" %in% class(obj) && R6::is.R6(obj)) {
        return(TRUE)
    }
    
    return(FALSE)
}
