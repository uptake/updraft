#' @title Module Interface
#' @name ModuleInterface
#' @importFrom R6 R6Class
#' @description Defines a contract of implementation for any module based class,
#'   i.e. a class that extends this interface. This class cannot be instantiated
#'   directly as it is abstract.
#' @section Contracted Methods: \describe{
#'   \item{\code{clearOutputCache()}}{Clears out the cached output of the
#'   associated module.} 
#'   \item{\code{getFuncObj()}}{Gets the base R function
#'   object that is called when the associated module's execution is started.}
#'   \item{\code{getInput()}}{Gets a named logical vector where the names are
#'   the possible inputs into a module and the values indicate if an input is
#'   required.} 
#'   \item{\code{getOutput()}}{Gets the output of the associated
#'   module.} 
#'   \item{\code{hasCompleted()}}{Indicates if the associated module's
#'   execution has completed.} 
#'   \item{\code{startExecution(argsList)}}{Starts the
#'   execution of the associated module with \code{argsList} as arguments.}
#'   \item{\code{getName()}}{
#'         \itemize{\item{Gets the name of this function.}
#'                  \item{\bold{Returns}: character string that is the name
#'                   of this module.}}}
#'   \item{\code{setName(arg)}}{Set the name of the module. Considers arg of
#'   length 1 of character type.}
#'   \item{\code{setModuleStartTime()}}{Set the current system time as the time
#'   when the execution of the associated module was initiated.}
#'   \item{\code{getModuleStartTime()}}{Get the time when the execution of the
#'   associated module was initiated.} 
#'   \item{\code{setModuleEndTime()}}{Set the
#'   current system time as the time when the execution of the associated module
#'   was completed} 
#'   \item{\code{getModuleEndTime()}}{Get the time when the
#'   execution of the associated module was completed.}
#'   \item{\code{setExecutionStatus(arg)}}{Set the status of execution - whether
#'   it was successfully completed or not. \code{arg} indicates a logical vector
#'   of length 1.} 
#'   \item{\code{getExecutionStatus()}}{Get the status of
#'   execution - whether it was successfully completed or not.}
#'   \item{\code{getExecutionTimings()}}{ 
#'               \itemize{ 
#'               \item{The list of timings indicating the module name,
#'                start-time and end-time.} 
#'                \item{\bold{Returns}:
#'                \code{A \code{list} with: \code{name}, \code{startTime} & \code{endTime} of
#'                the associated module was initiated.}} } }
#'   }
#'
#' @section Private: \describe{ \item{\code{moduleStartTime}}{The time when the
#'   execution of the associated module was initiated.}
#'   \item{\code{moduleEndTime}}{The time when the execution of the associated
#'   module was completed} \item{\code{successfulExecution}}{A logical vector of
#'   length 1 that indicates whether the module was successfully executed. It
#'   could take the following values:}{ \itemize{ \item{\code{NULL} Module
#'   Execution incomplete.} \item{\code{FALSE} - Execution Failed.}
#'   \item{\code{TRUE} - Successful execution.} } } }
#' @seealso is.Module
#' @export
ModuleInterface <- R6::R6Class("ModuleInterface"
    , inherit = UpDraftComponentInterface
    , public = list(
        clearOutputCache = function() {
            UpDraftSettings$errorLogger("clearOutputCache not implemented in ", class(self)[1])
        }
        
        , getFuncObj = function() {
            UpDraftSettings$errorLogger('getFuncObj not implemented in ', class(self)[1])    
        }
        
        , getInputs = function() {
            UpDraftSettings$errorLogger("getInputs not implemented in ", class(self)[1])    
        }
        
        , getOutput = function() {
            UpDraftSettings$errorLogger("getOutput not implemented in ", class(self)[1])
        }
        
        , hasCompleted = function() {
            UpDraftSettings$errorLogger("hasCompleted not implemented in ", class(self)[1])
        }
        
        , setName = function(arg) {
            if(length(arg) == 1 & is.character(arg))
            private$name <- arg
            return(invisible(NULL))
        }
        
        , getName = function() {
            return(private$name)
        }
        
        , startExecution = function(argsList) {
            UpDraftSettings$errorLogger("startExecution not implemented in ", class(self)[1])
        }
        
        , setModuleStartTime = function() {
            if(is.null(private$moduleStartTime)) {
                private$moduleStartTime <- as.POSIXct(Sys.time())
            }
            return(invisible(NULL))
        }
        
        , setModuleEndTime = function() {
            if(is.null(private$moduleEndTime)) {
                private$moduleEndTime <- as.POSIXct(Sys.time())
            }
            return(invisible(NULL))
        }
        
        , getModuleStartTime = function() {
            return(private$moduleStartTime)
        }
        
        , getModuleEndTime = function() {
            return(private$moduleEndTime)
        }
        
        , setExecutionStatus = function(arg) {
            if(length(arg) == 1 && is.logical(arg)) {
                private$successfulExecution <- arg
            }
            return(invisible(NULL))
        }
        
        , getExecutionStatus = function() {
            return(private$successfulExecution)
        }
        
        , getExecutionTimings = function() {
            return(
                list(
                    objectType = 'module'
                    , name = private$name
                    , startTime = private$moduleStartTime
                    , endTime = private$moduleEndTime
                )
            )
        }
    )
    
    , private = list(
        moduleStartTime = NULL
        , moduleEndTime = NULL
        , successfulExecution = NULL
        , name = ""
    )
)

#' @title is.Module
#' @name is.Module
#' @description Returns a boolean that indicates if \code{module} is a 
#'              valid module object. A valid module object is one that implements
#'              the interface defined by \code{\link{ModuleInterface}}.
#' @param module An object to check
#' @return boolean where \code{TRUE} indicates \code{module} is a valid 
#'         module object
#' @importFrom R6 is.R6
#' @export
is.Module <- function(module) {
    if("ModuleInterface" %in% class(module) && R6::is.R6(module)) {
        return(TRUE)
    }
    return(FALSE)
}
