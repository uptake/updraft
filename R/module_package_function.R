#' @title Package Function Module
#' @name PackageFunctionModule
#' @description Implementation of \code{ModuleInterface} for executing a function in a R package. Uses
#'              \code{future} package for execution.
#' @importFrom future resolved
#' @importFrom R6 R6Class
#' @importFrom utils getFromNamespace
#' @section Class Constructor:
#' \describe{
#'     \item{\code{new(name, fun, package = "base", assignedProcesses = 1)}}{
#'         \itemize{
#'             \item{\bold{\code{name}}: Unique character string that identifies this module instance.}
#'             \item{\bold{\code{fun}}: function this module will execute. Must be a character string.}
#'             \item{\code{package}: Name of the R package \code{fun}. Must be a character string.}
#'             \item{\code{assignedProcesses}: Integer number of processess to assign to the execution of this module.}
#'         }
#'     }
#' }
#' @section Public:
#' \describe{
#'     \item{\code{clearOutputCache()}}{
#'         \itemize{
#'             \item{Clears output cache stored in this instance so memory can be reclaimed.}
#'             \item{\bold{Returns}: \code{NULL}.}
#'         }
#'     }
#'     
#'     \item{\code{errorCheck(executionCheck = FALSE, ...)}}{
#'         \itemize{
#'             \item{Runs error checking on the internal state of this object for erroneous values.}
#'             \item{\code{executionCheck}: when set to \code{TRUE}, runs additional checks to determine if ready for execution.}
#'             \item{\code{...}: not used by this component.}
#'             \item{\bold{Returns}: \code{NULL}, will raise a fatal error if an error is found.}
#'         }
#'     }
#'     
#'     \item{\code{getFuncObj()}}{
#'         \itemize{
#'             \item{Gets the base R function object that is called when a module's execution is started.}
#'             \item{\bold{Returns}: R function object.}
#'         }
#'     }
#'     
#'     \item{\code{getInputs()}}{
#'         \itemize{
#'             \item{Gets a named logical vector where the names are the possible inputs into a module and the values indicate if an input is required.}
#'             \item{\bold{Returns}: logical vector.}
#'         }
#'     }
#'     
#'     \item{\code{getOutput()}}{
#'         \itemize{
#'             \item{Gets the output of the function executed by this module.}
#'             \item{\bold{Returns}: The output of the function if the execution has completed. \code{NULL} will be returned if the execution has not been started or is still in progress.}
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
#'     \item{\code{hasCompleted()}}{
#'         \itemize{
#'             \item{Indicates if the execution of the associated function of this module is complete.}
#'             \item{\bold{Returns}: a boolean where \code{TRUE} indicates the execution is complete and \code{FALSE} indicates the execution is not complete or not started.}
#'         }
#'     }
#'     
#'     \item{\code{startExecution(args)}}{
#'         \itemize{
#'             \item{Starts the execution of the function associated with this module. This is non-blocking if using an asynchronous.}
#'             \item{\bold{\code{args}}: named list of arguments for function associated with this module}
#'             \item{\bold{Returns}: \code{NULL}}
#'         }
#'     }
#'     
#' }
#' @section Private:
#' \describe{
#'     \item{\code{assignedProcesses}}{
#'         \itemize{
#'             \item{Number of processes assigned to execute this module.}
#'         }
#'     }
#'     
#'     \item{\code{fun}}{
#'         \itemize{
#'             \item{Stores associated function name.}
#'         }
#'     }
#'     
#'     \item{\code{futurePromise}}{
#'         \itemize{
#'             \item{Stores promise of future output of a function.}
#'         }
#'     }
#'     
#'     \item{\code{package}}{
#'         \itemize{
#'             \item{Stores associated R package name.}
#'         }
#'     }
#' }
#' @section Static Class Methods:
#' \describe{
#'     \item{\link[updraft]{PackageFunctionModule-cash-initFromSaveData}}{}
#' }  
#' @export
PackageFunctionModule <- R6::R6Class("PackageFunctionModule"
    , inherit = ModuleInterface
    
    , public = list(
        initialize = function(name
                              , fun
                              , package = 'base'
                              , assignedProcesses = 1
        ) {
            super$setName(name)
            private$fun <- fun
            private$package <- package
            if(!is.null(assignedProcesses)) {
                private$assignedProcesses <- assignedProcesses   
            }
            
            self$errorCheck()
        }
        
        , clearOutputCache = function() {
            # TODO: this rm is causing issues
            # if (!is.null(private$futurePromise)) {
            #     rm(private$futurePromise)
            # }

            private$futurePromise <- NULL

            return(NULL)
        }
        
        , errorCheck = function(executionCheck = FALSE
                                , ...
        ) {
            if (!is.character(private$fun) || !is.character(private$package) || !is.character(super$getName())) {
                UpDraftSettings$errorLogger("fun, package, name values must be of type character")
            }
            
            if (private$fun == '') {
                UpDraftSettings$errorLogger("fun value cannot be an empty string")
            }
            
            if (super$getName() == '') {
                UpDraftSettings$errorLogger("name value cannot be an empty string")
            }
    
            if(!is.numeric(private$assignedProcesses) || length(private$assignedProcesses) > 1) {
                UpDraftSettings$errorLogger("assignedProcesses must be a single whole number numeric")    
            }
            
            return(NULL)
        }
        
        , getFuncObj = function() {
            return(utils::getFromNamespace(private$fun
                                           , private$package))            
        }

        , getInputs = function() {
            return(GetFunctionArgs(utils::getFromNamespace(private$fun
                                                           , private$package)))    
        }
        
        , getOutput = function() {
            if (self$hasCompleted()) {
                out <- tryCatch(expr = {future::value(private$futurePromise)}
                                , error = function(e) {return(FALSE)})
                if(is.logical(out) && !out) {
                    super$setExecutionStatus(FALSE)
                } else {
                    super$setExecutionStatus(TRUE)
                }
                return(out)
            }
            return(NULL)
        }
        
        , getSaveInfo = function() {
            internalState = list()

            internalState[["class"]] <- class(self)[1]
            internalState[['name']] <- super$getName()
            internalState[['fun']] <- private$fun
            internalState[['package']] <- private$package
            internalState[['assignedProcesses']] <- private$assignedProcesses
            
            return(internalState)
        }

        , hasCompleted = function() {
            if (is.null(private$futurePromise) || !future::resolved(private$futurePromise)) {
                return(FALSE)
            }
            
            ## Capturing the end time of the module execution.
            ## This will be off by a couple of secs as this isn't an explicite wrapper around the future call.
            ## We do have a execution time logging statement in the function FutureFunctionCall.
            super$setModuleEndTime()
            return(TRUE)
        }
        
        , startExecution = function(args) {
            super$setModuleStartTime()
            if (private$package == "") {
                private$futurePromise <- FutureFunctionCall(func = private$fun
                                                            , args = args
                                                            , funcName = self$getName()
                                                            , assignedProcesses = private$assignedProcesses)
            } else {
                private$futurePromise <- FutureFunctionCall(func = utils::getFromNamespace(private$fun, private$package)
                                                            , args = args
                                                            , funcName = self$getName()
                                                            , assignedProcesses = private$assignedProcesses)
            }

            return(NULL)
        }
        
    )
    
    , private = list(
        fun = ""
        , futurePromise = NULL
        , package = ""
        , assignedProcesses = 1
    )
)

#' @title PackageFunctionModule Init From Save Data
#' @name PackageFunctionModule$initFromSaveData
#' @description Static factory method that generates a PackageFunctionModule
#'              from json save data.
#' @param saveData json saved data generated through the method \code{getSaveInfo}.
#' @return initialized \code{PackageFunctionModule}
PackageFunctionModule$initFromSaveData <- function(saveData) {
    module <- PackageFunctionModule$new(name = saveData$name
                                      , fun = saveData$fun
                                      , package = saveData$package
                                      , assignedProcesses = saveData$assignedProcesses)
    return(module)
}
