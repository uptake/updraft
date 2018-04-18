#' @title Module Interface
#' @name ModuleInterface
#' @importFrom R6 R6Class
#' @description Defines a contract of implementation for any module based class, i.e. a class that extends this
#'              interface. This class cannot be instantiated directly as it is abstract. 
#' @section Contracted Methods:
#' \describe{
#'      \item{\code{clearOutputCache()}}{Clears out the cached output of the associated module.}
#'      \item{\code{getFuncObj()}}{Gets the base R function object that is called when the associated module's execution is started.}
#'      \item{\code{getInput()}}{Gets a named logical vector where the names are the possible inputs into a module and the values indicate if an input is required.}
#'      \item{\code{getOutput()}}{Gets the output of the associated module.}
#'      \item{\code{hasCompleted()}}{Indicates if the associated module's execution has completed.}
#'      \item{\code{startExecution(argsList)}}{Starts the execution of the associated module with \code{argsList} as arguments.}
#' }
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
        
        , startExecution = function(argsList) {
            UpDraftSettings$errorLogger("startExecution not implemented in ", class(self)[1])
        }
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
