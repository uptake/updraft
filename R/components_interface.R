#' @title UpDraft Component Interface
#' @name UpDraftComponentInterface
#' @description Defines the interface for all component objects in updraft. Component objects
#'              are any object associated with a workflow. 
#' @importFrom R6 R6Class
#' @section Contracted Methods:
#' \describe{
#'    \item{\code{errorCheck(executionCheck = FALSE, ...)}}{Determines if internal state contains erroneous values. When \code{executionCheck} is set to TRUE, runs additional checks to check if ready for execution. \code{...} is for additional info that is required to run error checking for a particular component.}
#'    \item{\code{getName()}}{Returns the string character that identifies this component.}
#'    \item{\code{getSaveInfo()}}{Returns a list of attributes that is a copy of the internal state of this component.}
#' }
#' @export
UpDraftComponentInterface <- R6::R6Class("UpDraftComponentInterface"
    , public = list(
        initialize = function() {
            UpDraftSettings$errorLogger("Cannot instantiate ", class(self)[1], " as it is intended to be an interface, i.e. contract of what needs to be implemented of implementation classes of this interface")
        }
        
        , errorCheck =  function(executionCheck = FALSE
                                 , ...
        ) {
            UpDraftSettings$errorLogger("errorCheck not implemented in ", class(self)[1])
        }
        
        , getName = function() {
            UpDraftSettings$errorLogger("getName not implemented in ", class(self)[1])    
        }
        
        , getSaveInfo = function() {
            UpDraftSettings$errorLogger("getSaveInfo not implemented in ", class(self)[1])
        }
    )
)
